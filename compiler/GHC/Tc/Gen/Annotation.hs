{-
(c) The University of Glasgow 2006
(c) The AQUA Project, Glasgow University, 1993-1998

-}

{-# LANGUAGE TypeFamilies #-}

-- | Typechecking annotations
module GHC.Tc.Gen.Annotation ( tcAnnotations, annCtxt ) where

import GHC.Prelude

import GHC.Driver.Session
import GHC.Driver.Env

import {-# SOURCE #-} GHC.Tc.Gen.Splice ( runAnnotation )
import GHC.Tc.Utils.Monad

import GHC.Unit.Module

import GHC.Hs

import GHC.Utils.Outputable

import GHC.Types.Name
import GHC.Types.Annotations
import GHC.Types.SrcLoc

import Control.Monad ( when )

-- Some platforms don't support the interpreter, and compilation on those
-- platforms shouldn't fail just due to annotations
tcAnnotations :: [LAnnDecl GhcRn] -> TcM [Annotation]
tcAnnotations anns = hsc_interp <$> getTopEnv >>= \ case
    Just _  -> traverse tcAnnotation anns
    Nothing -> [] <$ warnAnns anns

warnAnns :: [LAnnDecl GhcRn] -> TcM ()
--- No GHCI; emit a warning (not an error) and ignore. cf #4268
warnAnns [] = pure ()
warnAnns anns@(L loc _ : _) = setSrcSpan loc $ addWarnTc NoReason $
             (text "Ignoring ANN annotation" <> plural anns <> comma
             <+> text "because this is a stage-1 compiler without -fexternal-interpreter or doesn't support GHCi")

tcAnnotation :: LAnnDecl GhcRn -> TcM Annotation
tcAnnotation (L loc ann@(HsAnnotation _ _ provenance expr)) = do
    -- Work out what the full target of this annotation was
    mod <- getModule
    let target = annProvenanceToTarget mod provenance

    -- Run that annotation and construct the full Annotation data structure
    setSrcSpan loc $ addErrCtxt (annCtxt ann) $ do
      -- See #10826 -- Annotations allow one to bypass Safe Haskell.
      dflags <- getDynFlags
      when (safeLanguageOn dflags) $ failWithTc safeHsErr
      runAnnotation target expr
    where
      safeHsErr = vcat [ text "Annotations are not compatible with Safe Haskell."
                  , text "See https://gitlab.haskell.org/ghc/ghc/issues/10826" ]

annProvenanceToTarget :: Module -> AnnProvenance Name
                      -> AnnTarget Name
annProvenanceToTarget _   (ValueAnnProvenance (L _ name)) = NamedTarget name
annProvenanceToTarget _   (TypeAnnProvenance (L _ name))  = NamedTarget name
annProvenanceToTarget mod ModuleAnnProvenance             = ModuleTarget mod

annCtxt :: (OutputableBndrId p) => AnnDecl (GhcPass p) -> SDoc
annCtxt ann
  = hang (text "In the annotation:") 2 (ppr ann)
