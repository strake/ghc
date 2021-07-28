-----------------------------------------------------------------------------
-- A Parser monad with access to the 'DynFlags'.
--
-- The 'P' monad only has access to the subset of 'DynFlags'
-- required for parsing Haskell.

-- The parser for C-- requires access to a lot more of the 'DynFlags',
-- so 'PD' provides access to 'DynFlags' via a 'HasDynFlags' instance.
-----------------------------------------------------------------------------
module GHC.Cmm.Parser.Monad (
    PD(..)
  , liftP
  , failMsgPD
  , getProfile
  , getPlatform
  , getPtrOpts
  , getHomeUnit
  , withHomeUnit
  ) where

import GHC.Prelude

import GHC.Platform
import GHC.Platform.Profile
import GHC.Cmm.Info

import Control.Monad

import GHC.Driver.Session
import GHC.Parser.Lexer
import GHC.Unit.Types

newtype PD a = PD { unPD :: DynFlags -> PState -> ParseResult a }
  deriving stock (Functor)

instance Applicative PD where
  pure = returnPD
  (<*>) = ap

instance Monad PD where
  (>>=) = thenPD

liftP :: P a -> PD a
liftP (P f) = PD $ \_ s -> f s

failMsgPD :: String -> PD a
failMsgPD = liftP . failMsgP

returnPD :: a -> PD a
returnPD = liftP . return

thenPD :: PD a -> (a -> PD b) -> PD b
(PD m) `thenPD` k = PD $ \d s ->
        case m d s of
                POk s1 a         -> unPD (k a) d s1
                PFailed s1 -> PFailed s1

instance HasDynFlags PD where
   getDynFlags = PD $ \d s -> POk s d

getProfile :: PD Profile
getProfile = targetProfile <$> getDynFlags

getPlatform :: PD Platform
getPlatform = profilePlatform <$> getProfile

getPtrOpts :: PD PtrOpts
getPtrOpts = do
   dflags <- getDynFlags
   profile <- getProfile
   pure $ PtrOpts
      { po_profile     = profile
      , po_align_check = gopt Opt_AlignmentSanitisation dflags
      }

-- | Return the UnitId of the home-unit. This is used to create labels.
getHomeUnit :: PD Unit
getHomeUnit = homeUnit <$> getDynFlags

withHomeUnit :: (Unit -> a) -> PD a
withHomeUnit = (<$> getHomeUnit)
