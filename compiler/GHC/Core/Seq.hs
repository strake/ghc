-- |
-- Various utilities for forcing Core structures
--
-- It can often be useful to force various parts of the AST. This module
-- provides a number of @seq@-like functions to accomplish this.

module GHC.Core.Seq (
        -- * Utilities for forcing Core structures
        seqExpr, seqUnfolding, seqRule,
        megaSeqIdInfo, seqRuleInfo, seqBind,
    ) where

import GHC.Prelude

import GHC.Core
import GHC.Types.Id.Info
import GHC.Types.Demand( seqDemand, seqStrictSig )
import GHC.Types.Cpr( seqCprSig )
import GHC.Types.Basic( seqOccInfo )
import GHC.Types.Var.Set( seqDVarSet )
import GHC.Types.Var( varType, tyVarKind )
import GHC.Core.Type( seqType, isTyVar )
import GHC.Core.Coercion( seqCo )
import GHC.Types.Id( Id, idInfo )

import Control.DeepSeq( rwhnf )

-- | Evaluate all the fields of the 'IdInfo' that are generally demanded by the
-- compiler
megaSeqIdInfo :: IdInfo -> ()
megaSeqIdInfo info
  = seqRuleInfo (ruleInfo info)                 `seq`

-- Omitting this improves runtimes a little, presumably because
-- some unfoldings are not calculated at all
--    seqUnfolding (unfoldingInfo info)         `seq`

    seqDemand (demandInfo info)                 `seq`
    seqStrictSig (strictnessInfo info)          `seq`
    seqCprSig (cprInfo info)                    `seq`
    rwhnf (cafInfo info)                        `seq`
    rwhnf (oneShotInfo info)                    `seq`
    seqOccInfo (occInfo info)

seqRuleInfo :: RuleInfo -> ()
seqRuleInfo (RuleInfo rules fvs) = foldMap' seqRule rules `seq` seqDVarSet fvs

seqRule :: CoreRule -> ()
seqRule Rule { ru_bndrs = bndrs, ru_args = args, ru_rhs = rhs }
  = foldMap' seqBndr bndrs `seq` foldMap' seqExpr (rhs:args) `seq` ()
seqRule BuiltinRule {} = ()

seqExpr :: CoreExpr -> ()
seqExpr (Var v)         = v `seq` ()
seqExpr (Lit lit)       = lit `seq` ()
seqExpr (App f a)       = seqExpr f `seq` seqExpr a
seqExpr (Lam b e)       = seqBndr b `seq` seqExpr e
seqExpr (Let b e)       = seqBind b `seq` seqExpr e
seqExpr (Case e b t as) = seqExpr e `seq` seqBndr b `seq` seqType t `seq` foldMap' seqAlt as
seqExpr (Cast e co)     = seqExpr e `seq` seqCo co
seqExpr (Tick n e)      = seqTickish n `seq` seqExpr e
seqExpr (Type t)        = seqType t
seqExpr (Coercion co)   = seqCo co

seqTickish :: Tickish Id -> ()
seqTickish ProfNote{ profNoteCC = cc } = cc `seq` ()
seqTickish HpcTick{} = ()
seqTickish Breakpoint{ breakpointFVs = ids } = foldMap' seqBndr ids
seqTickish SourceNote{} = ()

seqBndr :: CoreBndr -> ()
seqBndr b | isTyVar b = seqType (tyVarKind b)
          | otherwise = seqType (varType b)             `seq`
                        megaSeqIdInfo (idInfo b)

seqBind :: Bind CoreBndr -> ()
seqBind (NonRec b e) = seqBndr b `seq` seqExpr e
seqBind (Rec prs)    = seqPairs prs

seqPairs :: [(CoreBndr, CoreExpr)] -> ()
seqPairs [] = ()
seqPairs ((b,e):prs) = seqBndr b `seq` seqExpr e `seq` seqPairs prs

seqAlt :: CoreAlt -> ()
seqAlt (c, bs, e) = c `seq` foldMap' seqBndr bs `seq` seqExpr e `seq` ()

seqUnfolding :: Unfolding -> ()
seqUnfolding (CoreUnfolding { uf_tmpl = e, uf_is_top = top,
                uf_is_value = b1, uf_is_work_free = b2,
                uf_expandable = b3, uf_is_conlike = b4,
                uf_guidance = g})
  = seqExpr e `seq` top `seq` b1 `seq` b2 `seq` b3 `seq` b4 `seq` seqGuidance g

seqUnfolding _ = ()

seqGuidance :: UnfoldingGuidance -> ()
seqGuidance (UnfIfGoodArgs ns n b) = n `seq` sum ns `seq` b `seq` ()
seqGuidance _                      = ()
