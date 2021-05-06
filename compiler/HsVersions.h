#pragma once

-- For GHC_STAGE
#include "ghcplatform.h"

#if 0

IMPORTANT!  If you put extra tabs/spaces in these macro definitions,
you will screw up the layout where they are used in case expressions!

(This is cpp-dependent, of course)

#endif

#define GLOBAL_VAR(name,value,ty)  \
{-# NOINLINE name #-};             \
name :: IORef (ty);                \
name = GHC.Utils.GlobalVars.global (value);

#define GLOBAL_VAR_M(name,value,ty) \
{-# NOINLINE name #-};              \
name :: IORef (ty);                 \
name = GHC.Utils.GlobalVars.globalM (value);


#define SHARED_GLOBAL_VAR(name,accessor,saccessor,value,ty) \
{-# NOINLINE name #-};                                      \
name :: IORef (ty);                                         \
name = GHC.Utils.GlobalVars.sharedGlobal (value) (accessor);\
foreign import ccall unsafe saccessor                       \
  accessor :: Ptr (IORef a) -> IO (Ptr (IORef a));

#define SHARED_GLOBAL_VAR_M(name,accessor,saccessor,value,ty)  \
{-# NOINLINE name #-};                                         \
name :: IORef (ty);                                            \
name = GHC.Utils.GlobalVars.sharedGlobalM (value) (accessor);  \
foreign import ccall unsafe saccessor                          \
  accessor :: Ptr (IORef a) -> IO (Ptr (IORef a));


#define ASSERT(e)      if debugIsOn && not (e) then (assertPanic __FILE__ __LINE__) else
#define ASSERT2(e,msg) if debugIsOn && not (e) then (assertPprPanic (msg)) else
#define WARN( e, msg ) (warnPprTrace (e) (msg)) $

-- Examples:   Assuming   flagSet :: String -> m Bool
--
--    do { c   <- getChar; MASSERT( isUpper c ); ... }
--    do { c   <- getChar; MASSERT2( isUpper c, text "Bad" ); ... }
--    do { str <- getStr;  ASSERTM( flagSet str ); .. }
--    do { str <- getStr;  ASSERTM2( flagSet str, text "Bad" ); .. }
#define MASSERT(e)      ASSERT(e) return ()
#define MASSERT2(e,msg) ASSERT2(e,msg) return ()
#define ASSERTM(e)      do { bool <- e; MASSERT(bool) }
#define ASSERTM2(e,msg) do { bool <- e; MASSERT2(bool,msg) }
