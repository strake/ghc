/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2006-2009
 *
 * The RTS stores some "global" values on behalf of libraries, so that
 * some libraries can ensure that certain top-level things are shared
 * even when multiple versions of the library are loaded.  e.g. see
 * Data.Typeable and GHC.Conc.
 *
 * Do not #include this file directly: #include "Rts.h" instead.
 *
 * To understand the structure of the RTS headers, see the wiki:
 *   https://gitlab.haskell.org/ghc/ghc/wikis/commentary/source-tree/includes
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#define mkStoreAccessorPrototype(name)                                  \
    StgStablePtr                                                        \
    getOrSet##name(StgStablePtr ptr);

mkStoreAccessorPrototype(GHCConcSignalSignalHandlerStore)
mkStoreAccessorPrototype(GHCConcWindowsPendingDelaysStore)
mkStoreAccessorPrototype(GHCConcWindowsIOManagerThreadStore)
mkStoreAccessorPrototype(GHCConcWindowsProddingStore)
mkStoreAccessorPrototype(SystemEventThreadEventManagerStore)
mkStoreAccessorPrototype(SystemEventThreadIOManagerThreadStore)
mkStoreAccessorPrototype(SystemTimerThreadEventManagerStore)
mkStoreAccessorPrototype(SystemTimerThreadIOManagerThreadStore)
mkStoreAccessorPrototype(LibHSghcFastStringTable)
mkStoreAccessorPrototype(LibHSghcPersistentLinkerState)
mkStoreAccessorPrototype(LibHSghcInitLinkerDone)
mkStoreAccessorPrototype(LibHSghcGlobalHasPprDebug)
mkStoreAccessorPrototype(LibHSghcGlobalHasNoDebugOutput)
mkStoreAccessorPrototype(LibHSghcGlobalHasNoStateHack)
mkStoreAccessorPrototype(LibHSghcStaticOptions)
mkStoreAccessorPrototype(LibHSghcStaticOptionsReady)
