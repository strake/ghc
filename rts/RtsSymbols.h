/* -----------------------------------------------------------------------------
 *
 * (c) The GHC Team, 2000-2015
 *
 * RTS Symbols
 *
 * ---------------------------------------------------------------------------*/

#pragma once

#include "ghcautoconf.h"
#include "LinkerInternals.h"
#include <stdbool.h>

typedef struct _RtsSymbolVal {
    const SymbolName* lbl;
    SymbolAddr* addr;
    bool weak;
} RtsSymbolVal;

extern RtsSymbolVal rtsSyms[];

/* See Note [_iob_func symbol].  */
#if defined(mingw32_HOST_OS)
extern const void* __rts_iob_func;
#endif
