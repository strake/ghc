/* -----------------------------------------------------------------------------
*
* (c) The GHC Team 1998-2000
*
* Error Handling implementations for windows
*
* ---------------------------------------------------------------------------*/

#include "Rts.h"
#include "ghcconfig.h"
#include "veh_excn.h"
#include <assert.h>

/////////////////////////////////
// Exception / signal handlers.
/////////////////////////////////

/*
  SEH (Structured Error Handler) on Windows is quite tricky. On x86 SEHs are
  stack based and are stored in FS[0] of each thread. Which means every time we
  spawn an OS thread we'd have to set up the error handling. However on x64 it's
  table based and memory region based. e.g. you register a handler for a
  particular memory range. This means that we'd have to register handlers for
  each block of code we load externally or generate internally ourselves.

  In Windows XP VEH (Vectored Exception Handler) and VCH (Vectored Continue
  Handler) were added. Both of these are global/process wide handlers, the
  former handling all exceptions and the latter handling only exceptions which
  we're trying to recover from, e.g. a handler returned
  EXCEPTION_CONTINUE_EXECUTION.

  And lastly you have top level exception filters, which are also process global
  but the problem here is that you can only have one, and setting this removes
  the previous ones. The chain of exception handling looks like

                    [  Vectored Exception Handler  ]
                                |
                    [ Structured Exception Handler ]
                                |
                    [      Exception Filters       ]
                                |
                    [  Vectored Continue Handler   ]

  To make things more tricky, the exception handlers handle both hardware and
  software exceptions Which means previously when we registered VEH handlers
  we would also trap software exceptions. Which means when haskell code was
  loaded in a C++ or C# context we would swallow exceptions and terminate in
  contexes that normally the runtime should be able to continue on, e.g. you
  could be handling the segfault in your C++ code, or the div by 0.

  We could not handle these exceptions, but GHCi would just die a horrible death
  then on normal Haskell only code when such an exception occurs.

  So instead, we'll move to Continue handler, to run as late as possible, and
  also register a filter which calls any existing filter, and then runs the
  continue handlers, we then also only run as the last continue handler so we
  don't supercede any other VCH handlers.

  Lastly we'll also provide a way for users to disable the exception handling
  entirely so even if the new approach doesn't solve the issue they can work
  around it. After all, I don't expect any interpreted code if you are running
  a haskell dll.

  For a detailed analysis see
  https://reverseengineering.stackexchange.com/questions/14992/what-are-the-vectored-continue-handlers
  and https://www.gamekiller.net/threads/vectored-exception-handler.3237343/
  */

// Define some values for the ordering of VEH Handlers:
// - CALL_FIRST means call this exception handler first
// - CALL_LAST means call this exception handler last
#define CALL_FIRST 1
#define CALL_LAST 0

// this should be in <excpt.h>, but it's been removed from MinGW distributions
#if !defined(EH_UNWINDING)
#define EH_UNWINDING   0x02
#endif /* EH_UNWINDING */

// Registered exception handler
PVOID __hs_handle = NULL;
LPTOP_LEVEL_EXCEPTION_FILTER oldTopFilter = NULL;

long WINAPI __hs_exception_handler(struct _EXCEPTION_POINTERS *exception_data)
{
    long action = EXCEPTION_CONTINUE_SEARCH;
    ULONG_PTR what;

    // When the system unwinds the VEH stack after having handled an excn,
    // return immediately.
    if ((exception_data->ExceptionRecord->ExceptionFlags & EH_UNWINDING) == 0)
    {
        // Error handling cases covered by this implementation.
        switch (exception_data->ExceptionRecord->ExceptionCode) {
            case EXCEPTION_FLT_DIVIDE_BY_ZERO:
            case EXCEPTION_INT_DIVIDE_BY_ZERO:
                fprintf(stdout, "divide by zero\n");
                action = EXCEPTION_CONTINUE_EXECUTION;
                break;
            case EXCEPTION_STACK_OVERFLOW:
                fprintf(stdout, "C stack overflow in generated code\n");
                action = EXCEPTION_CONTINUE_EXECUTION;
                break;
            case EXCEPTION_ACCESS_VIOLATION:
                what = exception_data->ExceptionRecord->ExceptionInformation[0];
                fprintf(stdout, "Access violation in generated code"
                                " when %s %p\n"
                                , what == 0 ? "reading" : what == 1 ? "writing" : what == 8 ? "executing data at" : "?"
                                , (void*) exception_data->ExceptionRecord->ExceptionInformation[1]
                                );
                action = EXCEPTION_CONTINUE_EXECUTION;
                break;
            default:;
        }

        // If an error has occurred and we've decided to continue execution
        // then we've done so to prevent something else from handling the error.
        // But the correct action is still to exit as fast as possible.
        if (EXCEPTION_CONTINUE_EXECUTION == action)
        {
            fflush(stdout);
            stg_exit(EXIT_FAILURE);
        }
    }

    return action;
}

long WINAPI __hs_exception_filter(struct _EXCEPTION_POINTERS *exception_data)
{
    long result = EXCEPTION_CONTINUE_EXECUTION;
    if (oldTopFilter)
    {
        result = (*oldTopFilter)(exception_data);
        if (EXCEPTION_CONTINUE_SEARCH == result)
            result = EXCEPTION_CONTINUE_EXECUTION;
        return result;
    }

    return result;
}

void __register_hs_exception_handler( void )
{
    if (!RtsFlags.MiscFlags.install_seh_handlers)
        return;

    // Allow the VCH handler to be registered only once.
    if (NULL == __hs_handle)
    {
        // Be the last one to run, We can then be sure we didn't interfere with
        // anything else.
        __hs_handle = AddVectoredContinueHandler(CALL_LAST,
                                                 __hs_exception_handler);
        // should the handler not be registered this will return a null.
        assert(__hs_handle);

        // Register for an exception filter to ensure the continue handler gets
        // hit if no one handled the exception.
        oldTopFilter = SetUnhandledExceptionFilter (__hs_exception_filter);
    }
    else
    {
        errorBelch("There is no need to call __register_hs_exception_handler()"
                   " twice, VEH handlers are global per process.");
    }
}

void __unregister_hs_exception_handler( void )
{
    if (!RtsFlags.MiscFlags.install_seh_handlers)
        return;

    if (__hs_handle != NULL)
    {
        // Should the return value be checked? we're terminating anyway.
        RemoveVectoredContinueHandler(__hs_handle);
        __hs_handle = NULL;
    }
    else
    {
        errorBelch("__unregister_hs_exception_handler() called without having"
                   "called __register_hs_exception_handler() first.");
    }
}

