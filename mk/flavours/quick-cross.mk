SRC_HC_OPTS        = -O0 -H64m
GhcStage1HcOpts    = -O
GhcStage2HcOpts    = -O0 -fllvm
GhcLibHcOpts       = -O -fllvm
BUILD_PROF_LIBS    = NO
SplitSections      = NO
HADDOCK_DOCS       = NO
BUILD_SPHINX_HTML  = NO
BUILD_SPHINX_PDF   = NO
BUILD_MAN          = NO
WITH_TERMINFO      = NO

INTEGER_LIBRARY      = integer-simple
Stage1Only           = YES
DYNAMIC_GHC_PROGRAMS = NO
