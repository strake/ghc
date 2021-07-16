# Args to vtc are: extra compile flags

test('T3103', [reqlib("ghc-bignum"), extra_files(['Foreign/', 'GHC/'])], multimod_compile,
     ['Foreign.Ptr',
      '-v0 -hide-all-packages -package ghc-bignum -package ghc-prim -this-unit-id base'])
