{-
*********************************************************************************
*                                                                               *
*       John Hughes's and Simon Peyton Jones's Pretty Printer Combinators       *
*                                                                               *
*               based on "The Design of a Pretty-printing Library"              *
*               in Advanced Functional Programming,                             *
*               Johan Jeuring and Erik Meijer (eds), LNCS 925                   *
*               http://www.cs.chalmers.se/~rjmh/Papers/pretty.ps                *
*                                                                               *
*               Heavily modified by Simon Peyton Jones, Dec 96                  *
*                                                                               *
*********************************************************************************

Version 3.0     28 May 1997
  * Cured massive performance bug.  If you write

        foldl <> empty (map (text.show) [1..10000])

    you get quadratic behaviour with V2.0.  Why?  For just the same reason as you get
    quadratic behaviour with left-associated (++) chains.

    This is really bad news.  One thing a pretty-printer abstraction should
    certainly guarantee is insensivity to associativity.  It matters: suddenly
    GHC's compilation times went up by a factor of 100 when I switched to the
    new pretty printer.

    I fixed it with a bit of a hack (because I wanted to get GHC back on the
    road).  I added two new constructors to the Doc type, Above and Beside:

         <> = Beside
         $$ = Above

    Then, where I need to get to a "TextBeside" or "NilAbove" form I "force"
    the Doc to squeeze out these suspended calls to Beside and Above; but in so
    doing I re-associate. It's quite simple, but I'm not satisfied that I've done
    the best possible job.  I'll send you the code if you are interested.

  * Added new exports:
        punctuate, hang
        int, integer, float, double, rational,
        lparen, rparen, lbrack, rbrack, lbrace, rbrace,

  * fullRender's type signature has changed.  Rather than producing a string it
    now takes an extra couple of arguments that tells it how to glue fragments
    of output together:

        fullRender :: Mode
                   -> Int                       -- Line length
                   -> Float                     -- Ribbons per line
                   -> (TextDetails -> a -> a)   -- What to do with text
                   -> a                         -- What to do at the end
                   -> Doc
                   -> a                         -- Result

    The "fragments" are encapsulated in the TextDetails data type:
        data TextDetails = Chr  Char
                         | Str  String
                         | PStr FastString

    The Chr and Str constructors are obvious enough.  The PStr constructor has a packed
    string (FastString) inside it.  It's generated by using the new "ptext" export.

    An advantage of this new setup is that you can get the renderer to do output
    directly (by passing in a function of type (TextDetails -> IO () -> IO ()),
    rather than producing a string that you then print.


Version 2.0     24 April 1997
  * Made empty into a left unit for <> as well as a right unit;
    it is also now true that
        nest k empty = empty
    which wasn't true before.

  * Fixed an obscure bug in sep that occasionally gave very weird behaviour

  * Added $+$

  * Corrected and tidied up the laws and invariants

======================================================================
Relative to John's original paper, there are the following new features:

1.  There's an empty document, "empty".  It's a left and right unit for
    both <> and $$, and anywhere in the argument list for
    sep, hcat, hsep, vcat, fcat etc.

    It is Really Useful in practice.

2.  There is a paragraph-fill combinator, fsep, that's much like sep,
    only it keeps fitting things on one line until it can't fit any more.

3.  Some random useful extra combinators are provided.
        <+> puts its arguments beside each other with a space between them,
            unless either argument is empty in which case it returns the other


        hcat is a list version of <>
        hsep is a list version of <+>
        vcat is a list version of $$

        sep (separate) is either like hsep or like vcat, depending on what fits

        cat  is behaves like sep,  but it uses <> for horizontal conposition
        fcat is behaves like fsep, but it uses <> for horizontal conposition

        These new ones do the obvious things:
                char, semi, comma, colon, space,
                parens, brackets, braces,
                quotes, quote, doubleQuotes

4.      The "above" combinator, $$, now overlaps its two arguments if the
        last line of the top argument stops before the first line of the second begins.
        For example:  text "hi" $$ nest 5 "there"
        lays out as
                        hi   there
        rather than
                        hi
                             there

        There are two places this is really useful

        a) When making labelled blocks, like this:
                Left ->   code for left
                Right ->  code for right
                LongLongLongLabel ->
                          code for longlonglonglabel
           The block is on the same line as the label if the label is
           short, but on the next line otherwise.

        b) When laying out lists like this:
                [ first
                , second
                , third
                ]
           which some people like.  But if the list fits on one line
           you want [first, second, third].  You can't do this with
           John's original combinators, but it's quite easy with the
           new $$.

        The combinator $+$ gives the original "never-overlap" behaviour.

5.      Several different renderers are provided:
                * a standard one
                * one that uses cut-marks to avoid deeply-nested documents
                        simply piling up in the right-hand margin
                * one that ignores indentation (fewer chars output; good for machines)
                * one that ignores indentation and newlines (ditto, only more so)

6.      Numerous implementation tidy-ups
        Use of unboxed data types to speed up the implementation
-}


{-# LANGUAGE BangPatterns, CPP, MagicHash #-}

module Pretty (
        -- * The document type
        Doc, TextDetails(..),

        -- * Constructing documents

        -- ** Converting values into documents
        char, text, ftext, ptext, ztext, zeroWidthText,
        int, integer, float, double, rational,

        -- ** Simple derived documents
        semi, comma, colon, space, equals,
        lparen, rparen, lbrack, rbrack, lbrace, rbrace,

        -- ** Wrapping documents in delimiters
        parens, brackets, braces, quotes, quote, doubleQuotes,
        maybeParens,

        -- ** Combining documents
        empty,
        (<>), (<+>), hcat, hsep,
        ($$), ($+$), vcat,
        sep, cat,
        fsep, fcat,
        nest,
        hang, punctuate,

        -- * Predicates on documents
        isEmpty,

        -- * Rendering documents

        -- ** Rendering with a particular style
        Mode(..),

        -- ** General rendering
        fullRender,

        -- ** GHC-specific rendering
        printDoc, printDoc_, showDoc,
        bufLeftRender -- performance hack

  ) where

import BufWrite
import FastString
import FastTypes
import Panic
import Numeric (fromRat)
import System.IO
import Prelude hiding (error)

--for a RULES
import GHC.Base ( unpackCString# )
import GHC.Exts ( Int# )
import GHC.Ptr  ( Ptr(..) )

-- Don't import Util( assertPanic ) because it makes a loop in the module structure


-- ---------------------------------------------------------------------------
-- The Doc calculus

{-
Laws for $$
~~~~~~~~~~~
<a1>    (x $$ y) $$ z   = x $$ (y $$ z)
<a2>    empty $$ x      = x
<a3>    x $$ empty      = x

        ...ditto $+$...

Laws for <>
~~~~~~~~~~~
<b1>    (x <> y) <> z   = x <> (y <> z)
<b2>    empty <> x      = empty
<b3>    x <> empty      = x

        ...ditto <+>...

Laws for text
~~~~~~~~~~~~~
<t1>    text s <> text t        = text (s++t)
<t2>    text "" <> x            = x, if x non-empty

** because of law n6, t2 only holds if x doesn't
** start with `nest'.


Laws for nest
~~~~~~~~~~~~~
<n1>    nest 0 x                = x
<n2>    nest k (nest k' x)      = nest (k+k') x
<n3>    nest k (x <> y)         = nest k x <> nest k y
<n4>    nest k (x $$ y)         = nest k x $$ nest k y
<n5>    nest k empty            = empty
<n6>    x <> nest k y           = x <> y, if x non-empty

** Note the side condition on <n6>!  It is this that
** makes it OK for empty to be a left unit for <>.

Miscellaneous
~~~~~~~~~~~~~
<m1>    (text s <> x) $$ y = text s <> ((text "" <> x) $$
                                         nest (-length s) y)

<m2>    (x $$ y) <> z = x $$ (y <> z)
        if y non-empty


Laws for list versions
~~~~~~~~~~~~~~~~~~~~~~
<l1>    sep (ps++[empty]++qs)   = sep (ps ++ qs)
        ...ditto hsep, hcat, vcat, fill...

<l2>    nest k (sep ps) = sep (map (nest k) ps)
        ...ditto hsep, hcat, vcat, fill...

Laws for oneLiner
~~~~~~~~~~~~~~~~~
<o1>    oneLiner (nest k p) = nest k (oneLiner p)
<o2>    oneLiner (x <> y)   = oneLiner x <> oneLiner y

You might think that the following verion of <m1> would
be neater:

<3 NO>  (text s <> x) $$ y = text s <> ((empty <> x)) $$
                                         nest (-length s) y)

But it doesn't work, for if x=empty, we would have

        text s $$ y = text s <> (empty $$ nest (-length s) y)
                    = text s <> nest (-length s) y
-}

-- ---------------------------------------------------------------------------
-- Operator fixity

infixl 6 <>
infixl 6 <+>
infixl 5 $$, $+$


-- ---------------------------------------------------------------------------
-- The Doc data type

-- | The abstract type of documents.
-- A Doc represents a *set* of layouts. A Doc with
-- no occurrences of Union or NoDoc represents just one layout.
data Doc
  = Empty                                            -- empty
  | NilAbove Doc                                     -- text "" $$ x
  | TextBeside !TextDetails FastInt Doc              -- text s <> x
  | Nest FastInt Doc                                 -- nest k x
  | Union Doc Doc                                    -- ul `union` ur
  | NoDoc                                            -- The empty set of documents
  | Beside Doc Bool Doc                              -- True <=> space between
  | Above Doc Bool Doc                               -- True <=> never overlap

{-
Here are the invariants:

1) The argument of NilAbove is never Empty. Therefore
   a NilAbove occupies at least two lines.

2) The argument of @TextBeside@ is never @Nest@.

3) The layouts of the two arguments of @Union@ both flatten to the same
   string.

4) The arguments of @Union@ are either @TextBeside@, or @NilAbove@.

5) A @NoDoc@ may only appear on the first line of the left argument of an
   union. Therefore, the right argument of an union can never be equivalent
   to the empty set (@NoDoc@).

6) An empty document is always represented by @Empty@.  It can't be
   hidden inside a @Nest@, or a @Union@ of two @Empty@s.

7) The first line of every layout in the left argument of @Union@ is
   longer than the first line of any layout in the right argument.
   (1) ensures that the left argument has a first line.  In view of
   (3), this invariant means that the right argument must have at
   least two lines.

Notice the difference between
   * NoDoc (no documents)
   * Empty (one empty document; no height and no width)
   * text "" (a document containing the empty string;
              one line high, but has no width)
-}


-- | RDoc is a "reduced GDoc", guaranteed not to have a top-level Above or Beside.
type RDoc = Doc

-- | The TextDetails data type
--
-- A TextDetails represents a fragment of text that will be
-- output at some point.
data TextDetails = Chr  {-# UNPACK #-} !Char -- ^ A single Char fragment
                 | Str  String -- ^ A whole String fragment
                 | PStr FastString                      -- a hashed string
                 | ZStr FastZString                     -- a z-encoded string
                 | LStr {-#UNPACK#-}!LitString FastInt  -- a '\0'-terminated
                                                        -- array of bytes

instance Show Doc where
  showsPrec _ doc cont = showDocPlus PageMode 100 doc cont

showDoc :: Mode -> Int -> Doc -> String
showDoc mode cols doc = showDocPlus mode cols doc ""

showDocPlus :: Mode -> Int -> Doc -> String -> String
showDocPlus mode cols doc rest = fullRender mode cols 1.5 txtPrinter rest doc

-- ---------------------------------------------------------------------------
-- Values and Predicates on GDocs and TextDetails

-- | A document of height and width 1, containing a literal character.
char :: Char -> Doc
char c = textBeside_ (Chr c) (_ILIT(1)) Empty

-- | A document of height 1 containing a literal string.
-- 'text' satisfies the following laws:
--
-- * @'text' s '<>' 'text' t = 'text' (s'++'t)@
--
-- * @'text' \"\" '<>' x = x@, if @x@ non-empty
--
-- The side condition on the last law is necessary because @'text' \"\"@
-- has height 1, while 'empty' has no height.
text :: String -> Doc
text s = case iUnbox (length s) of {sl -> textBeside_ (Str s)  sl Empty}
{-# NOINLINE [0] text #-}   -- Give the RULE a chance to fire
                            -- It must wait till after phase 1 when
                            -- the unpackCString first is manifested

-- RULE that turns (text "abc") into (ptext (A# "abc"#)) to avoid the
-- intermediate packing/unpacking of the string.
{-# RULES
  "text/str" forall a. text (unpackCString# a) = ptext (Ptr a)
 #-}

ftext :: FastString -> Doc
ftext s = case iUnbox (lengthFS s) of {sl -> textBeside_ (PStr s) sl Empty}

ptext :: LitString -> Doc
ptext s = case iUnbox (lengthLS s) of {sl -> textBeside_ (LStr s sl) sl Empty}

ztext :: FastZString -> Doc
ztext s = case iUnbox (lengthFZS s) of {sl -> textBeside_ (ZStr s) sl Empty}

-- | Some text, but without any width. Use for non-printing text
-- such as a HTML or Latex tags
zeroWidthText :: String -> Doc
zeroWidthText s = textBeside_ (Str s) (_ILIT(0)) Empty

-- | The empty document, with no height and no width.
-- 'empty' is the identity for '<>', '<+>', '$$' and '$+$', and anywhere
-- in the argument list for 'sep', 'hcat', 'hsep', 'vcat', 'fcat' etc.
empty :: Doc
empty = Empty

-- | Returns 'True' if the document is empty
isEmpty :: Doc -> Bool
isEmpty Empty = True
isEmpty _     = False

-- | Produce spacing for indenting the amount specified.
--
-- an old version inserted tabs being 8 columns apart in the output.
spaces :: Int# -> String
spaces n | n <=# _ILIT(0) = ""
         | otherwise      = ' ' : spaces (n -# _ILIT(1))

{-
Q: What is the reason for negative indentation (i.e. argument to indent
   is < 0) ?

A:
This indicates an error in the library client's code.
If we compose a <> b, and the first line of b is more indented than some
other lines of b, the law <n6> (<> eats nests) may cause the pretty
printer to produce an invalid layout:

doc       |0123345
------------------
d1        |a...|
d2        |...b|
          |c...|

d1<>d2    |ab..|
         c|....|

Consider a <> b, let `s' be the length of the last line of `a', `k' the
indentation of the first line of b, and `k0' the indentation of the
left-most line b_i of b.

The produced layout will have negative indentation if `k - k0 > s', as
the first line of b will be put on the (s+1)th column, effectively
translating b horizontally by (k-s). Now if the i^th line of b has an
indentation k0 < (k-s), it is translated out-of-page, causing
`negative indentation'.
-}


semi   :: Doc -- ^ A ';' character
comma  :: Doc -- ^ A ',' character
colon  :: Doc -- ^ A ':' character
space  :: Doc -- ^ A space character
equals :: Doc -- ^ A '=' character
lparen :: Doc -- ^ A '(' character
rparen :: Doc -- ^ A ')' character
lbrack :: Doc -- ^ A '[' character
rbrack :: Doc -- ^ A ']' character
lbrace :: Doc -- ^ A '{' character
rbrace :: Doc -- ^ A '}' character
semi   = char ';'
comma  = char ','
colon  = char ':'
space  = char ' '
equals = char '='
lparen = char '('
rparen = char ')'
lbrack = char '['
rbrack = char ']'
lbrace = char '{'
rbrace = char '}'

spaceText, nlText :: TextDetails
spaceText = Chr ' '
nlText    = Chr '\n'

int      :: Int      -> Doc -- ^ @int n = text (show n)@
integer  :: Integer  -> Doc -- ^ @integer n = text (show n)@
float    :: Float    -> Doc -- ^ @float n = text (show n)@
double   :: Double   -> Doc -- ^ @double n = text (show n)@
rational :: Rational -> Doc -- ^ @rational n = text (show n)@
int      n = text (show n)
integer  n = text (show n)
float    n = text (show n)
double   n = text (show n)
rational n = text (show (fromRat n :: Double))
--rational n = text (show (fromRationalX n)) -- _showRational 30 n)

parens       :: Doc -> Doc -- ^ Wrap document in @(...)@
brackets     :: Doc -> Doc -- ^ Wrap document in @[...]@
braces       :: Doc -> Doc -- ^ Wrap document in @{...}@
quotes       :: Doc -> Doc -- ^ Wrap document in @\'...\'@
quote        :: Doc -> Doc
doubleQuotes :: Doc -> Doc -- ^ Wrap document in @\"...\"@
quotes p       = char '`' <> p <> char '\''
quote p        = char '\'' <> p
doubleQuotes p = char '"' <> p <> char '"'
parens p       = char '(' <> p <> char ')'
brackets p     = char '[' <> p <> char ']'
braces p       = char '{' <> p <> char '}'

-- | Apply 'parens' to 'Doc' if boolean is true.
maybeParens :: Bool -> Doc -> Doc
maybeParens False = id
maybeParens True = parens

-- ---------------------------------------------------------------------------
-- Structural operations on GDocs

-- | Perform some simplification of a built up @GDoc@.
reduceDoc :: Doc -> RDoc
reduceDoc (Beside p g q) = beside p g (reduceDoc q)
reduceDoc (Above  p g q) = above  p g (reduceDoc q)
reduceDoc p              = p

-- | List version of '<>'.
hcat :: [Doc] -> Doc
hcat = foldr (<>)  empty

-- | List version of '<+>'.
hsep :: [Doc] -> Doc
hsep = foldr (<+>) empty

-- | List version of '$$'.
vcat :: [Doc] -> Doc
vcat = foldr ($$)  empty

-- | Nest (or indent) a document by a given number of positions
-- (which may also be negative).  'nest' satisfies the laws:
--
-- * @'nest' 0 x = x@
--
-- * @'nest' k ('nest' k' x) = 'nest' (k+k') x@
--
-- * @'nest' k (x '<>' y) = 'nest' k z '<>' 'nest' k y@
--
-- * @'nest' k (x '$$' y) = 'nest' k x '$$' 'nest' k y@
--
-- * @'nest' k 'empty' = 'empty'@
--
-- * @x '<>' 'nest' k y = x '<>' y@, if @x@ non-empty
--
-- The side condition on the last law is needed because
-- 'empty' is a left identity for '<>'.
nest :: Int -> Doc -> Doc
nest k p = mkNest (iUnbox k) (reduceDoc p)

-- | @hang d1 n d2 = sep [d1, nest n d2]@
hang :: Doc -> Int -> Doc -> Doc
hang d1 n d2 = sep [d1, nest n d2]

-- | @punctuate p [d1, ... dn] = [d1 \<> p, d2 \<> p, ... dn-1 \<> p, dn]@
punctuate :: Doc -> [Doc] -> [Doc]
punctuate _ []     = []
punctuate p (x:xs) = go x xs
                   where go y []     = [y]
                         go y (z:zs) = (y <> p) : go z zs

-- mkNest checks for Nest's invariant that it doesn't have an Empty inside it
mkNest :: Int# -> Doc -> Doc
mkNest k (Nest k1 p)       = mkNest (k +# k1) p
mkNest _ NoDoc             = NoDoc
mkNest _ Empty             = Empty
mkNest k p  | k ==# _ILIT(0)  = p       -- Worth a try!
mkNest k p                 = nest_ k p

-- mkUnion checks for an empty document
mkUnion :: Doc -> Doc -> Doc
mkUnion Empty _ = Empty
mkUnion p q     = p `union_` q

-- Arg of a NilAbove is always an RDoc
nilAbove_ :: Doc -> Doc
nilAbove_ = NilAbove

-- Arg of a TextBeside is always an RDoc
textBeside_ :: TextDetails -> FastInt -> Doc -> Doc
textBeside_ = TextBeside

-- Arg of Nest is always an RDoc
nest_ :: FastInt -> Doc -> Doc
nest_ = Nest

-- Args of union are always RDocs
union_ :: Doc -> Doc -> Doc
union_ = Union


-- ---------------------------------------------------------------------------
-- Vertical composition @$$@

-- | Above, except that if the last line of the first argument stops
-- at least one position before the first line of the second begins,
-- these two lines are overlapped.  For example:
--
-- >    text "hi" $$ nest 5 (text "there")
--
-- lays out as
--
-- >    hi   there
--
-- rather than
--
-- >    hi
-- >         there
--
-- '$$' is associative, with identity 'empty', and also satisfies
--
-- * @(x '$$' y) '<>' z = x '$$' (y '<>' z)@, if @y@ non-empty.
--
($$) :: Doc -> Doc -> Doc
p $$  q = Above p False q

-- | Above, with no overlapping.
-- '$+$' is associative, with identity 'empty'.
($+$) :: Doc -> Doc -> Doc
p $+$ q = Above p True q

above :: Doc -> Bool -> RDoc -> RDoc
above (Above p g1 q1)  g2 q2 = above p g1 (above q1 g2 q2)
above p@(Beside{})     g  q  = aboveNest (reduceDoc p) g (_ILIT(0)) (reduceDoc q)
above p g q                  = aboveNest p             g (_ILIT(0)) (reduceDoc q)

-- Specfication: aboveNest p g k q = p $g$ (nest k q)
aboveNest :: RDoc -> Bool -> FastInt -> RDoc -> RDoc
aboveNest NoDoc               _ _ _ = NoDoc
aboveNest (p1 `Union` p2)     g k q = aboveNest p1 g k q `union_`
                                      aboveNest p2 g k q

aboveNest Empty               _ k q = mkNest k q
aboveNest (Nest k1 p)         g k q = nest_ k1 (aboveNest p g (k -# k1) q)
                                  -- p can't be Empty, so no need for mkNest

aboveNest (NilAbove p)        g k q = nilAbove_ (aboveNest p g k q)
aboveNest (TextBeside s sl p) g k q = textBeside_ s sl rest
                                    where
                                      !k1  = k -# sl
                                      rest = case p of
                                                Empty -> nilAboveNest g k1 q
                                                _     -> aboveNest  p g k1 q
aboveNest (Above {})          _ _ _ = error "aboveNest Above"
aboveNest (Beside {})         _ _ _ = error "aboveNest Beside"

-- Specification: text s <> nilaboveNest g k q
--              = text s <> (text "" $g$ nest k q)
nilAboveNest :: Bool -> FastInt -> RDoc -> RDoc
nilAboveNest _ _ Empty       = Empty
                               -- Here's why the "text s <>" is in the spec!
nilAboveNest g k (Nest k1 q) = nilAboveNest g (k +# k1) q
nilAboveNest g k q           | not g && k ># _ILIT(0)        -- No newline if no overlap
                             = textBeside_ (Str (spaces k)) k q
                             | otherwise           -- Put them really above
                             = nilAbove_ (mkNest k q)


-- ---------------------------------------------------------------------------
-- Horizontal composition @<>@

-- We intentionally avoid Data.Monoid.(<>) here due to interactions of
-- Data.Monoid.(<>) and (<+>).  See
-- http://www.haskell.org/pipermail/libraries/2011-November/017066.html

-- | Beside.
-- '<>' is associative, with identity 'empty'.
(<>) :: Doc -> Doc -> Doc
p <>  q = Beside p False q

-- | Beside, separated by space, unless one of the arguments is 'empty'.
-- '<+>' is associative, with identity 'empty'.
(<+>) :: Doc -> Doc -> Doc
p <+> q = Beside p True  q

-- Specification: beside g p q = p <g> q
beside :: Doc -> Bool -> RDoc -> RDoc
beside NoDoc               _ _   = NoDoc
beside (p1 `Union` p2)     g q   = beside p1 g q `union_` beside p2 g q
beside Empty               _ q   = q
beside (Nest k p)          g q   = nest_ k $! beside p g q
beside p@(Beside p1 g1 q1) g2 q2
         | g1 == g2              = beside p1 g1 $! beside q1 g2 q2
         | otherwise             = beside (reduceDoc p) g2 q2
beside p@(Above{})         g q   = let d = reduceDoc p in d `seq` beside d g q
beside (NilAbove p)        g q   = nilAbove_ $! beside p g q
beside (TextBeside s sl p) g q   = textBeside_ s sl $! rest
                               where
                                  rest = case p of
                                           Empty -> nilBeside g q
                                           _     -> beside p g q

-- Specification: text "" <> nilBeside g p
--              = text "" <g> p
nilBeside :: Bool -> RDoc -> RDoc
nilBeside _ Empty         = Empty -- Hence the text "" in the spec
nilBeside g (Nest _ p)    = nilBeside g p
nilBeside g p | g         = textBeside_ spaceText (_ILIT(1)) p
              | otherwise = p


-- ---------------------------------------------------------------------------
-- Separate, @sep@

-- Specification: sep ps  = oneLiner (hsep ps)
--                         `union`
--                          vcat ps

-- | Either 'hsep' or 'vcat'.
sep  :: [Doc] -> Doc
sep = sepX True   -- Separate with spaces

-- | Either 'hcat' or 'vcat'.
cat :: [Doc] -> Doc
cat = sepX False  -- Don't

sepX :: Bool -> [Doc] -> Doc
sepX _ []     = empty
sepX x (p:ps) = sep1 x (reduceDoc p) (_ILIT(0)) ps


-- Specification: sep1 g k ys = sep (x : map (nest k) ys)
--                            = oneLiner (x <g> nest k (hsep ys))
--                              `union` x $$ nest k (vcat ys)
sep1 :: Bool -> RDoc -> FastInt -> [Doc] -> RDoc
sep1 _ NoDoc               _ _  = NoDoc
sep1 g (p `Union` q)       k ys = sep1 g p k ys `union_`
                                  aboveNest q False k (reduceDoc (vcat ys))

sep1 g Empty               k ys = mkNest k (sepX g ys)
sep1 g (Nest n p)          k ys = nest_ n (sep1 g p (k -# n) ys)

sep1 _ (NilAbove p)        k ys = nilAbove_
                                  (aboveNest p False k (reduceDoc (vcat ys)))
sep1 g (TextBeside s sl p) k ys = textBeside_ s sl (sepNB g p (k -# sl) ys)
sep1 _ (Above {})          _ _  = error "sep1 Above"
sep1 _ (Beside {})         _ _  = error "sep1 Beside"

-- Specification: sepNB p k ys = sep1 (text "" <> p) k ys
-- Called when we have already found some text in the first item
-- We have to eat up nests
sepNB :: Bool -> Doc -> FastInt -> [Doc] -> Doc
sepNB g (Nest _ p) k ys
  = sepNB g p k ys -- Never triggered, because of invariant (2)
sepNB g Empty k ys
  = oneLiner (nilBeside g (reduceDoc rest)) `mkUnion`
    -- XXX: TODO: PRETTY: Used to use True here (but GHC used False...)
    nilAboveNest False k (reduceDoc (vcat ys))
  where
    rest | g         = hsep ys
         | otherwise = hcat ys
sepNB g p k ys
  = sep1 g p k ys


-- ---------------------------------------------------------------------------
-- @fill@

-- | \"Paragraph fill\" version of 'cat'.
fcat :: [Doc] -> Doc
fcat = fill False

-- | \"Paragraph fill\" version of 'sep'.
fsep :: [Doc] -> Doc
fsep = fill True

-- Specification:
--
-- fill g docs = fillIndent 0 docs
--
-- fillIndent k [] = []
-- fillIndent k [p] = p
-- fillIndent k (p1:p2:ps) =
--    oneLiner p1 <g> fillIndent (k + length p1 + g ? 1 : 0)
--                               (remove_nests (oneLiner p2) : ps)
--     `Union`
--    (p1 $*$ nest (-k) (fillIndent 0 ps))
--
-- $*$ is defined for layouts (not Docs) as
-- layout1 $*$ layout2 | hasMoreThanOneLine layout1 = layout1 $$ layout2
--                     | otherwise                  = layout1 $+$ layout2

fill :: Bool -> [Doc] -> Doc
fill _ []     = empty
fill g (p:ps) = fill1 g (reduceDoc p) (_ILIT(0)) ps

fill1 :: Bool -> RDoc -> FastInt -> [Doc] -> Doc
fill1 _ NoDoc               _ _  = NoDoc
fill1 g (p `Union` q)       k ys = fill1 g p k ys `union_`
                                   aboveNest q False k (fill g ys)
fill1 g Empty               k ys = mkNest k (fill g ys)
fill1 g (Nest n p)          k ys = nest_ n (fill1 g p (k -# n) ys)
fill1 g (NilAbove p)        k ys = nilAbove_ (aboveNest p False k (fill g ys))
fill1 g (TextBeside s sl p) k ys = textBeside_ s sl (fillNB g p (k -# sl) ys)
fill1 _ (Above {})          _ _  = error "fill1 Above"
fill1 _ (Beside {})         _ _  = error "fill1 Beside"

fillNB :: Bool -> Doc -> Int# -> [Doc] -> Doc
fillNB g (Nest _ p)  k ys   = fillNB g p k ys
                              -- Never triggered, because of invariant (2)
fillNB _ Empty _ []         = Empty
fillNB g Empty k (y:ys)     = nilBeside g (fill1 g (oneLiner (reduceDoc y)) k' ys)
                              `mkUnion`
                              nilAboveNest False k (fill g (y:ys))
                            where
                              !k' | g         = k -# _ILIT(1)
                                  | otherwise = k

fillNB g p k ys             = fill1 g p k ys


-- ---------------------------------------------------------------------------
-- Selecting the best layout

best :: Int   -- Line length
     -> Int   -- Ribbon length
     -> RDoc
     -> RDoc  -- No unions in here!
best w_ r_ p
  = get (iUnbox w_) p
  where
    !r = iUnbox r_
    get :: FastInt          -- (Remaining) width of line
        -> Doc -> Doc
    get _ Empty               = Empty
    get _ NoDoc               = NoDoc
    get w (NilAbove p)        = nilAbove_ (get w p)
    get w (TextBeside s sl p) = textBeside_ s sl (get1 w sl p)
    get w (Nest k p)          = nest_ k (get (w -# k) p)
    get w (p `Union` q)       = nicest w r (get w p) (get w q)
    get _ (Above {})          = error "best get Above"
    get _ (Beside {})         = error "best get Beside"

    get1 :: FastInt         -- (Remaining) width of line
         -> FastInt         -- Amount of first line already eaten up
         -> Doc         -- This is an argument to TextBeside => eat Nests
         -> Doc         -- No unions in here!

    get1 _ _  Empty               = Empty
    get1 _ _  NoDoc               = NoDoc
    get1 w sl (NilAbove p)        = nilAbove_ (get (w -# sl) p)
    get1 w sl (TextBeside t tl p) = textBeside_ t tl (get1 w (sl +# tl) p)
    get1 w sl (Nest _ p)          = get1 w sl p
    get1 w sl (p `Union` q)       = nicest1 w r sl (get1 w sl p)
                                                   (get1 w sl q)
    get1 _ _  (Above {})          = error "best get1 Above"
    get1 _ _  (Beside {})         = error "best get1 Beside"

nicest :: FastInt -> FastInt -> Doc -> Doc -> Doc
nicest w r p q = nicest1 w r (_ILIT(0)) p q

nicest1 :: FastInt -> FastInt -> Int# -> Doc -> Doc -> Doc
nicest1 w r sl p q | fits ((w `minFastInt` r) -# sl) p = p
                   | otherwise                   = q

fits :: FastInt     -- Space available
     -> Doc
     -> Bool -- True if *first line* of Doc fits in space available
fits n _   | n <# _ILIT(0) = False
fits _ NoDoc               = False
fits _ Empty               = True
fits _ (NilAbove _)        = True
fits n (TextBeside _ sl p) = fits (n -# sl) p
fits _ (Above {})          = error "fits Above"
fits _ (Beside {})         = error "fits Beside"
fits _ (Union {})          = error "fits Union"
fits _ (Nest {})           = error "fits Nest"

-- | @first@ returns its first argument if it is non-empty, otherwise its second.
first :: Doc -> Doc -> Doc
first p q | nonEmptySet p = p -- unused, because (get OneLineMode) is unused
          | otherwise     = q

nonEmptySet :: Doc -> Bool
nonEmptySet NoDoc              = False
nonEmptySet (_ `Union` _)      = True
nonEmptySet Empty              = True
nonEmptySet (NilAbove _)       = True
nonEmptySet (TextBeside _ _ p) = nonEmptySet p
nonEmptySet (Nest _ p)         = nonEmptySet p
nonEmptySet (Above {})         = error "nonEmptySet Above"
nonEmptySet (Beside {})        = error "nonEmptySet Beside"

-- @oneLiner@ returns the one-line members of the given set of @GDoc@s.
oneLiner :: Doc -> Doc
oneLiner NoDoc               = NoDoc
oneLiner Empty               = Empty
oneLiner (NilAbove _)        = NoDoc
oneLiner (TextBeside s sl p) = textBeside_ s sl (oneLiner p)
oneLiner (Nest k p)          = nest_ k (oneLiner p)
oneLiner (p `Union` _)       = oneLiner p
oneLiner (Above {})          = error "oneLiner Above"
oneLiner (Beside {})         = error "oneLiner Beside"


-- ---------------------------------------------------------------------------
-- Rendering

-- | Rendering mode.
data Mode = PageMode     -- ^ Normal
          | ZigZagMode   -- ^ With zig-zag cuts
          | LeftMode     -- ^ No indentation, infinitely long lines
          | OneLineMode  -- ^ All on one line

-- | Default TextDetails printer
txtPrinter :: TextDetails -> String -> String
txtPrinter (Chr c)   s  = c:s
txtPrinter (Str s1)  s2 = s1 ++ s2
txtPrinter (PStr s1) s2 = unpackFS s1 ++ s2
txtPrinter (ZStr s1) s2 = zString s1 ++ s2
txtPrinter (LStr s1 _) s2 = unpackLitString s1 ++ s2

-- | The general rendering interface.
fullRender :: Mode                     -- ^ Rendering mode
           -> Int                      -- ^ Line length
           -> Float                    -- ^ Ribbons per line
           -> (TextDetails -> a -> a)  -- ^ What to do with text
           -> a                        -- ^ What to do at the end
           -> Doc                      -- ^ The document
           -> a                        -- ^ Result
fullRender OneLineMode _ _ txt end doc
  = lay (reduceDoc doc)
  where
    lay NoDoc              = cant_fail
    lay (Union _ q)        = lay q -- Second arg can't be NoDoc
    lay (Nest _ p)         = lay p
    lay Empty              = end
    lay (NilAbove p)       = spaceText `txt` lay p -- NoDoc always on first line
    lay (TextBeside s _ p) = s `txt` lay p
    lay (Above {})         = error "fullRender/OneLineMode Above"
    lay (Beside {})        = error "fullRender/OneLineMode Beside"

fullRender LeftMode    _ _ txt end doc
  = lay (reduceDoc doc)
  where
    lay NoDoc              = cant_fail
    lay (Union p q)        = lay (first p q)
    lay (Nest _ p)         = lay p
    lay Empty              = end
    lay (NilAbove p)       = nlText `txt` lay p -- NoDoc always on first line
    lay (TextBeside s _ p) = s `txt` lay p
    lay (Above {})         = error "fullRender/LeftMode Above"
    lay (Beside {})        = error "fullRender/LeftMode Beside"

fullRender m lineLen ribbons txt rest doc
  = display m lineLen ribbonLen txt rest doc'
  where
    doc' = best bestLineLen ribbonLen (reduceDoc doc)

    bestLineLen, ribbonLen :: Int
    ribbonLen   = round (fromIntegral lineLen / ribbons)
    bestLineLen = case m of
                      ZigZagMode -> maxBound
                      _          -> lineLen

display :: Mode -> Int -> Int -> (TextDetails -> a -> a) -> a -> Doc -> a
display m page_width ribbon_width txt end doc
  = case (iUnbox page_width) -# (iUnbox ribbon_width) of { gap_width ->
    case gap_width `quotFastInt` _ILIT(2) of { shift ->
    let
        lay k (Nest k1 p)  = lay (k +# k1) p
        lay _ Empty        = end
        lay k (NilAbove p) = nlText `txt` lay k p
        lay k (TextBeside s sl p)
            = case m of
                    ZigZagMode |  k >=# gap_width
                               -> nlText `txt` (
                                  Str (multi_ch shift '/') `txt` (
                                  nlText `txt`
                                  lay1 (k -# shift) s sl p ))

                               |  k <# _ILIT(0)
                               -> nlText `txt` (
                                  Str (multi_ch shift '\\') `txt` (
                                  nlText `txt`
                                  lay1 (k +# shift) s sl p ))

                    _ -> lay1 k s sl p
        lay _ (Above {})   = error "display lay Above"
        lay _ (Beside {})  = error "display lay Beside"
        lay _ NoDoc        = error "display lay NoDoc"
        lay _ (Union {})   = error "display lay Union"

        lay1 k s sl p = indent k (s `txt` lay2 (k +# sl) p)

        lay2 k (NilAbove p)        = nlText `txt` lay k p
        lay2 k (TextBeside s sl p) = s `txt` lay2 (k +# sl) p
        lay2 k (Nest _ p)          = lay2 k p
        lay2 _ Empty               = end
        lay2 _ (Above {})          = error "display lay2 Above"
        lay2 _ (Beside {})         = error "display lay2 Beside"
        lay2 _ NoDoc               = error "display lay2 NoDoc"
        lay2 _ (Union {})          = error "display lay2 Union"

        -- optimise long indentations using LitString chunks of 8 spaces
        indent n r | n >=# _ILIT(8) = LStr (sLit "        ") (_ILIT(8)) `txt`
                                      indent (n -# _ILIT(8)) r
                   | otherwise      = Str (spaces n) `txt` r
    in
    lay (_ILIT(0)) doc
    }}

cant_fail :: a
cant_fail = error "easy_display: NoDoc"

multi_ch :: Int# -> Char -> String
multi_ch n ch | n <=# _ILIT(0) = ""
              | otherwise      = ch : multi_ch (n -# _ILIT(1)) ch

printDoc :: Mode -> Int -> Handle -> Doc -> IO ()
-- printDoc adds a newline to the end
printDoc mode cols hdl doc = printDoc_ mode cols hdl (doc $$ text "")

printDoc_ :: Mode -> Int -> Handle -> Doc -> IO ()
-- printDoc_ does not add a newline at the end, so that
-- successive calls can output stuff on the same line
-- Rather like putStr vs putStrLn
printDoc_ LeftMode _ hdl doc
  = do { printLeftRender hdl doc; hFlush hdl }
printDoc_ mode pprCols hdl doc
  = do { fullRender mode pprCols 1.5 put done doc ;
         hFlush hdl }
  where
    put (Chr c)  next = hPutChar hdl c >> next
    put (Str s)  next = hPutStr  hdl s >> next
    put (PStr s) next = hPutStr  hdl (unpackFS s) >> next
                        -- NB. not hPutFS, we want this to go through
                        -- the I/O library's encoding layer. (#3398)
    put (ZStr s) next = hPutFZS  hdl s >> next
    put (LStr s l) next = hPutLitString hdl s l >> next

    done = return () -- hPutChar hdl '\n'

  -- some versions of hPutBuf will barf if the length is zero
hPutLitString :: Handle -> Ptr a -> Int# -> IO ()
hPutLitString handle a l = if l ==# _ILIT(0)
                            then return ()
                            else hPutBuf handle a (iBox l)

-- Printing output in LeftMode is performance critical: it's used when
-- dumping C and assembly output, so we allow ourselves a few dirty
-- hacks:
--
-- (1) we specialise fullRender for LeftMode with IO output.
--
-- (2) we add a layer of buffering on top of Handles.  Handles
--     don't perform well with lots of hPutChars, which is mostly
--     what we're doing here, because Handles have to be thread-safe
--     and async exception-safe.  We only have a single thread and don't
--     care about exceptions, so we add a layer of fast buffering
--     over the Handle interface.
--
-- (3) a few hacks in layLeft below to convince GHC to generate the right
--     code.

printLeftRender :: Handle -> Doc -> IO ()
printLeftRender hdl doc = do
  b <- newBufHandle hdl
  bufLeftRender b doc
  bFlush b

bufLeftRender :: BufHandle -> Doc -> IO ()
bufLeftRender b doc = layLeft b (reduceDoc doc)

-- HACK ALERT!  the "return () >>" below convinces GHC to eta-expand
-- this function with the IO state lambda.  Otherwise we end up with
-- closures in all the case branches.
layLeft :: BufHandle -> Doc -> IO ()
layLeft b _ | b `seq` False  = undefined -- make it strict in b
layLeft _ NoDoc              = cant_fail
layLeft b (Union p q)        = return () >> layLeft b (first p q)
layLeft b (Nest _ p)         = return () >> layLeft b p
layLeft b Empty              = bPutChar b '\n'
layLeft b (NilAbove p)       = bPutChar b '\n' >> layLeft b p
layLeft b (TextBeside s _ p) = put b s >> layLeft b p
 where
    put b _ | b `seq` False = undefined
    put b (Chr c)    = bPutChar b c
    put b (Str s)    = bPutStr  b s
    put b (PStr s)   = bPutFS   b s
    put b (ZStr s)   = bPutFZS  b s
    put b (LStr s l) = bPutLitString b s l
layLeft _ _                  = panic "layLeft: Unhandled case"

-- Define error=panic, for easier comparison with libraries/pretty.
error :: String -> a
error = panic
