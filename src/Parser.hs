module Parser
    (
        Syntax(..),
        Symbol(..),
        Error(..),
        ErrorTree(..),
        InputStream(..),
        Output,
        parse,
        ComposableOutput(..)
    ) where

--------------------------------------------------------------------------------
--  Main data types

-- a generic tree
data Tree a = TreeNode a [Tree a]
    deriving Show

-- Syntax
-- | syntax tree
{-
prefix: syn
-}
data Syntax = SynChar Char | SynEOI | SynSeq [Syntax] | SynSel [Syntax] | SynOpt Syntax | SynRep Syntax | SynAbs Syntax
    deriving Show

-- Symbol
-- | symbols
{-
prefix: sym
-}
data Symbol = SymChar Char | SymEOI | SymSeq | SymSel | SymOpt | SymRep | SymAbs (Maybe Char)
    deriving Show

-- SymTree
-- | symbol tree
type SymTree = Tree Symbol

-- Error
-- | kinds of errors
{-
prefix: err
-}
data Error = 
      CharExpected
    | EOIExpected
    | SeqExpected
    | SelExpected
    | AbsExpected
    deriving Show

-- ErrorTree
-- | error tree
type ErrorTree = Tree Error

-- InputStream
-- | string as input stream
{-
prefix: st
-}
type InputStream = String

-- parser output is either a symbol tree or an error tree
data Output = 
        -- | input was valid
        Valid
            -- | symbol tree
            SymTree
            -- | remaining input
            InputStream
        -- | input was invalid
        | Invalid
            -- | error trees
            ErrorTree 
            -- | remaining input
            InputStream
    deriving Show

--------------------------------------------------------------------------------
--  Data types and functions to compose parsing information

-- | parser output that supports composition of all syntax elements including selections, sequences, repetitions, and so on
data ComposableOutput =
    -- | previous input was valid or not yet parsed
      CmpValid
        -- | remaining input
        InputStream
        -- | symbol tree
        SymTree
        -- | error trees
        [ErrorTree]
    -- | previous input was invalid
    | CmpInvalid
        -- | remaining input
        InputStream
        -- | error tree
        ErrorTree
    deriving Show

outFromCmpOut :: ComposableOutput -> Output
outFromCmpOut (CmpValid st symTree _) = Valid symTree st
outFromCmpOut (CmpInvalid st lErr) = Invalid lErr st

-- parser output that supports composition of syntax element sequence
data ComposableOutputSeq =
    CmpSeqValid { stValidRemainingCmpSeq :: InputStream, lSymTreeCmpSeq :: [SymTree], lErrHintCmpSeq :: [ErrorTree] }
    | CmpSeqInvalid { stInvalidRemainingCmpSeq :: InputStream, lErrTreeCmpSeq :: ErrorTree }
    deriving Show

cmpOutFromCmpOutSeq :: ComposableOutputSeq -> ComposableOutput
cmpOutFromCmpOutSeq (CmpSeqValid st lSymTree lErrHint) = CmpValid st (TreeNode SymSeq lSymTree) lErrHint
cmpOutFromCmpOutSeq (CmpSeqInvalid st errTree) = CmpInvalid st (TreeNode SeqExpected [errTree])

-- parser output that supports composition of syntax element selections
data ComposableOutputSel =
      CmpSelValid { stValidRemainingCmpSel :: InputStream, symTreeCmpSel :: SymTree, lErrHintCmpSel :: [ErrorTree] }
    | CmpSelInvalid { stInvalidRemainingCmpSel :: InputStream, lErrTreeCmpSel :: ErrorTree }
    deriving Show

cmpOutFromCmpOutSel :: ComposableOutputSel -> ComposableOutput
cmpOutFromCmpOutSel (CmpSelValid st symTree lErrHint) = CmpValid st (TreeNode SymSel [symTree]) lErrHint
cmpOutFromCmpOutSel (CmpSelInvalid st errTree) = CmpInvalid st errTree

-- parser output that supports composition of syntax element option
data ComposableOutputOpt =
      CmpOptValid { stValidRemainingCmpOpt :: InputStream, symTreeCmpOpt :: Maybe SymTree, lErrHintCmpOpt :: [ErrorTree] }
    | CmpOptInvalid { stInvalidRemainingCmpOpt :: InputStream, lErrTreeCmpOpt :: ErrorTree }
    deriving Show

cmpOutFromCmpOutOpt :: ComposableOutputOpt -> ComposableOutput
cmpOutFromCmpOutOpt (CmpOptValid st (Just symTree) lErrHint) = CmpValid st (TreeNode SymOpt [symTree]) lErrHint
cmpOutFromCmpOutOpt (CmpOptValid st Nothing lErrHint) = CmpValid st (TreeNode SymOpt []) lErrHint
cmpOutFromCmpOutOpt (CmpOptInvalid st errTree) = undefined -- because: should never happen

-- parser output that supports composition of syntax element repetition
data ComposableOutputRep =
      CmpRepValid { stValidRemainingCmpRep :: InputStream, lSymTreeCmpRep :: [SymTree], lErrHintCmpRep :: [ErrorTree] }
    | CmpRepInvalid { stInvalidRemainingCmpRep :: InputStream, lErrTreeCmpRep :: ErrorTree }
    deriving Show

cmpOutFromCmpOutRep :: ComposableOutputRep -> ComposableOutput
cmpOutFromCmpOutRep (CmpRepValid st lSymTree lErrHint) = CmpValid st (TreeNode SymRep lSymTree) lErrHint
cmpOutFromCmpOutRep (CmpRepInvalid st errTree) = undefined -- because: should never happen

-- parser output that supports composition of syntax element absence
data ComposableOutputAbs =
      CmpAbsValid { stValidRemainingCmpAbs :: InputStream, chReadCmpAbs :: Maybe Char, lErrHintCmpAbs :: [ErrorTree] }
    | CmpAbsInvalid { stInvalidRemainingCmpAbs :: InputStream }
    deriving Show

cmpOutFromCmpOutAbs :: ComposableOutputAbs -> ComposableOutput
cmpOutFromCmpOutAbs (CmpAbsValid st mch lErrHint) = CmpValid st (TreeNode (SymAbs mch) []) lErrHint
cmpOutFromCmpOutAbs (CmpAbsInvalid st) = CmpInvalid st (TreeNode AbsExpected [])

--------------------------------------------------------------------------------
--  The parser

parse :: InputStream -> Syntax -> Output

-- parsing SynChar
parse inp syn = outFromCmpOut (parseComposable inp syn)

-- composable parser
parseComposable :: InputStream -> Syntax -> ComposableOutput

-- parsing SynChar
parseComposable [] syn@(SynChar ch) = CmpInvalid [] (TreeNode CharExpected [])
parseComposable st@(es:rs) (SynChar ch)
    | es == ch = CmpValid rs (TreeNode (SymChar ch) []) []
    | otherwise = CmpInvalid st (TreeNode CharExpected [])

-- parsing SynEOI
parseComposable [] SynEOI = CmpValid [] (TreeNode SymEOI []) []
parseComposable st SynEOI = CmpInvalid st (TreeNode EOIExpected [])

-- parsing SynSeq
parseComposable inp (SynSeq lSyn) = cmpOutFromCmpOutSeq (parseComposableSeq inp lSyn)

-- parsing SynSel
parseComposable inp (SynSel lSyn) = cmpOutFromCmpOutSel (parseComposableSel [] inp lSyn)

-- parsing SynOpt
parseComposable inp (SynOpt synOpt) = cmpOutFromCmpOutOpt (parseComposableOpt [] inp synOpt)

-- parsing SynRep
parseComposable inp (SynRep synRep) = cmpOutFromCmpOutRep (parseComposableRep [] inp synRep)

-- parsing SynAbs
parseComposable inp (SynAbs synAbs) = cmpOutFromCmpOutAbs (parseComposableAbs [] inp synAbs)

-- composable parser for sequences
parseComposableSeq :: InputStream -> [Syntax] -> ComposableOutputSeq
parseComposableSeq inp [] = CmpSeqValid inp [] []
parseComposableSeq inp (eSyn:rSyn) = parseComposableSeq' inp eSyn rSyn
    where
        parseComposableSeq' :: InputStream -> Syntax -> [Syntax] -> ComposableOutputSeq
        parseComposableSeq' inp eSyn {-rSyn-} = parseComposableSeq'' (parseComposable inp eSyn) {-rSyn-}
        parseComposableSeq'' :: ComposableOutput -> [Syntax] -> ComposableOutputSeq
        parseComposableSeq'' (CmpValid st symTree lErrHint) lSyn = parseComposableSeqValid'' symTree (parseComposableSeq st lSyn)
        parseComposableSeq'' (CmpInvalid st lErr) _ = CmpSeqInvalid st lErr
        parseComposableSeqValid'' :: SymTree -> ComposableOutputSeq -> ComposableOutputSeq
        parseComposableSeqValid'' symTree (CmpSeqValid st lSymTree lErrHint) = CmpSeqValid st (symTree : lSymTree) lErrHint
        parseComposableSeqValid'' symTree (CmpSeqInvalid st lErr) = CmpSeqInvalid st lErr

-- composable parser for selections
parseComposableSel :: [ErrorTree] -> InputStream -> [Syntax] -> ComposableOutputSel
parseComposableSel lErrHint0 inp [] = CmpSelInvalid inp (TreeNode SelExpected lErrHint0)
parseComposableSel lErrHint0 inp lSyn@(eSyn:rSyn) = parseComposableSel' lErrHint0 inp eSyn rSyn
    where
        parseComposableSel' :: [ErrorTree] -> InputStream -> Syntax -> [Syntax] -> ComposableOutputSel
        parseComposableSel' lErrHint0 inp eSyn {-rSyn-} = parseComposableSel'' lErrHint0 (parseComposable inp eSyn) {-rSyn-}
        parseComposableSel'' :: [ErrorTree] -> ComposableOutput -> [Syntax] -> ComposableOutputSel
        parseComposableSel'' lErrHint0 (CmpValid st symTree lErrHint) lSyn = CmpSelValid st symTree (lErrHint0 ++ lErrHint)
        parseComposableSel'' lErrHint0 (CmpInvalid st errTree) lSyn = parseComposableSel (errTree : lErrHint0) st lSyn

-- composable parser for options
parseComposableOpt :: [ErrorTree] -> InputStream -> Syntax -> ComposableOutputOpt
parseComposableOpt lErrHint0 inp syn = parseComposableOpt' lErrHint0 inp syn
    where
        parseComposableOpt' :: [ErrorTree] -> InputStream -> Syntax -> ComposableOutputOpt
        parseComposableOpt' lErrHint0 inp syn = parseComposableOpt'' lErrHint0 (parseComposable inp syn)
        parseComposableOpt'' :: [ErrorTree] -> ComposableOutput -> ComposableOutputOpt
        parseComposableOpt'' lErrHint0 (CmpValid st symTree lErrHint) = CmpOptValid st (Just symTree) (lErrHint0 ++ lErrHint)
        parseComposableOpt'' lErrHint0 (CmpInvalid st errTree) = CmpOptValid st Nothing lErrHint0

-- composable parser for repetitions
parseComposableRep :: [ErrorTree] -> InputStream -> Syntax -> ComposableOutputRep
parseComposableRep lErrHint0 inp syn = parseComposableRep' lErrHint0 inp syn
    where
        parseComposableRep' :: [ErrorTree] -> InputStream -> Syntax -> ComposableOutputRep
        parseComposableRep' lErrHint0 [] syn = CmpRepValid [] [] lErrHint0
        parseComposableRep' lErrHint0 inp syn = parseComposableRep'' lErrHint0 (parseComposable inp syn) syn
        parseComposableRep'' :: [ErrorTree] -> ComposableOutput -> Syntax -> ComposableOutputRep
        parseComposableRep'' lErrHint0 (CmpValid st symTree lErrHint) syn = parseComposableRepValid'' symTree (parseComposableRep (lErrHint0 ++ lErrHint) st syn)
        parseComposableRep'' lErrHint0 (CmpInvalid st lErr) _ = CmpRepValid st [] lErrHint0
        parseComposableRepValid'' :: SymTree -> ComposableOutputRep -> ComposableOutputRep
        parseComposableRepValid'' symTree (CmpRepValid st lSymTree lErrHint) = CmpRepValid st (symTree : lSymTree) lErrHint
        parseComposableRepValid'' symTree (CmpRepInvalid st lErr) = CmpRepInvalid st lErr

-- composable parser for absence
parseComposableAbs :: [ErrorTree] -> InputStream -> Syntax -> ComposableOutputAbs
parseComposableAbs lErrHint0 inp syn = parseComposableAbs' lErrHint0 inp syn
    where
        parseComposableAbs' :: [ErrorTree] -> InputStream -> Syntax -> ComposableOutputAbs
        parseComposableAbs' lErrHint0 inp syn = parseComposableAbs'' lErrHint0 inp (parseComposable inp syn)
        parseComposableAbs'' :: [ErrorTree] -> InputStream -> ComposableOutput -> ComposableOutputAbs
        parseComposableAbs'' _ st0 (CmpValid st symTree lErrHint) = CmpAbsInvalid st0
        parseComposableAbs'' lErrHint0 [] (CmpInvalid st errTree) = CmpAbsValid [] Nothing lErrHint0
        parseComposableAbs'' lErrHint0 (es:rs) (CmpInvalid st errTree) = CmpAbsValid rs (Just es) lErrHint0
