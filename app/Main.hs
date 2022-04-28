module Main where

import Parser

main :: IO ()
main =
    do
        putStrLn ""
        putStrLn "parse \"0\" synNumber:"
        putStr "    "; print $ parse "0" synNumber
        putStrLn ""
        putStrLn "parse \"2\" synNumber"
        putStr "    "; print $ parse "2" synNumber
        putStrLn ""
        putStrLn "parse \"90\" synNumber"
        putStr "    "; print $ parse "90" synNumber
        putStrLn ""
        putStrLn "parse \"3445679898762340598703456870982734509872435\" synNumber"
        putStr "    "; print $ parse "3445679898762340598703456870982734509872435" synNumber
        putStrLn ""
        putStrLn "parse \"01\" synNumber"
        putStr "    "; print $ parse "01" synNumber
        putStrLn ""
        putStrLn "parse \"{-\" synCommentBegin"
        putStr "    "; print $ parse "{-" synCommentBegin
        putStrLn ""
        putStrLn "parse \"{- asdöflkjpoiwert-wertüpoiüp--oisdfg-sdfg+üposdfg-}\" synCommentBegin"
        putStr "    "; print $ parse "{- asdöflkjpoiwert-wertüpoiüp--oisdfg-sdfg+üposdfg-}" synCommentBegin
        putStrLn ""
        putStrLn "parse \"-}\" synCommentEnd"
        putStr "    "; print $ parse "-}" synCommentEnd
        putStrLn ""
        putStrLn "parse \"--\" synNotCommentEnd"
        putStr "    "; print $ parse "--" synNotCommentEnd
        putStrLn ""
        putStrLn "parse \"-}\" synNotCommentEnd"
        putStr "    "; print $ parse "-}" synNotCommentEnd
        putStrLn ""
        putStrLn "parse \"asdöflkjpoiwert-wertüpoiüp--oisdfg-sdfg+üposdfg-\" synNotCommentEnds"
        putStr "    "; print $ parse "asdöflkjpoiwert-wertüpoiüp--oisdfg-sdfg+üposdfg-" synNotCommentEnds
        putStrLn ""
        putStrLn "parse \"asdöflkjpoiwert-wertüpoiüp--oisdfg-sdfg+üposdfg-}\" synNotCommentEnds"
        putStr "    "; print $ parse "asdöflkjpoiwert-wertüpoiüp--oisdfg-sdfg+üposdfg-}" synNotCommentEnds
        putStrLn ""
        putStrLn "parse \"{- asdöflkjpoiwert-wertüpoiüp--oisdfg-sdfg+üposdfg-}\" synComment"
        putStr "    "; print $ parse "{- asdöflkjpoiwert-wertüpoiüp--oisdfg-sdfg+üposdfg-}" synComment
        putStrLn ""
        putStrLn "parse \"90{- asdöflkjpoiwert-wertüpoiüp--oisdfg-sdfg+üposdfg--}3445679898762340598703456870982734509872435\" synMain"
        putStr "    "; print $ parse "90{- asdöflkjpoiwert-wertüpoiüp--oisdfg-sdfg+üposdfg--}3445679898762340598703456870982734509872435" synMain

--------------------------------------------------------------------------------
--  Syntax definition

synMain = SynSeq [synNumber, synComment, synNumber, SynEOI]
synComment = SynSeq [ synCommentBegin, synNotCommentEnds, synCommentEnd]
synCommentBegin = SynSeq [SynChar '{', SynChar '-']
synCommentEnd = SynSeq [SynChar '-', SynChar '}']
synNotCommentEnds = SynRep synNotCommentEnd
synNotCommentEnd = SynAbs synCommentEnd
synNumber = SynSel [synNumberZero,synNonZeroNumber]
synNumberZero = SynChar '0'
synNonZeroNumber = SynSeq [synNonZeroDigit, SynRep synDigit]
synNonZeroDigit = SynSel (map SynChar ['1'..'9'])
synDigit = SynSel (map SynChar ['0'..'9'])
