import Data.Vector
import qualified Data.Vector as V
import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Text as T
import Data.Maybe
import Debug.Trace

data Piece = King | Queen | Rook | Bishop | Knight deriving (Eq, Show, Enum)

traceShowId a = trace (show a) a
tracePcsLeft a = trace ("pieces left: " L.++ show a) a
traceSafeSqs a = trace ("safe squares: " L.++ show a) a 
traceIndex i = trace ("\n--Search--\n-> " L.++ show i) i
traceRemBoard a = trace ("rem board: " L.++ show a) a
traceSearch bc ps pb i = trace ("\n--Search--\nindex: " L.++ show i L.++ "\nboard: " L.++ show bc L.++ "\npieceSet: " L.++ show ps L.++ "\npieces: " L.++ show pb) False

solve files ranks k q r b n =
    let 
        inside (f, r) =
            0 <= f && f < files && 0 <= r && r < ranks 

        moves (f, r) King = 
            [(x, y) | x <- [f - 1 .. f + 1], y <- [r - 1 .. r + 1], inside (x, y)]

        moves sq Queen =
            moves sq Rook L.++ moves sq Bishop

        moves (f, r) Rook =
            [(x, r) | x <- [0 .. files - 1]] L.++ [(f, y) | y <- [0 .. ranks - 1]]

        moves (f, r) Bishop =
            diag1 L.++ diag2
            where
                diag1 = L.zip [f - a1 .. f + b1 - 1] [r - a1 .. r + b1 - 1]
                diag2 = L.zip [f - a2 .. f + b2 - 1] [r + a2, r + a2 - 1 .. r - b2 + 1]
                a1 = min f r
                b1 = min (files - f) (ranks - r)
                a2 = min f (ranks - r - 1)
                b2 = min (files - f) (r + 1)

        moves (f, r) Knight =
            L.filter inside $ L.map (\(x, y) -> (f + x, r + y)) 
                [(-2, -1), (-2, 1), (-1, -2), (-1, 2), (1, -2), (1, 2), (2, -1), (2, 1)]
        
        idx2sq i =
            i `divMod` ranks

        sq2idx sq =
            fst sq * ranks + snd sq

        nextPieces pieceSet = 
            [p | (p, n) <- pieceSet, n > 0]

        popPiece pieceSet piece = 
            L.map (\(p, n) -> (p, n - (fromEnum $ piece == p))) pieceSet

        nextIndex idx board =
            fromMaybe (-1) $ fmap (idx + 1 +) $ findIndex not $ V.drop (idx + 1) board

        coverBoard board moves =
            board // [(i, True) | i <- moves]

        safeSquares idx board =
            V.foldl (\a b -> a + (fromEnum $ not b)) 0 $ V.drop idx board

        noCapture piecesOnBoard moves =
            S.null $ S.intersection (S.fromList $ L.map fst piecesOnBoard) (S.fromList moves)

        search boardCoverage pieceSet piecesOnBoard idx
            | piecesLeft == 0 = [piecesOnBoard]
            | idx < 0 = []
            | (safeSquares idx boardCoverage) < piecesLeft = []
            | otherwise = skipSquare L.++ trySquare
            where 
                piecesLeft = L.sum . L.map snd $ pieceSet
                
                skipSquare = 
                    search boardCoverage pieceSet piecesOnBoard $ nextIndex idx boardCoverage
                
                trySquare = 
                    [s | p <- nextPieces pieceSet,
                         let mv = L.map sq2idx . moves (idx2sq idx) $ p
                             bc = coverBoard boardCoverage mv
                             ps = popPiece pieceSet p
                             pb = (idx, p) : piecesOnBoard
                             i  = nextIndex idx bc,
                         noCapture piecesOnBoard mv,
                         s <- search bc ps pb i]
    in 
        search (V.replicate (files * ranks) False) 
               [(King, k), (Queen, q), (Rook, r), (Bishop, b), (Knight, n)]
               [] 0

main = do 
    line <- getLine
    let files : ranks : k : q : r : b : n : [] = parse line
    print $ L.length $ solve files ranks k q r b n
    where 
        parse line = L.map ((read :: String -> Int)  . T.unpack) $ split line
        split line = L.filter (not . T.null) $ T.splitOn (T.pack " ") (T.pack line)
