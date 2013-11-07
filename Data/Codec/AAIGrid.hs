module Data.Codec.AAIGrid (ReferencedGrid, GridHeader, BoundingBox, Coordinate, Header, openGridFile, gridFile) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error
import Data.List
import Data.Maybe
import System.IO

data GridHeader     = GridHeader BoundingBox Int Int 
data Header         = Header String Float
  deriving (Show, Eq)
data Coordinate     = Coordinate Float Float
  deriving Show
data BoundingBox    = BoundingBox Coordinate Coordinate
  deriving Show
data ReferencedGrid = ReferencedGrid GridHeader [[Float]] | NoGrid String

instance Show ReferencedGrid where 
  show (ReferencedGrid gh ff) = show gh ++ "\n" ++ (show $ length ff) ++ " rows parsed." 
instance Show GridHeader where 
  show (GridHeader bb rows cols) = show bb ++ "\n" ++ (show rows) ++ " rows\n" ++ (show cols) ++ " columns"

openGridFile :: FilePath -> IO ReferencedGrid
openGridFile filename = do
  bytes <- readFile filename
  case parse gridFile "" bytes of
    Right rg    -> return rg
    Left errors -> return (NoGrid $ show errors)

headerValue :: Header -> Float
headerValue (Header _ v) = v

getHeader :: [Header] -> String -> Maybe Header 
getHeader hs needle = 
   find (\(Header key value) -> key == needle) hs

buildHeader :: [Header] -> [[Float]] -> Either String GridHeader
buildHeader hs rows 
  | nrows == Nothing = Left "nrows header not found" 
  | ncols == Nothing = Left "ncols header not found"
  | xll   == Nothing = Left "xllcorner header not found"
  | yll   == Nothing = Left "yllcorner header not found"
  | dx    == Nothing = Left "dx header not found"
  | dy    == Nothing = Left "dy header not found"
  | otherwise        = 
    -- if (floor . headerValue . fromJust) nrows == lrows then
      let xl = (headerValue $ fromJust xll)
          yl = (headerValue $ fromJust yll)
          xr = xl + (headerValue $ fromJust dx) * (headerValue $ fromJust nrows)
          yr = yl + (headerValue $ fromJust dy) * (headerValue $ fromJust ncols)
          nr = floor (headerValue $ fromJust nrows)
          nc = floor (headerValue $ fromJust ncols)
      in 
        Right (GridHeader (BoundingBox (Coordinate xl yl) (Coordinate xr yr)) nr nc)
    -- else 
    --  Left ("nrows header does not match row count " ++ (show lrows))
  where nrows = getHeader hs "nrows"
        ncols = getHeader hs "ncols"
        xll   = getHeader hs "xllcorner"
        yll   = getHeader hs "yllcorner"
        dx    = getHeader hs "dx"
        dy    = getHeader hs "dy" 
        lrows = length rows
  
gridFile :: Parser ReferencedGrid 
gridFile = do  
  headers <- count 6 header  
  rows <- sepBy1 row (many1 newline)
  case buildHeader headers rows of
    Left  msg -> return (NoGrid msg)
    Right hdr -> return (ReferencedGrid hdr rows)

header :: Parser Header
header = do 
  key <- stringCell
  spaces 
  value <- floatCell
  many1 newline
  return (Header key value)

row :: Parser [Float]
row = sepBy1 floatCell (choice [tab, char ' '])

stringCell :: Parser String
stringCell =  many (noneOf " \t\n")

floatCell :: Parser Float
floatCell = do
  string <- stringCell 
  return (read string::Float)
