module RMC where

{-
parseWith :: Show a => Parser a -> String -> 
parseWith p input
    = case (parse p "" input) of
        Left err -> error "incorrect parse: unimplemented yet"
        Right x -> x
-}

import Text.ParserCombinators.Parsec as P
import Data.Char
import Control.Exception

data RMC = RMC {
      time		:: RMCTime,
      status		:: RMCStatus,
      location		:: RMCLocation,
      speed		:: Double,
      sDirection	:: Double,		-- speed direction
      date		:: RMCDate,			
      mDecl		:: Double,		-- Magnetic declination
      mDeclDir		:: Bool,		-- True("e") or False("w")
      modeIndicator	:: RMCModeIndicator
    }

--------------------------------------------------------------------------------

data RMCTime 		= RMCTime
                          deriving (Show, Read)
----------------------------------------
data RMCDate 		= RMCDate
                          deriving (Show, Read)
----------------------------------------
data RMCLocation 	= RMCLocation {
      latitude 		:: Angle,
      p			:: Char,
      longitude		:: Angle,
      j			:: Char
    }

data Angle = Angle {
      degree	:: Int,
      minute	:: Double
    }

instance Read Angle where
    readsPrec _ input 	 = [(IndA, xs)]
                    
    

toRMCLocation [latitude, p, longitude, j]@input = 
    if (not ((p `elem` "NS") &&
             (j `elem` "EW")))
    then error $ "while parsing RMCLocation with args:" ++ concat input
    else RMCLocation (read latitude)
                     p
                     (read latitude)
                     j
----------------------------------------

data RMCModeIndicator 	= IndA | IndD | IndE | IndN

instance Read RMCModeIndicator where
    readsPrec _ ('A':xs) = [(IndA, xs)]
    readsPrec _ ('D':xs) = [(IndD, xs)]
    readsPrec _ ('E':xs) = [(IndE, xs)]
    readsPrec _ ('N':xs) = [(IndN, xs)]
    readsPrec _ _	 = []

instance Show RMCModeIndicator where
    show IndA = "A"
    show IndD = "D"
    show IndE = "E"
    show IndN = "N"

----------------------------------------
data RMCStatus	 	= StatusA | StatusV

instance Read RMCStatus where
    readsPrec _ ('A':xs) = [(StatusA, xs)]
    readsPrec _ ('V':xs) = [(StatusV, xs)]
    readsPrec _ _	 = []

instance Show RMCStatus where
    show StatusA = "A"
    show StatusV = "A"

----------------------------------------

parseRMC :: String -> Either ParseError RMC
parseRMC = parse rmcParser ""

rmcParser :: Parser RMC
rmcParser = do string "$GPRMC,"
               cells <- sepBy1 phrase (skipMany $ char ',')
               char '*'
               s1 <- symbol
               s2 <- symbol
               do P.try cr
                  lf
                  <|> cr 
               return $ toRMC $ cells ++ [[s1,s2]]
                   where cr = char $ chr 0x0d
                         lf = char $ chr 0x0a

phrase = many symbol
symbol = oneOf ("." ++ ['A' .. 'Z'] ++ ['0' .. '9'])

--------------------------------------------------------------------------------

toRMC :: [String] -> RMC
toRMC cells = case cells of
                [time, status, latitude, p, longitude, j, speed, 
                 sDirection, date, mDecl, mDeclDir, indicator, checkSum] ->
                         RMC (read time)
                             (read status)
                             (toRMCLocation [latitude, p, longitude, j])
                             (read speed)				-- Double
                             (read sDirection)
                             (read date)
                             (read mDecl)
                             (read mDeclDir)
                             (read indicator)