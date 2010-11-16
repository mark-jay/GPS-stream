
import Text.ParserCombinators.Parsec as P
import Data.Char
import Control.Exception
import Network.CGI.Protocol(maybeRead)

data RMC = RMC {
      time		:: Maybe RMCTime,
      status		:: Maybe Char,
      location		:: RMCLocation,
      speed		:: Maybe Double,
      sDirection	:: Maybe Double,	-- speed direction
      date		:: Maybe RMCDate,			
      mDecl		:: Maybe Double,	-- Magnetic declination
      mDeclDir		:: Maybe Char,		-- "e" or "w"
      modeIndicator	:: Maybe Char
    }deriving Show

--------------------------------------------------------------------------------

data RMCTime 		= RMCTime {
      rmcHours	 	:: Maybe Int,
      rmcMinutes	:: Maybe Int,
      rmcSeconds	:: Maybe Double
    }deriving Show
----------------------------------------
data RMCDate 		= RMCDate {
      day		:: Maybe Int,
      month		:: Maybe Int,
      year		:: Maybe Int
    }deriving Show
----------------------------------------
data RMCLocation 	= RMCLocation {
      latitude 		:: Angle,
      p			:: Maybe Char,
      longitude		:: Angle,
      j			:: Maybe Char
    }deriving Show

data Angle = Angle {
      degree	:: Maybe Int,
      minute	:: Maybe Double
    }deriving Show
----------------------------------------
parseRMC :: String -> Either ParseError (RMC, Int)
parseRMC = parse rmcParser ""

-- every field may be empty(except "$GPRMC,", \13 and checkSum) but must be correct
-- mode indicator may be missed with ',' or without it
rmcParser :: Parser (RMC, Int)
rmcParser = do string "$GPRMC,"
               time		<- timeParser
               char ','
               status		<- many $ oneOf "AV"
               char ','
               location 	<- locationParser
               char ','
               speed		<- float
               char ','
               sDirection	<- float
               char ','
               date		<- dateParser
               char ','
               mDecl		<- float
               char ','
	       mDeclDir		<- many $ oneOf "EW"
               m 		<- do { P.try $ string ",*"; 	return Nothing}
                                      <|> do { char ','
                                             ; m' <- oneOf "ADEN"
                                             ; char '*'
                                             ; return $ Just m'}
                                      <|> do { char '*'; 	return Nothing}
               checkSum		<- 2 `count` hex
               -- EOl must be at the end
               do P.try $ do {cr; lf}	
                  <|> cr
               return $ (RMC time (maybeRead status) location (maybeRead speed)
                         (maybeRead sDirection) date (maybeRead mDecl) (maybeRead mDeclDir) m,
                         read ("0x" ++ checkSum))
                   where cr = char $ chr 0x0d
                         lf = char $ chr 0x0a

hex = oneOf ['A' .. 'F'] <|> oneOf ['a' .. 'v'] <|> digit

-- float = many $ digit <|> char '.'
float :: Parser String
float = do { c  <- P.try (digit <|> char '.')
           ; cs <- float
           ; return $ c:cs}
             <|> return ""
----------------------------------------

timeParser :: Parser (Maybe RMCTime)
timeParser = do { hours 		<- 2 `count` digit
                ; minutes		<- 2 `count` digit
                ; seconds		<- float
                ; return $ Just $ RMCTime (maybeRead hours) (maybeRead minutes) (maybeRead seconds)
                } <|> return Nothing

locationParser :: Parser RMCLocation
locationParser = do latD	<- do 2 `count` digit
                                            <|> return ""
                    latM	<- float
                    char ','
                    p		<- many $ oneOf "NS"
                    char ','
                    lonD	<- do 3 `count` digit
                                            <|> return ""
                    lonM	<- float
                    char ','
                    j		<- many $ oneOf "EW"
                    return $ RMCLocation (Angle (maybeRead latD) (maybeRead latM))
                                         (maybeRead p)
                                         (Angle (maybeRead lonD) (maybeRead lonM))
                                         (maybeRead j)

dateParser :: Parser (Maybe RMCDate)
dateParser = do { day	<- 2 `count` digit
                ; month	<- 2 `count` digit
                ; year	<- 2 `count` digit
                ; return $ Just $ RMCDate (maybeRead day) (maybeRead month) (maybeRead year)
                } <|> return Nothing
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

test :: String -> IO()
test input = case parseRMC input of
               Left _ 			-> putStrLn "error"
               Right (rmc, checkSum)	-> do putStrLn "answer is:"
                                              print rmc

-- parseTest rmcParser "$GPRMC,125504.049,A,5542.2389,N,03741.6063,E,0.06,25.82,200906,,*3B\13"
-- parseTest locationParser "5542.2389,N,03741.6063,E"
-- parseTest rmcParser "$GPRMC,125504.049,,5542.2389,N,03741.6063,E,0.06,25.82,200906,,E*3B\13"
-- parseTest rmcParser "$GPRMC,125504.049,,5542.2389,N,03741.6063,E,0.06,25.82,,,E*3B\13"
-- parseTest rmcParser "$GPRMC,,V,,,,,,,080907,9.6,E,N*31\13"
-- parseTest rmcParser "$GPRMC,,,,,,,,,,,,N*31\13"
-- parseTest rmcParser "$GPRMC,,,,,,,,,,,,*31\13"
-- parseTest rmcParser "$GPRMC,,,,,,,,,,,E,E*31\13"
-- parseTest rmcParser "$GPRMC,,,,,,,,,,,E,*31\13"
-- parseTest rmcParser "$GPRMC,,,,,,,,,,,E*31\13"