module SqlStream.RMCStream(rmcTableMeta, rmcToRow) where

import SqlStream.SqlStream

import qualified RMC.Protobuf.RMC.RMC	as RMC
import qualified RMC.API 		as RMC

--------------------------------------------------------------------------------

rmcTableMeta :: TableMeta
rmcTableMeta = mkTableMeta ["time", 
                            "status",
                            "latitude",
                            "longitude",
                            "speed",
                            "direction",
                            "mDecl",
                            "mDeclDir",
                            "modeInd"
                           ]

rmcToRow :: RMC.RMC -> Row
rmcToRow rmc@(RMC.RMC _ status lati longi speed 
              direction _ mDecl mDeclDir modeInd) = 
      [FDayTime 	(RMC.rmcToUTCTime rmc), 
       FRMCStatus 	status, 
       FDouble 		lati,
       FDouble 		longi,
       FDouble 		speed,
       FDouble 		direction,
       FDouble 		mDecl,
       FMDeclDir	mDeclDir,
       FModeInd		modeInd]
