-- | This module contains the parser for the command-line options
module GIS.Exe.Parser where

import Options.Applicative
import GIS.Types
import GIS.Math.Projections
import Control.Lens hiding (argument)
import Data.Monoid

-- | Data type for one run of the program
data Program = Program { com :: Command 
                       , file :: FilePath -- "Path to dbf file"
                       } 

-- | Data type for the appropriate subcommand
data Command = Computation { computation :: String , outputM :: Maybe FilePath } 
    | MapMaker { output :: FilePath , generateAll :: Bool, projection :: Maybe String }
    -- also a "map and label command?"

-- | Parses the `Program` data type
program :: Parser Program
program = Program 
    <$> hsubparser
        ( command "compute" (info computationP ( progDesc "Compute perimeter, area, etc. of map" ))
        <> command "map" (info mapMaker ( progDesc "Make a map from a shapefile database." )))
    <*> ( argument str ( metavar "SHAPEFILE" <> help "Path to .shp file" ) )

-- | Parses the `Command` datatype into a Computation
computationP :: Parser Command
computationP = Computation
    <$> argument str
        ( metavar "COMPUTATION"
        <> help "which computation to perform, e.g. area, perimeter, etc." )
    <*> ( optional $ strOption  -- because it's a maybe filepath
        ( long "output"
        <> short 'o'
        <> metavar "OUTPUT"
        <> help "Where to write output" ) )

-- | Parses the `Command` datatype into a map
mapMaker :: Parser Command
mapMaker = MapMaker
    <$> strOption
        (long "output"
        <> short 'o'
        <> metavar "OUTPUT"
        <> help "Where to write the image/map" )
    <*> switch
        (long "generate-all"
        <> short 'a'
        <> help "Whether to generate a separate file for each object in the shapefile" )
    <*> ( optional $ strOption
        (long "projection"
        <> short 'p'
        <> help "Which projection to use, e.g. mercator etc.") )

-- | display help if user enters e.g.
--
-- > hgis --help wrongcommand
helpDisplay = info ( program <**> helper)
    ( fullDesc
    <> progDesc "GIS for Haskell. Can make maps, compute areas/perimeters for shapefiles."
    <> header "hgis - GIS in Haskell" )

{--
pickComputation :: String -> District -> [Double]
pickComputation str dist = case str of
    "perimeter" -> pure $ view perimeter dist
    "area" -> view area dist
--}

-- | Parse a `Maybe String` into the appropriate `Projection`, doing nothing for
-- a `Nothing`
pickProjection :: Maybe String -> Projection
pickProjection str = case str of
    Just "mercator" -> mercator
    Just "bonne" -> bonne
    Just "albers" -> albers washingtonDC
    Nothing -> id
