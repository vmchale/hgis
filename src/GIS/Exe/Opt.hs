{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module GIS.Exe.Opt
    ( exec
    , Program (..)
    ) where

import GIS.Hylo
import Control.Lens
import Control.Monad
import Options.Generic hiding (getAll)
import Control.Monad.IO.Class
import Data.Monoid hiding (getAll)
import System.Directory
--
import GIS.Utils
import GIS.Math.Projections
import GIS.Math.Spherical
import GIS.Graphics.PlotSVG
import GIS.Types

-- | projection type? hmm
data Program = Program { file :: FilePath <?> "Path to dbf file"
                       , output :: FilePath <?> "Where to write the output"
                       , generateAll :: Bool <?> "Whether to generate a separate file for each shape"
                       , noComputations :: Bool <?> "Generate map without annotating area, perimeter, etc."
                       -- specify a list of states to allow? idkkkkk
                       -- of course bool for svg/png is a good idea!
                       } deriving (Generic)

instance ParseRecord Program

exec :: IO ()
exec = do
    clinput <- getRecord "GIS cli utility for haskell"
    makeFolders
    let outfile = unHelpful . output $ clinput
    let path = unHelpful . file $ clinput
    mkMapSVG "example.svg" =<< districtToMap <$> getDistricts path -- svg now idk?

makeFolders :: IO ()
makeFolders = do
    createDirectoryIfMissing False "data/districts"
    districts <- getDistricts "data/2016/tl_2016_us_cd115.shp"
    void $ mapM (\d -> makeMapSVG (view districtLabel d) ("data/districts/" <> (view districtLabel d) <> (show $ view perimeter d) <> ".png") (view shape d)) districts
    --void $ mapM (\d -> makeLabelledMapPng (view districtLabel d) ("data/districts/" <> (view districtLabel d) <> (show $ view perimeter d) <> ".png") [((head $ view shape d), (view districtLabel d))]) districts
