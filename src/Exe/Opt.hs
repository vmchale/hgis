{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Exe.Opt
    ( exec
    , Program (..)
    ) where

import Compute
import Control.Lens
import Control.Monad
import Options.Generic hiding (getAll)
import Control.Monad.IO.Class
import Data.Monoid hiding (getAll)
import System.Directory
--
import Utils
import Math.Projections
import Math.Spherical
import Graphics.PlotPNG
import Graphics.PlotSVG
import Types

-- | projection type? hmm
data Program = Program { file :: Maybe FilePath <?> "Path to dbf file"
                       , output :: Maybe FilePath <?> "Where to write the output"
                       -- specify a list of states to allow? idkkkkk
                       -- of course bool for svg/png is a good idea!
                       } deriving (Generic)

instance ParseRecord Program

exec :: IO ()
exec = do
    clinput <- getRecord "GIS cli utility for haskell"
    makeFolders
    let outfile = unHelpful . output $ clinput
    case outfile of
        (Just file) -> makeMapPng "amerikey" file =<< getAll -- svg now idk?
        Nothing -> makeMapSVG {--Png--} "amerikey" "example.svg" {--.svg--} =<< getAll

makeFolders :: IO ()
makeFolders = do
    createDirectoryIfMissing False "data/districts"
    districts <- getDistricts "data/2016/tl_2016_us_cd115.shp"
    --void $ mapM (\d -> makeMapPng (view districtLabel d) ("data/districts/" <> (view districtLabel d) <> (show $ view perimeter d) <> ".png") (view shape d)) districts
    void $ mapM (\d -> makeLabelledMapPng (view districtLabel d) ("data/districts/" <> (view districtLabel d) <> (show $ view perimeter d) <> ".png") [((head $ view shape d), (view districtLabel d))]) districts
