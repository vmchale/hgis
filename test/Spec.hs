import Test.Hspec
import Math.Spherical
import Graphics.PlotPNG
import Graphics.PlotSVG

main :: IO ()
main = hspec $ do
    describe "distance" $ do
        parallel $ it "computes distances along geodesics" $ do
            distance ((-74.0059), 40.7128) ((-118.2347), 34.0522) `shouldBe` 3934.997673163554
    describe "mercatorFullPng" $ do
        parallel $ it "Reads a map and writes a .png with the mercator projection" $ do
            mercatorFullPng "World Map - Mercator Projection" "test/testmap.png" "test/data/worldmap/TM_WORLD_BORDERS-0.3.shp" >>= (`shouldBe` ())
    describe "mercatorFullSVG" $ do
        parallel $ it "Reads a map and writes a .svg with the mercator projection" $ do
            mercatorFullSVG "World Map - Mercator Projection" "test/testmap.svg" "test/data/worldmap/TM_WORLD_BORDERS-0.3.shp" >>= (`shouldBe` ())
