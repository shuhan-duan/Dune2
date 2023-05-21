{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module CarteSpec where

import Test.Hspec

import Carte
import qualified Data.Map.Strict as M

testCoord = describe "Coord" $ do
    let coord1 = C {cx = 1, cy = 2}
    let coord2 = C {cx = 3, cy = 4}
    let coord3 = C {cx = 1, cy = 2}
    describe "prop_Coord_unique" $ do
      it "returns True if the two coordinates are distinct" $
        prop_Coord_unique coord1 coord2 `shouldBe` True
      it "returns False if the two coordinates are the same" $
        prop_Coord_unique coord1 coord3 `shouldBe` False
    describe "prop_Coord_positive" $ do
      it "returns True if both coordinates are non-negative" $
        prop_Coord_positive coord1 `shouldBe` True
      it "returns False if either coordinate is negative" $
        prop_Coord_positive (C {cx = -1, cy = 2}) `shouldBe` False

testTerrain = describe "prop_terrain" $ do
    it "a Ressource should have a positive integer value" $
      prop_Terrain (Ressource 0) `shouldBe` False
      
    it "an Herbe should have no associated value" $
      prop_Terrain Herbe `shouldBe` True

    it "an Eau should have no associated value" $
      prop_Terrain Eau `shouldBe` True


testsCollecteCase :: SpecWith ()
testsCollecteCase = describe "collecteCase" $ do
  let carte = Carte $ M.fromList [ (C 1 1, Herbe)
                                 , (C 2 1, Ressource 5)
                                 , (C 2 2, Ressource 3)
                                 , (C 3 1, Eau)
                                 ]
  it "ne peut pas extraire plus de ressources que ce qui est présent sur la case" $
    collecteCase (C 2 1) 10 carte `shouldBe` (5, Carte (M.fromList [(C 1 1, Herbe), (C 2 1, Herbe), (C 2 2, Ressource 3), (C 3 1, Eau)]))
  it "extrait toutes les ressources présentes sur la case si elles sont inférieures ou égales à la quantité demandée" $
    collecteCase (C 2 2) 5 carte `shouldBe` (3, Carte (M.fromList [(C 1 1, Herbe), (C 2 1, Ressource 5), (C 2 2, Herbe), (C 3 1, Eau)]))
  it "ne peut pas extraire des ressources sur une case ne contenant pas de gisement de ressources" $
    collecteCase (C 1 1) 1 carte `shouldBe` (0, Carte (M.fromList [(C 1 1, Herbe), (C 2 1, Ressource 5), (C 2 2, Ressource 3), (C 3 1, Eau)]))

carteSpec = do
  testCoord
  testTerrain
  testsCollecteCase