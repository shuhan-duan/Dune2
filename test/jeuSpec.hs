module JeuSpec where

import Test.Hspec

import Environnement
import Batiment
import Unite
import Carte
import qualified Data.Map as M
import Common
import Batiment


jeuSpec = do
  describe "produireUnite" $ do
    let coord1 = C {cx = 1, cy = 2}
        coord2 = C {cx = 3, cy = 4}
        coord3 = C {cx = 1, cy = 2}
        coord4 = C {cx = 3, cy = 1}
        carte = Carte $ M.fromList [ (coord1, Herbe)
                                   , (coord2, Ressource 5)
                                   , (coord3, Ressource 3)
                                   , (coord4, Eau)
                                   ]
        bat1 = Batiment {bid = BatId 1, btype = Usine, bproprio = JoueurId 1, bcoord = coord1, benergie = 200, bpointsVie = 100, btempsProd = Nothing}
        bat2 = Batiment {bid = BatId 2, btype = QuartierGeneral, bproprio = JoueurId 2, bcoord = coord2, benergie = 200, bpointsVie = 100, btempsProd = Nothing}
        joueur1 = Joueur { jid = JoueurId 1
                         , jcredits = 1000
                         , jbatiments = M.fromList [(bid bat1, bat1)]
                         , junites = M.empty
                         , jenergieConsume = 0
                         , jenergieProduit = 0
                         }
        joueur2 = Joueur { jid = JoueurId 2
                         , jcredits = 1000
                         , jbatiments = M.fromList [(bid bat2, bat2)]
                         , junites = M.empty
                         , jenergieConsume = 0
                         , jenergieProduit = 0
                         }
        env = Environnement { joueurs = [joueur1, joueur2]
                            , ecarte = carte
                            , unites = M.empty
                            , batiments = M.fromList [(bid bat1, bat1), (bid bat2, bat2)]
                            }
        env' = produireUnite env Collecteur bat1
    it "return True if the output of usine has begin" $ do
      prop_post_produireUnite env Collecteur bat1 `shouldBe` True
    it "return False if the output of the usine has begin" $ do
      prop_post_produireUnite env' Combatant bat1 `shouldBe` False
    it "return False if the batiment is not Usine" $ do
      prop_pre_produireUnite env Collecteur bat2 `shouldBe` False
    it "return True if the output of unite has done" $ do
      prop_postTerminerProd joueur1 env' bat1 `shouldBe` True
