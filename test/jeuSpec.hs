module JeuSpec where

import Test.Hspec

import Environnement
import Batiment
import Unite
import Carte
import qualified Data.Map as M


testsbatiment :: SpecWith ()
testsCollecteCase = describe "produireUnite" $ do
  let coord1 = C {cx = 1, cy = 2}
      coord2 = C {cx = 3, cy = 4}
      coord3 = C {cx = 1, cy = 2}
      coord4 = C {cx= 3, cy=1}
      carte = Carte $ M.fromList [ (coord1, Herbe)
                                 , (coord2, Ressource 5)
                                 , (coord3, Ressource 3)
                                 , (coord4, Eau)
                                 ]
      bat1= Batiment {bid= BatId 1,btype= Usine,bproprio= JoueurId 1,bcoord=coord1,benergie=200,bpointsVie=100,btempsProd=Nothing}
      bat1= Batiment {bid= BatId 2,btype= QuartierGeneral,bproprio= JoueurId 2,bcoord=coord2,benergie=200,bpointsVie=100,btempsProd=Nothing}
      joueur1 = Joueur { jid = JoueurId 1
                        , jcredits = 1000
                        , jbatiments M.fromList [bid bat1,bat1]
                        , junites = M.empty
                        , jenergieConsume =0
                        , jenergieProduit = 0
                        }
      joueur2 = Joueur { jid = JoueurId 2
                        , jcredits = 1000
                        , jbatiments M.fromList [bid bat2,bat2]
                        , junites = M.empty
                        , jenergieConsume =0
                        , jenergieProduit = 0
                        }
      env = Environnement{
                joueurs =[joueur1,joueur2],
                ecarte=carte,
                unites = M.empty ,
                batiments M.fromList [(bid bat1,bat1),(bid bat2,bat2)] 
                }
      env' = produireUnite env Collecteur bat1
  it "return True if the poutput of unite has begin" $
    prop_post_produireUnite env Collecteur bat1 `shouldBe` True 
  it "return False if the output of the usine has begin" $
   prop_post_produireUnite env' Combatant bat1 `shouldBe` False
  it "si les pres condition pour produire une unite sont verifiees il renvoie True" $
    prop_pre_produireUnite env Collecteur bat1 `shouldBe` True
  it "si la productioin de l'unite est lancer il renvoie True" $
    prop_post_produireUnite env Collecteur bat1 `shouldBe` True  
  it "return False if the batiment is not Usine" $
    prop_pre_produireUnite env Collecteur bat2 `shouldBe` False
  it "return True if the output of unite has done" $
    prop_postTerminerProd joueur1 env' bat1 `shouldBe` True
  it "return True if the construc of new Batiment has success" $
    prop_postConstruireBatiment joueur2 env Centrale coord3 `shouldBe` True


    
 jeuSpec = do
  testsbatiment