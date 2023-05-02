{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Common where
import Data.Map
import Carte


newtype JoueurId = JoueurId Int deriving (Eq)

newtype UniteId = UniteId Int 

newtype BatId = BatId Int

-- get BatId from newtype BatId
unBatId :: BatId -> Int
unBatId (BatId n) = n

-- get Uid from UniteId
unUniteId :: UniteId -> Int
unUniteId (UniteId n) = n

data Joueur = Joueur { jid :: JoueurId
                        , jcredits :: Int
                        , jbatiments :: Map BatId Batiment
                        , junites :: Map UniteId Unite
                        , jenergieConsume :: Int
                        , jenergieProduit :: Int
                        }

data BatimentType = QuartierGeneral | Raffinerie | Usine | Centrale  deriving (Eq)

data Batiment = Batiment { bid :: BatId
                            , btype :: BatimentType
                            , bproprio :: JoueurId
                            , bcoord :: Coord
                            , benergie :: Int --La consommation ou la production d'énergie
                            , bpointsVie :: Int 
                            , btempsProd :: Maybe (Int, UniteType) -- ( temps restant,unité produite)
                            }

data Tank = Tank Int Int
            |EmptyTank Int
            |FullTank Int
                deriving Show
data Entite = Batiment 
            |Unite

data UniteType = Collecteur Tank 
                | Combatant   
                    
data Ordre = Collecter Coord
            | Deplacer Coord
            |Attaquer Entite
            | Patrouiller Coord Coord

                
data Unite = Unite { uid :: UniteId
                    , utype :: UniteType
                    , uproprio ::JoueurId
                    , uproducteur :: BatId
                    , ucoord :: Coord
                    , udirection :: Int  -- angle en degrés, modulo 360
                    , upointsVie :: Int
                    , uTache :: [Ordre]
                    , ubut :: Ordre
                    }


data Environnement = Environnement{
                joueurs :: [Joueur],
                ecarte :: Carte,
                unites :: Map UniteId Unite ,
                batiments :: Map BatId Batiment
                }