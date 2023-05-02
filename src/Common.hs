{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Common where
import Data.Map
import Carte


newtype JoueurId = JoueurId Int deriving (Eq)

newtype UniteId = UniteId Int deriving (Eq)

newtype BatId = BatId Int deriving (Eq)


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
                        }deriving (Eq)

joueurExiste :: JoueurId -> Environnement -> Bool
joueurExiste joueurId env = joueurId `elem` fmap jid (joueurs env)

data BatimentType = QuartierGeneral | Raffinerie | Usine | Centrale  deriving (Eq)

data Batiment = Batiment { bid :: BatId
                            , btype :: BatimentType
                            , bproprio :: JoueurId
                            , bcoord :: Coord
                            , benergie :: Int --La consommation ou la production d'énergie
                            , bpointsVie :: Int
                            , btempsProd :: Maybe (Int, UniteType) -- ( temps restant,unité produite)
                            }deriving (Eq)

data Tank = Tank Int Int
            |EmptyTank Int
            |FullTank Int
                deriving (Show,Eq)

type Entite = Either Batiment Unite

data UniteType = Collecteur Tank
                | Combatant deriving (Eq)

data Ordre = Collecter Coord
            | Deplacer Coord
            |Attaquer Entite
            | Patrouiller Coord Coord deriving (Eq)


data Unite = Unite { uid :: UniteId
                    , utype :: UniteType
                    , uproprio ::JoueurId
                    , uproducteur :: BatId
                    , ucoord :: Coord
                    -- , udirection :: Int  -- angle en degrés, modulo 360
                    , upointsVie :: Int
                    , uTache :: [Ordre]
                    , ubut :: Ordre
                    }deriving (Eq)


data Environnement = Environnement{
                joueurs :: [Joueur],
                ecarte :: Carte,
                unites :: Map UniteId Unite ,
                batiments :: Map BatId Batiment
                }

