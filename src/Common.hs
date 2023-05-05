{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Common where
import Data.Map
import Carte


newtype JoueurId = JoueurId Int deriving (Show, Eq ,Ord)

newtype UniteId = UniteId Int deriving (Show, Eq ,Ord)
-- get Uid from UniteId
unUniteId :: UniteId -> Int
unUniteId (UniteId n) = n

newtype BatId = BatId Int deriving (Show, Eq ,Ord)

-- get BatId from newtype BatId
unBatId :: BatId -> Int
unBatId (BatId n) = n

data Joueur = Joueur { jid :: JoueurId
                        , jcredits :: Int
                        , jbatiments :: Map BatId Batiment
                        , junites :: Map UniteId Unite
                        , jenergieConsume :: Int
                        , jenergieProduit :: Int
                        }deriving (Show, Eq)


data BatimentType = QuartierGeneral | Raffinerie | Usine | Centrale  deriving (Show, Eq)

data Batiment = Batiment { bid :: BatId
                            , btype :: BatimentType
                            , bproprio :: JoueurId
                            , bcoord :: Coord
                            , benergie :: Int --La consommation ou la production d'énergie
                            , bpointsVie :: Int
                            , btempsProd :: Maybe (Int, UniteType) -- ( temps restant,unité produite)
                            }deriving (Show, Eq)



type Entite = Either Batiment Unite

type EntiteId = Either BatId UniteId

data Direction = Nord | Est | Sud | Ouest
  deriving (Show, Eq, Enum)

data Ordre = Collecter Coord
            | Deplacer Coord
            | Attaquer Entite
            | Patrouiller Coord Coord deriving (Show, Eq)

data Tank = Tank Int Int
            |EmptyTank Int
            |FullTank Int
                deriving (Show,Eq)

data UniteType = Collecteur Tank
                | Combatant deriving (Show, Eq)

data Unite = Unite {
    uid :: UniteId,
    utype :: UniteType,
    uproprio :: JoueurId,
    ucoord :: Coord,
    udirection :: Direction,
    upointsVie :: Int,
    ucuve :: Maybe Tank,
    uordres :: [Ordre],
    ubut :: Ordre
} deriving (Show, Eq)


data Environnement = Environnement{
                joueurs :: [Joueur],
                ecarte :: Carte,
                unites :: Map UniteId Unite ,
                batiments :: Map BatId Batiment
                }

entiteCoord :: Entite -> Coord
entiteCoord (Left batiment) = bcoord batiment
entiteCoord (Right unite) = ucoord unite

data Commande = DonnerOrdre UniteId Ordre
              | Construire JoueurId BatimentType Coord
              | Produire JoueurId UniteType
              deriving (Show, Eq)
