module Environment where

import Carte
import qualified Data.Map.Strict as M
import Data.Map (Map)

newtype JoueurId = JoueurId Int

data Joueur = Joueur { jid :: JoueurId
                     , jcredits :: Int
                     , jcentrales :: [BatId]
                     , jraffineries :: [BatId]
                     , jusines :: [BatId]
                     }

newtype BatId = BatId Int

data BatimentType = QuartierGeneral | Raffinerie | Usine | Centrale

data Batiment = Batiment { bid :: BatId
                         , btype :: BatimentType
                         , bproprio :: JoueurId
                         , bcoord :: Coord
                         , benergie :: Int
                         , bpointsVie :: Int
                         , btempsProd :: Maybe (Int, UniteType)
                         }

newtype UniteId = UniteId Int

data UniteType = Tank | Soldat | Trike

data Unite = Unite { uid :: UniteId
                   , utype :: UniteType
                   , uproducteur :: BatId
                   , uproprio :: JoueurId
                   , ucoord :: Coord
                   , udirection :: Int  -- angle en degrés, modulo 360
                   , upointsVie :: Int
                   }

data Environnement = Environnement{
                joueurs :: [Joueur],
                ecarte :: Carte,
                unites :: Map UniteId Unite ,
                batiments :: Map BatId Batiment
                }


