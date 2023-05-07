{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module SpriteMap where

import Data.Map.Strict (Map)
import qualified Data.Map as M

import Sprite (Sprite)
import qualified Sprite as S
import Common

newtype SpriteId = SpriteId String
  deriving (Eq, Ord)

instance Show SpriteId where
  show (SpriteId id) = show id

type SpriteMap = Map SpriteId Sprite

createSpriteMap :: SpriteMap
createSpriteMap = M.empty

addSprite :: SpriteId -> Sprite -> SpriteMap -> SpriteMap
addSprite sid spr tmap =
  M.insertWithKey (\_ _ _ -> error $ "addSprite - Sprite '" <> (show sid) <> "' already in sprite map.")
  sid spr tmap

fetchSprite :: SpriteId -> SpriteMap -> Sprite
fetchSprite sid smap = case M.lookup sid smap of
                         Nothing -> error $ "fetchSprite - No such Sprite: " <> (show sid)
                         Just spr -> spr

updateSprite :: (Sprite -> Sprite) -> SpriteId -> SpriteMap -> SpriteMap
updateSprite f sid smap = M.alter aux sid smap
  where aux Nothing = error $ "updateSprite - No such sprite '" <> (show sid) <> "' in sprite map."
        aux (Just old) = Just $ f old

changeSprite :: SpriteId -> Sprite -> SpriteMap -> SpriteMap
changeSprite sid spr smap = updateSprite (\_ -> spr) sid smap

removeSprite :: SpriteId -> SpriteMap -> SpriteMap
removeSprite sid smap = case M.lookup sid smap of
                          Nothing -> error $ "removeSprite - No such sprite '" <> (show sid) <> "' in sprite map."
                          Just _ -> M.delete sid smap

getSpriteIdForBatiment :: Batiment -> SpriteId
getSpriteIdForBatiment (Batiment {bid = batId, btype = QuartierGeneral}) = SpriteId ("quartier_general_" ++ show batId)
getSpriteIdForBatiment (Batiment {bid = batId, btype = Raffinerie}) = SpriteId ("raffinerie_" ++ show batId)
getSpriteIdForBatiment (Batiment {bid = batId, btype = Usine}) = SpriteId ("usine_" ++ show batId)
getSpriteIdForBatiment (Batiment {bid = batId, btype = Centrale}) = SpriteId ("centrale_" ++ show batId)

getSpriteIdForUnite :: Unite -> SpriteId
getSpriteIdForUnite (Unite {uid = unitId, utype = Collecteur}) = SpriteId ("collecteur_" ++ show unitId)
getSpriteIdForUnite (Unite {uid = unitId, utype = Combatant}) = SpriteId ("combatant_" ++ show unitId)
