{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module TextureMap where

import Control.Monad

import Data.Map.Strict (Map)
import qualified Data.Map as M

import SDL
import qualified SDL.Video.Renderer as R
import Common

newtype TextureId = TextureId String
  deriving (Eq, Ord)

instance Show TextureId where
  show (TextureId tid) = show tid

type TextureMap = Map TextureId Texture

-- | Création d'une map de textures vide
createTextureMap :: TextureMap
createTextureMap = M.empty

-- | Ajout d'une texture
addTexture :: TextureId -> Texture -> TextureMap -> TextureMap
addTexture tid = M.insertWithKey (\_ _ _ -> error $ "addTexture - Texture '" <> show tid <> "' already in texture map.")
  tid

-- | Chargement d'une texture à partir d'un fichier
loadTexture  :: Renderer -> FilePath -> TextureId -> TextureMap -> IO TextureMap
loadTexture rdr path tid tmap = do
  srf <- R.loadBMP path
  txt <- createTextureFromSurface rdr srf
  R.freeSurface srf
  pure $ addTexture tid txt tmap

-- | Récupération d'une texture à partir de sa clé
fetchTexture :: TextureId -> TextureMap -> Texture
fetchTexture tid tmap = case M.lookup tid tmap of
                           Nothing -> error $ "fetchTexture - No such texture: " <> show tid
                           Just txt -> txt

-- | Destruction d'une texture (récupération mémoire)
destroyTexture :: TextureId -> TextureMap -> IO TextureMap
destroyTexture tid tmap = case M.lookup tid tmap of
                            Nothing -> error $ "destroyTexture - No such texture : " <> show tid
                            Just txt -> do
                              let tmap' = M.delete tid tmap
                              R.destroyTexture txt
                              pure tmap'


-- | Destruction de la map des textures
destroyTextureMap :: TextureMap -> IO TextureMap
destroyTextureMap tmap =
  let tids = (fst <$> M.toList tmap)
  in do
    forM_ tids (`TextureMap.destroyTexture` tmap)
    pure createTextureMap

lookup :: TextureId -> TextureMap -> Maybe Texture
lookup = M.lookup

getTextureIdForBatiment :: Batiment -> TextureId
getTextureIdForBatiment (Batiment {bid = batId, btype = QuartierGeneral}) = TextureId ("quartier_general_" ++ show batId)
getTextureIdForBatiment (Batiment {bid = batId, btype = Raffinerie}) = TextureId ("raffinerie_" ++ show batId)
getTextureIdForBatiment (Batiment {bid = batId, btype = Usine}) = TextureId ("usine_" ++ show batId)
getTextureIdForBatiment (Batiment {bid = batId, btype = Centrale}) = TextureId ("centrale_" ++ show batId)

getTextureIdForUnite :: Unite -> TextureId
getTextureIdForUnite (Unite {uid = unitId, utype = Collecteur _}) = TextureId ("collecteur_" ++ show unitId)
getTextureIdForUnite (Unite {uid = unitId, utype = Combatant}) = TextureId ("combatant_" ++ show unitId)
