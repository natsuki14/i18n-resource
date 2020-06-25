module Data.I18n (
	module Data.I18n.ResourceList,
	Resource (..),
	lookup,
	pickup,
) where

import Prelude hiding (lookup)

import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import qualified Network.HTTP.Media as Media

import Data.I18n.ResourceList
import Data.I18n.Internal

firstJust :: Maybe a -> Maybe a -> Maybe a
firstJust (Just x) _ = Just x
firstJust _ (Just x) = Just x
firstJust _ _ = Nothing

firstJusts :: [Maybe a] -> Maybe a
firstJusts = foldr firstJust Nothing

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (a, b) = (f a, b)

lookup :: Resource a res
	=> res a
	-> ByteString
	-> Maybe a
lookup = Media.mapAcceptLanguage . toList

pickup :: Resource a res
	=> res a
	-> [ByteString]
	-> a
pickup res languages =
	fromMaybe (snd $ head opts) $
		firstJusts $ map (lookup res) languages
	where opts = toList res
