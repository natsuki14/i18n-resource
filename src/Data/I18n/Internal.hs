{-# LANGUAGE MultiParamTypeClasses #-}

module Data.I18n.Internal where

import qualified Network.HTTP.Media as Media

class Resource res a where
	fromList :: [(Media.Language, a)] -> Maybe (res a)
	fromList [] = Nothing
	fromList list = Just . unsafeFromList $ list
	unsafeFromList :: [(Media.Language, a)] -> res a
	toList :: res a -> [(Media.Language, a)]

