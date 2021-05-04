{-# LANGUAGE OverloadedStrings #-}

module Data.I18n.Resource (
	module Data.I18n.Resource,
	Resource,
	fromNonEmpty,
	singleton,
	union,

	Language,
	fromList
) where

import Data.I18n.Resource.Internal

import Prelude hiding (lookup)

import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import GHC.Exts (fromList)
import Network.HTTP.Media (Language, toParts)


lookup :: Language -> Resource a -> Maybe a
lookup lang (Resource res)
	| null parts = Just $ res M.! []
	| otherwise  = go (reverse parts) res where
		parts = toParts lang
		go [] _ = Nothing
		go parts@(_:remain) res =
			M.lookup parts res
				`unionMaybe` go remain res

lenientLookup :: Language -> Resource a -> a
lenientLookup "*" (Resource res) = res M.! []
lenientLookup lang res = fromMaybe wildcard $ lookup lang res
	where wildcard = lenientLookup "*" res

infixl 9 !
(!) :: Resource a -> Language -> a
(!) = flip lenientLookup

infixl 9 !?
(!?) :: Resource a -> Language -> Maybe a
(!?) = flip lookup
