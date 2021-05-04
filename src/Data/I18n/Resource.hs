{-# LANGUAGE OverloadedStrings #-}

module Data.I18n.Resource (
	module Data.I18n.Resource,
	Resource,
	Language
) where

import Data.I18n.Resource.Internal

import Prelude hiding (lookup)

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Network.HTTP.Media (Language, toParts)


fromList :: NonEmpty (Language, a) -> Resource a
fromList list = mainResource list' `union` subResource list' `union` defaultResource where
	list' = NonEmpty.toList list
	defaultResource = singleton "*" . snd $ NonEmpty.last list

singleton :: Language -> a -> Resource a
singleton lang = Resource . M.singleton (toParts lang)

union :: Resource a -> Resource a -> Resource a
union (Resource a) (Resource b) = Resource $ M.union a b

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
