{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.I18n.Resource.Internal where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.Functor ((<&>))
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import GHC.Exts (IsList(..))
import GHC.Generics (Generic)
import Network.HTTP.Media (Language(..), toParts, parseAccept)


newtype Resource a = Resource (Map [CI ByteString] a)
	deriving (Eq, Ord)

instance Show a => Show (Resource a) where
	show (Resource res) = "Resource (fromList " ++ show languageMap ++ ")"
		where
			toLanguage :: [CI ByteString] -> Maybe Language
			toLanguage = parseAccept . toByteString
			toByteString []   = "*"
			toByteString list = BS.intercalate "-" $ CI.original <$> reverse list
			languageMap = catMaybes $ mapFstMaybe toLanguage <$> M.toList res

instance IsList (Resource a) where
	type Item (Resource a) = (Language, a)
	fromList = fromNonEmpty . fromList
	toList (Resource res) = undefined

fromNonEmpty :: NonEmpty (Language, a) -> Resource a
fromNonEmpty list = mainResource list' `union` subResource list' `union` defaultResource where
	list' = NonEmpty.toList list
	defaultResource = singleton "*" . snd $ NonEmpty.last list

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (a, b) = (f a, b)

mapFstMaybe :: (a -> Maybe b) -> (a, c) -> Maybe (b, c)
mapFstMaybe f (a, b) = f a <&> \a' -> (a', b)

singleton :: Language -> a -> Resource a
singleton lang = Resource . M.singleton (toParts lang)

union :: Resource a -> Resource a -> Resource a
union (Resource a) (Resource b) = Resource $ M.union a b

subLanguages :: [CI ByteString] -> [[CI ByteString]]
subLanguages langs = reverse <$> go langs where
	go [] = []
	go [lang] = [[lang]]
	go (lang:sublang) = (lang :) <$> properSubsequences sublang

properSubsequences :: [a] -> [[a]]
properSubsequences = dropLast . List.subsequences

dropLast :: [a] -> [a]
dropLast []  = error $ "dropLast: empty list"
dropLast [_] = []
dropLast (x:xs) = x : dropLast xs

mainResource :: [(Language, a)] -> Resource a
mainResource = Resource . M.fromList . fmap (mapFst $ reverse . toParts)

subResource :: [(Language, a)] -> Resource a
subResource list = Resource . M.fromList $ do
	(lang, a) <- list
	sublang <- subLanguages $ toParts lang
	return (sublang, a)

unionMaybe :: Maybe a -> Maybe a -> Maybe a
unionMaybe a@(Just _) _ = a
unionMaybe Nothing    b = b
