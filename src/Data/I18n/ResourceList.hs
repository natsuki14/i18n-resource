{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.I18n.ResourceList where

import Data.Aeson (ToJSON (..))
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import Data.Text.Encoding (decodeUtf8)
import qualified Network.HTTP.Media as Media

import Data.I18n.Internal

newtype ResourceList a = ResourceList {
	unI18nResource :: [(Media.Language, a)]
}

instance (Resource a) ResourceList where
	unsafeFromList = ResourceList
	toList         = unI18nResource

instance ToJSON a => ToJSON (ResourceList a) where
	-- Media.renderHeader guarantees that the return value is UTF-8 encoded.
	toJSON res = Aeson.Object . HashMap.fromList . map mkObj $ opts
		where
			opts = toList res
			mkObj (lang, value) = (decodeUtf8 . Media.renderHeader $ lang, toJSON value)
