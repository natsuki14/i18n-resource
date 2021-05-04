{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit.Unicode
import Data.I18n.Resource

import Data.Functor (void)


resource :: Resource String
resource = [
	("zh-Hant-CN", "歡迎"),
	("zh-Hant-TW", "歡迎"),
	("zh-Hans-CN", "欢迎"),
	("ja-JP", "ようこそ"),
	("en-US", "welcome") ]

tests = TestList [
	resource ! "fr" ~?= "welcome",  -- The last is the default
	resource !? "fr" ~?= Nothing,  -- (!?) should return Nothing if not found
	resource ! "*" ~?= "welcome",  -- Wildcards should return the default
	resource !? "*" ~?= Just "welcome", -- (!?) with wildcard should return Just
	resource ! "zh" ~?= "欢迎",    -- The latter is prior
	resource !? "zh" ~?= Just "欢迎",    -- (!?) with languagetag should return Just with the default value
	resource ! "zh-cn" ~?= "欢迎",
	resource ! "zh-hans" ~?= "欢迎",
	resource ! "zh-hans-cn" ~?= "欢迎",
	resource ! "zh-hant" ~?= "歡迎",
	resource ! "zh-hant-cn" ~?= "歡迎",
	resource ! "zh-tw" ~?= "歡迎",
	resource ! "zh-hant-tw" ~?= "歡迎",
	resource ! "ja-us" ~?= "ようこそ",
	resource ! "en" ~?= "welcome" ]

main :: IO ()
main = void $ runTestTT tests
