module Propellor.Locale where

import           Propellor
import           Propellor.Property.Apt  as Apt
import qualified Propellor.Property.File as File

data Lang = En
          | Fr

instance Show Lang where
  show En = "en"
  show Fr = "fr"

data Country = US
             | FR
             deriving (Show)


data Encoding = UTF_8

instance Show Encoding where
  show UTF_8 = "UTF-8"

data Locale = SimpleLocale Lang Country
            | EncodingLocale Lang Country Encoding

instance Show Locale where
  show (SimpleLocale l c)     = show l ++ "_" ++ show c
  show (EncodingLocale l c e) = show l ++ "_" ++ show c ++ "."++ show e

en_us_UTF_8 :: Locale
en_us_UTF_8 = EncodingLocale En US UTF_8

setDefaultLocale :: Locale -> Property DebianLike
setDefaultLocale locale =
  propertyList ("setting default locale to " ++ localeString) $ props 
  & Apt.installed ["locales"]
  & scriptProperty [ "locale-gen " ++ localeString ] `assume` MadeChange
  & "/etc/default/locale" `File.hasContent` [
  "LC_ALL=" ++ localeString
  ,"LANG=" ++  localeString
  ]
  where
    localeString = show locale
