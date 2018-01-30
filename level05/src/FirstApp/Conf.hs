{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module FirstApp.Conf
    ( parseOptions
    ) where

import           GHC.Word                  (Word16)

import           Data.Bifunctor            (first)
import           Data.Monoid               ((<>), Last (Last))

import           FirstApp.Types            (Conf (Conf), ConfigError (..),
                                            DBFilePath (DBFilePath),
                                            PartialConf (PartialConf), Port (Port))

import           FirstApp.Conf.CommandLine (commandLineParser)
import           FirstApp.Conf.File        (parseJSONConfigFile)

-- For the purposes of this application we will encode some default values to
-- ensure that our application continues to function in the event of missing
-- configuration values from either the file or command line inputs.
defaultConf
  :: PartialConf
defaultConf = PartialConf (Last $ Just $ Port 3000) (Last $ Just $ DBFilePath "db.lite")

-- We need something that will take our PartialConf and see if can finally build
-- a complete ``Conf`` record. Also we need to highlight any missing values by
-- providing the relevant error.
makeConfig
  :: PartialConf
  -> Either ConfigError Conf
makeConfig (PartialConf (Last Nothing) _) = Left ConfigMissingFieldsError
makeConfig (PartialConf _ (Last Nothing)) = Left ConfigMissingFieldsError
makeConfig (PartialConf (Last (Just port)) (Last (Just filePath))) = Right $ Conf port filePath

-- This is the function we'll actually export for building our configuration.
-- Since it wraps all our efforts to read information from the command line, and
-- the file, before combining it all and returning the required information.

-- Remember that we want the command line configuration to take precedence over
-- the File configuration, so if we think about combining each of our ``Conf``
-- records. By now we should be able to write something like this:
--
-- ``defaults <> file <> commandLine``
--
parseOptions
  :: FilePath
  -> IO (Either ConfigError Conf)
parseOptions filePath =
  -- Parse the options from the config file: "appconfig.json"
  -- Parse the options from the commandline using 'commandLineParser'
  -- Combine these with the default configuration 'defaultConf'
  -- Return the final configuration value
  do
    commandLineConf <- commandLineParser
    configResult <- parseJSONConfigFile filePath
    pure $ configResult >>= \fileConf -> makeConfig $ defaultConf <> fileConf <> commandLineConf
