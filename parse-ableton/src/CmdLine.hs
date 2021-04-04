module CmdLine (
    Options(..)
  , OptionsSummarise(..)
  , OptionsCreateLUTs(..)
  , Command(..)
  , getOptions
  ) where

import Options.Applicative

data Options = Options {
      input :: FilePath
    , cmd   :: Command
    }
  deriving (Show)

data Command =
    -- | Dump the parsed XML
    DumpParsed

    -- | Show all multi-sample parts
  | ShowMSP

    -- | Invert the multi-sample parts (from part of the sample to settings)
  | InvertMSP

    -- | Summarise the multi-sample parts
  | SummariseMSP OptionsSummarise

    -- | Create LUTs for use in ks-sampler
  | CreateLUTs OptionsCreateLUTs
  deriving (Show)

data OptionsSummarise = OptionsSummarise {
      -- | Collate overlapping velocity ranges (provided they have disjoint selectors)
      optCollateVelocities :: Bool
    }
  deriving (Show)

data OptionsCreateLUTs = OptionsCreateLUTs {
      -- | Sample rate
      --
      -- Required to able to able convert offsets to @ms@.
      sampleRate :: Double

      -- | Output directory
    , outputDir :: FilePath

      -- | File prefix
    , filePrefix :: FilePath
    }
  deriving (Show)

getOptions :: IO Options
getOptions = execParser opts
  where
    opts = info (parseOptions <**> helper) $ mconcat [
          fullDesc
        , progDesc "Parse Ableton Sampler Instrument Rack data"
        , header "parse-sampler-rack - a parser for Ableton sampler data"
        ]

{-------------------------------------------------------------------------------
  Parsers
-------------------------------------------------------------------------------}

parseOptions :: Parser Options
parseOptions = Options
    <$> argument str (metavar "INPUT")
    <*> parseCommand

parseCommand :: Parser Command
parseCommand = subparser $ mconcat [
      aux "dump-parsed"   (pure DumpParsed)                         "Dump parsed XML"
    , aux "show-msp"      (pure ShowMSP)                            "Show all multi-sample parts"
    , aux "invert-msp"    (pure InvertMSP)                          "Show mapping from sample range to multi-sample settings"
    , aux "summarise-msp" (SummariseMSP <$> parseOptionsSummarise)  "Summarise multi-sample parts"
    , aux "create-luts"   (CreateLUTs   <$> parseOptionsCreateLUTs) "Create LUTs for use in ks-sampler"
    ]
  where
    aux :: String -> Parser Command -> String -> Mod CommandFields Command
    aux c p h = command c $ info (p <**> helper) $ mconcat [
          progDesc h
        ]
parseOptionsSummarise :: Parser OptionsSummarise
parseOptionsSummarise = OptionsSummarise
    <$> (switch $ mconcat [
             long "collate-velocities"
           , help "Collate overlapping velocity ranges (provided they have disjoint selector ranges)"
           ])

parseOptionsCreateLUTs :: Parser OptionsCreateLUTs
parseOptionsCreateLUTs = OptionsCreateLUTs
    <$> (option auto $ mconcat [
             long "sample-rate"
           , help "Sample rate (to convert offsets to ms)"
           , showDefault
           , value 44100
           ])
    <*> (option str $ mconcat [
            long "output-dir"
          , help "Output directory"
          , showDefault
          , value "out"
          ])
    <*> (option str $ mconcat [
            long "file-prefix"
          , help "File prefix"
          , showDefault
          , value ""
          ])
