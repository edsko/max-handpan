module Main where

import Conduit
import Control.Monad.Reader
import Data.Conduit.Zlib (ungzip)
import Text.XML.Stream.Parse

import Ableton.MultiSampleParts
import CmdLine
import LUTs
import Util.Interval.Map qualified as IM
import XML.Parser qualified as P
import XML.TypeDriven

main :: IO ()
main = do
    Options{..} <- getOptions
    mParsed     <- withSourceFile input $ \inp -> runConduit $
                        inp
                     .| ungzip
                     .| parseBytes def
                     .| (transPipe (flip runReaderT []) $ P.runParser parse)
    case mParsed of
      Nothing ->
        putStrLn "XML failed to parse"
      Just parsed ->
        case cmd of
          DumpParsed ->
            print $ parsed
          ShowMSP -> do
            print $ allMSP parsed
          InvertMSP -> do
            let inverted = invertMSP $ allMSP parsed
            print $ IM.toList <$> inverted
            print $ statsMSP  <$> inverted
          SummariseMSP options -> do
            let inverted = invertMSP $ allMSP parsed
            print $ summariseMSP options inverted
          CreateLUTs options ->
            createLUTs options (allMSP parsed)
