module CmdArgs (CmdArgs (..), parseCmdArgs)
where

import Control.Applicative ((<|>))
import Data.Semigroup ((<>))
import Options.Applicative (
  execParser,
  flag',
  fullDesc,
  header,
  help,
  helper,
  InfoMod,
  info,
  long,
  metavar,
  progDesc,
  short,
  strOption)
import qualified Options.Applicative as O (Parser)

data CmdArgs = PrintDNSs | UpdateFile FilePath

resolvConfLocationArg :: O.Parser CmdArgs
resolvConfLocationArg = fmap UpdateFile . strOption $
       long "update"
    <> metavar "FILE"
    <> help ("If specified, prepends obtained DNS servers to a file, " ++
             "preserving only three topmost \"nameserver\" entries")

updateArg :: O.Parser CmdArgs
updateArg = flag' (UpdateFile "/etc/resolv.conf") $
  short 'u' <> help "Update \"/etc/resolv.conf\""

cmdArgs :: O.Parser CmdArgs
cmdArgs = resolvConfLocationArg <|> updateArg <|> pure PrintDNSs

infoMod :: InfoMod a
infoMod = fullDesc <> header "DHCP DNS Query" <> progDesc "Query DHCP for DNS servers"

parseCmdArgs :: IO CmdArgs
parseCmdArgs = execParser $ info (helper <*> cmdArgs) infoMod

