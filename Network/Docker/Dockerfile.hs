module Network.Docker.Dockerfile where

import Data.Char
import Data.List

type ImgName = String
type ImgTag = String

type LocalPath = FilePath
type ImagePath = FilePath

type Cmd = String
type CmdArg = String

type Pkg = String

type Port = Int

data DockerFileCmd = FROM ImgName ImgTag
                   | ADD LocalPath ImagePath
                   | WORKDIR ImagePath
                   | RUN Cmd [CmdArg] [Cmd]
                   | CMD Cmd [CmdArg]
                   | EXPOSE Port
                   | ENV [(String, String)]
                   deriving (Eq)

instance Show DockerFileCmd where
  show (FROM n t) = unwords ["FROM", n ++":"++ t]
  show (ADD lp ip) = unwords ["ADD",lp,ip]
  show (WORKDIR wd) = unwords ["WORKDIR",show wd]
  show (RUN c ca cpost) = unwords ["RUN",c,unwords ca, intercalate " && " cpost] 
  show (CMD c ca) = unwords ["CMD",c,unwords ca]
  show (EXPOSE p) = unwords ["EXPOSE",show p]
  show (ENV ll) = unwords ["ENV"] 

       

data Distro = DebianLike String  -- e.g. DebianLike "ubuntu"
            | Alpine
            deriving (Eq, Show)

pkgInstall :: Distro -> [Pkg] -> DockerFileCmd
pkgInstall (DebianLike _) ca =
  RUN "apt-get update && apt-get install -y --no-install-recommends" ca ["&& rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*", "apt-get clean", "apt-get purge"]
pkgInstall Alpine ca = RUN "apk-install" ca []
                   
                  

