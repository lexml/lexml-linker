module LexML.MD5 where

import qualified Crypto.Hash.MD5 as M
import qualified Data.ByteString.UTF8 as U


md5sum = U.toString . M.hash
