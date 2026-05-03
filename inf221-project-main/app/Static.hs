{-# LANGUAGE TemplateHaskell #-}
-- Lånt rett fra note-share eksempelet i INF221

-- | 'staticDir' embeds all the files in /static.  This is a separate module for
-- compile time reasons.
module Static (staticDir) where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedDir)

staticDir :: [(FilePath, ByteString)]
staticDir = $(embedDir "static")
