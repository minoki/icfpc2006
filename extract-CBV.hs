#!/usr/bin/env cabal
{- cabal:
build-depends: base, bytestring, JuicyPixels ^>=3.3.5
-}
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Codec.Picture

main = do
  content <- BS.readFile "codex.umz"
  let (_, content') = BS.breakSubstring (BS.pack [0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF]) content
      (content'', _) = BS.breakSubstring (BSC.pack "societas_eruditorum") content'
  print (BS.length content'')
  let image :: Image Pixel8
      image = generateImage (\x y -> BS.index content'' (x + 64 * y)) 64 (BS.length content'' `quot` 64)
  savePngImage "CBV.png" (ImageY8 image)
