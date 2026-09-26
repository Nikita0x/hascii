module Hascii.ASCII where

import           Codec.Picture

data PixelInfo = PixelInfo {
    pixel           :: PixelRGB8,
    brightnessValue :: Int
}


brightness :: PixelRGB8 -> PixelInfo
brightness (PixelRGB8 r g b) =
    let brightness = round
            ( 0.299 * fromIntegral r
                + 0.587 * fromIntegral g
                + 0.114 * fromIntegral b
            )
    in PixelInfo
        { pixel = PixelRGB8 r g b
        , brightnessValue = brightness
        }


-- | Converts a brightness value into an ASCII character.
toChar :: Int -> Char
toChar brightness
    | brightness < 30 = '@'
    | brightness < 60 = '#'
    | brightness < 90 = '*'
    | brightness < 120 = '+'
    | brightness < 150 = '-'
    | brightness < 180 = ':'
    | brightness < 210 = '.'
    | otherwise = ' '


type Glyph = [String]
type ColoredGlyph = [
        [(Char, PixelRGB8)]
    ]

glyph :: Char -> Glyph
glyph '@' =
    [ "  ####  "
    , " ##  ## "
    , "##    ##"
    , "## ## ##"
    , "##    ##"
    , "##  ####"
    , " ##     "
    , "  ##### "
    ]

glyph '#' =
    [ " ##  ## "
    , " ##  ## "
    , "########"
    , "########"
    , " ##  ## "
    , " ##  ## "
    , "########"
    , "########"
    ]

glyph '*' =
    [ "   ##   "
    , "## ## ##"
    , " #####  "
    , "  ##### "
    , " #####  "
    , "## ## ##"
    , "   ##   "
    , "        "
    ]

glyph '+' =
    [ "   ##   "
    , "   ##   "
    , "   ##   "
    , "########"
    , "########"
    , "   ##   "
    , "   ##   "
    , "   ##   "
    ]

glyph '-' =
    [ "        "
    , "        "
    , "        "
    , "########"
    , "########"
    , "        "
    , "        "
    , "        "
    ]

glyph ':' =
    [ "        "
    , "   ##   "
    , "   ##   "
    , "        "
    , "        "
    , "   ##   "
    , "   ##   "
    , "        "
    ]

glyph '.' =
    [ "        "
    , "        "
    , "        "
    , "        "
    , "        "
    , "   ##   "
    , "   ##   "
    , "        "
    ]

glyph ' ' =
    [ "        "
    , "        "
    , "        "
    , "        "
    , "        "
    , "        "
    , "        "
    , "        "
    ]

glyph _ = []
