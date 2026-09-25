module Hascii.ASCII where

import           Codec.Picture


-- | Converts an RGB pixel to a perceived brightness value from 0 to 255.
brightness :: PixelRGB8 -> Int
brightness (PixelRGB8 r g b) =
    round
        ( 0.299 * fromIntegral r
            + 0.587 * fromIntegral g
            + 0.114 * fromIntegral b
        )

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
