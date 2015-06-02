-- |
-- Module      :  Yi.Style.Solarized
-- Copyright   :  (c) Tom Sydney Kerckhove 2015
-- License     :  MIT
--
-- Maintainer  :  syd.kerckhove@gmail.com
-- Stability   :  experimental
-- Portability :  portable

module Yi.Style.Solarized ( solarizedTheme ) where

import           Data.Bits   (shiftR)
import           Data.Monoid (mappend, mempty)
import           Data.Word   (Word32)
import           Yi

-- | The main theme specification. An example use in your @yi.hs@ might
-- look something like
--
-- @
-- main :: IO ()
-- main = yi $ myConfig {
--   defaultKm = defaultKm myConfig
--   , startFrontEnd = start
--   , configUI = (configUI defaultConfig) { configTheme = solarizedTheme }
--   }
-- @
solarizedTheme :: Proto UIStyle
solarizedTheme = defaultTheme `override` \sets _ -> sets
  { baseAttributes     = emptyAttributes { foreground = solarizedBase1
                                         , background = solarizedBase03
                                         , reverseAttr = False }
  , modelineAttributes = emptyAttributes { foreground = solarizedBase1
                                         , background = solarizedBase03
                                         , reverseAttr = False }
  , tabBarAttributes   = emptyAttributes { foreground = solarizedBase1
                                         , background = solarizedBase03
                                         , reverseAttr = False }
  , tabInFocusStyle    = withFg solarizedBase1 `mappend` withBg solarizedBase03
  , selectedStyle      = withBg solarizedBase03
  , hintStyle          = withBg solarizedBase03
  , builtinStyle       = withFg solarizedCyan
  , commentStyle       = withFg solarizedBase01
  , keywordStyle       = withFg solarizedRed
  , stringStyle        = withFg solarizedRed
  , typeStyle          = withFg solarizedYellow
  , errorStyle         = withFg solarizedRed `mappend` withBd True

  , operatorStyle      = withFg solarizedBlue

  , importStyle        = withFg solarizedGreen
  , numberStyle        = withFg solarizedGreen
  , preprocessorStyle  = withFg solarizedBase3

  }


-- | Convenience function
rgb :: Word32 -> Color
rgb x = RGB (fi (x `shiftR` 16))
            (fi (x `shiftR` 8))
            (fi x)
  where
    fi = fromIntegral


-- | Hex value: 0x002B36
solarizedBase03 :: Color
solarizedBase03 = rgb 0x000000

-- | Hex value: 0x073642
solarizedBase02 :: Color
solarizedBase02 = rgb 0x073642

-- | Hex value: 0x586E75
solarizedBase01 :: Color
solarizedBase01 = rgb 0x586E75

-- | Hex value: 0x657B83
solarizedBase00 :: Color
solarizedBase00 = rgb 0x657B83

-- | Hex value: 0x839496
solarizedBase0 :: Color
solarizedBase0 = rgb 0x839496

-- | Hex value: 0x93A1A1
solarizedBase1 :: Color
solarizedBase1 = rgb 0x93A1A1

-- | Hex value: 0xEEE8d5
solarizedBase2 :: Color
solarizedBase2 = rgb 0xEEE8d5

-- | Hex value: 0xFDF6e3
solarizedBase3 :: Color
solarizedBase3 = rgb 0xFDF6e3

-- | Hex value: 0xB58900
solarizedYellow :: Color
solarizedYellow = rgb 0xB58900

-- | Hex value: 0xCB4B16
solarizedOrange :: Color
solarizedOrange = rgb 0xCB4B16

-- | Hex value: 0xDC322F
solarizedRed :: Color
solarizedRed = rgb 0xDC322F

-- | Hex value: 0xD33682
solarizedMagenta :: Color
solarizedMagenta = rgb 0xD33682

-- | Hex value: 0x6C71C4
solarizedViolet :: Color
solarizedViolet = rgb 0x6C71C4

-- | Hex value: 0x268BD2
solarizedBlue :: Color
solarizedBlue = rgb 0x268BD2

-- | Hex value: 0x2AA198
solarizedCyan :: Color
solarizedCyan = rgb 0x2AA198

-- | Hex value: 0x859900
solarizedGreen :: Color
solarizedGreen = rgb 0x859900

