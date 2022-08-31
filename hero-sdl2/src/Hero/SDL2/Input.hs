{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}

module Hero.SDL2.Input
  ( addSDLEvents,
    KeyboardState,
    getKeyboardState,
    isPressed,
    SDLEvents,
    KeyboardEvent (..),
    getKeyboardEvents,
    SDLCodes.Scancode
      ( ..,
        ScancodeUnknown,
        ScancodeA,
        ScancodeB,
        ScancodeC,
        ScancodeD,
        ScancodeE,
        ScancodeF,
        ScancodeG,
        ScancodeH,
        ScancodeI,
        ScancodeJ,
        ScancodeK,
        ScancodeL,
        ScancodeM,
        ScancodeN,
        ScancodeO,
        ScancodeP,
        ScancodeQ,
        ScancodeR,
        ScancodeS,
        ScancodeT,
        ScancodeU,
        ScancodeV,
        ScancodeW,
        ScancodeX,
        ScancodeY,
        ScancodeZ,
        Scancode1,
        Scancode2,
        Scancode3,
        Scancode4,
        Scancode5,
        Scancode6,
        Scancode7,
        Scancode8,
        Scancode9,
        Scancode0,
        ScancodeReturn,
        ScancodeEscape,
        ScancodeBackspace,
        ScancodeTab,
        ScancodeSpace,
        ScancodeMinus,
        ScancodeEquals,
        ScancodeLeftBracket,
        ScancodeRightBracket,
        ScancodeBackslash,
        ScancodeNonUSHash,
        ScancodeSemicolon,
        ScancodeApostrophe,
        ScancodeGrave,
        ScancodeComma,
        ScancodePeriod,
        ScancodeSlash,
        ScancodeCapsLock,
        ScancodeF1,
        ScancodeF2,
        ScancodeF3,
        ScancodeF4,
        ScancodeF5,
        ScancodeF6,
        ScancodeF7,
        ScancodeF8,
        ScancodeF9,
        ScancodeF10,
        ScancodeF11,
        ScancodeF12,
        ScancodePrintScreen,
        ScancodeScrollLock,
        ScancodePause,
        ScancodeInsert,
        ScancodeHome,
        ScancodePageUp,
        ScancodeDelete,
        ScancodeEnd,
        ScancodePageDown,
        ScancodeRight,
        ScancodeLeft,
        ScancodeDown,
        ScancodeUp,
        ScancodeNumLockClear,
        ScancodeKPDivide,
        ScancodeKPMultiply,
        ScancodeKPMinus,
        ScancodeKPPlus,
        ScancodeKPEnter,
        ScancodeKP1,
        ScancodeKP2,
        ScancodeKP3,
        ScancodeKP4,
        ScancodeKP5,
        ScancodeKP6,
        ScancodeKP7,
        ScancodeKP8,
        ScancodeKP9,
        ScancodeKP0,
        ScancodeKPPeriod,
        ScancodeNonUSBackslash,
        ScancodeApplication,
        ScancodePower,
        ScancodeKPEquals,
        ScancodeF13,
        ScancodeF14,
        ScancodeF15,
        ScancodeF16,
        ScancodeF17,
        ScancodeF18,
        ScancodeF19,
        ScancodeF20,
        ScancodeF21,
        ScancodeF22,
        ScancodeF23,
        ScancodeF24,
        ScancodeExecute,
        ScancodeHelp,
        ScancodeMenu,
        ScancodeSelect,
        ScancodeStop,
        ScancodeAgain,
        ScancodeUndo,
        ScancodeCut,
        ScancodeCopy,
        ScancodePaste,
        ScancodeFind,
        ScancodeMute,
        ScancodeVolumeUp,
        ScancodeVolumeDown,
        ScancodeKPComma,
        ScancodeKPEqualsAS400,
        ScancodeInternational1,
        ScancodeInternational2,
        ScancodeInternational3,
        ScancodeInternational4,
        ScancodeInternational5,
        ScancodeInternational6,
        ScancodeInternational7,
        ScancodeInternational8,
        ScancodeInternational9,
        ScancodeLang1,
        ScancodeLang2,
        ScancodeLang3,
        ScancodeLang4,
        ScancodeLang5,
        ScancodeLang6,
        ScancodeLang7,
        ScancodeLang8,
        ScancodeLang9,
        ScancodeAltErase,
        ScancodeSysReq,
        ScancodeCancel,
        ScancodeClear,
        ScancodePrior,
        ScancodeReturn2,
        ScancodeSeparator,
        ScancodeOut,
        ScancodeOper,
        ScancodeClearAgain,
        ScancodeCrSel,
        ScancodeExSel,
        ScancodeKP00,
        ScancodeKP000,
        ScancodeThousandsSeparator,
        ScancodeDecimalSeparator,
        ScancodeCurrencyUnit,
        ScancodeCurrencySubunit,
        ScancodeLeftParen,
        ScancodeRightParen,
        ScancodeLeftBrace,
        ScancodeRightBrace,
        ScancodeKPTab,
        ScancodeKPBackspace,
        ScancodeKPA,
        ScancodeKPB,
        ScancodeKPC,
        ScancodeKPD,
        ScancodeKPE,
        ScancodeKPF,
        ScancodeKPXOR,
        ScancodeKPPower,
        ScancodeKPPercent,
        ScancodeKPLess,
        ScancodeKPGreater,
        ScancodeKPAmpersand,
        ScancodeKPDblAmpersand,
        ScancodeKPVerticalBar,
        ScancodeKPDblVerticalBar,
        ScancodeKPColon,
        ScancodeKPHash,
        ScancodeKPSpace,
        ScancodeKPAt,
        ScancodeKPExclam,
        ScancodeKPMemStore,
        ScancodeKPMemRecall,
        ScancodeKPMemClear,
        ScancodeKPMemAdd,
        ScancodeKPMemSubtract,
        ScancodeKPMemMultiply,
        ScancodeKPMemDivide,
        ScancodeKPPlusMinus,
        ScancodeKPClear,
        ScancodeKPClearEntry,
        ScancodeKPBinary,
        ScancodeKPOctal,
        ScancodeKPDecimal,
        ScancodeKPHexadecimal,
        ScancodeLCtrl,
        ScancodeLShift,
        ScancodeLAlt,
        ScancodeLGUI,
        ScancodeRCtrl,
        ScancodeRShift,
        ScancodeRAlt,
        ScancodeRGUI,
        ScancodeMode,
        ScancodeAudioNext,
        ScancodeAudioPrev,
        ScancodeAudioStop,
        ScancodeAudioPlay,
        ScancodeAudioMute,
        ScancodeMediaSelect,
        ScancodeWWW,
        ScancodeMail,
        ScancodeCalculator,
        ScancodeComputer,
        ScancodeACSearch,
        ScancodeACHome,
        ScancodeACBack,
        ScancodeACForward,
        ScancodeACStop,
        ScancodeACRefresh,
        ScancodeACBookmarks,
        ScancodeBrightnessDown,
        ScancodeBrightnessUp,
        ScancodeDisplaySwitch,
        ScancodeKBDIllumToggle,
        ScancodeKBDIllumDown,
        ScancodeKBDIllumUp,
        ScancodeEject,
        ScancodeSleep,
        ScancodeApp1,
        ScancodeApp2
      ),
  )
where

import Control.Monad.IO.Class (MonadIO)
import Data.Maybe (catMaybes)
import GHC.Generics (Generic)
import Hero
  ( Component (Store),
    Global,
    System,
    addGlobal,
    getGlobal,
    liftSystem,
    putGlobal,
    withSetup,
    (>>>),
  )
import Optics.Core (view)
import SDL.Event qualified as SDL
import SDL.Init qualified as SDL
import SDL.Input.Keyboard qualified as SDL
import SDL.Input.Keyboard.Codes qualified as SDLCodes

-- | Current snapshot of the keyboard state. Contains the keyboard state of the moment it was sampled and does NOT change.
-- Use `KeyboardState` if you interested only in the current state of the keyboard. Do not use it if you want to know
-- that a key was pressed. It might happen that the user presses a key very quickly between two frames so you might not catch it
-- with `KeyboardState`.
newtype KeyboardState = KeyboardState (SDLCodes.Scancode -> Bool)

-- | Gets the current keyboard state. 
getKeyboardState :: System i KeyboardState
getKeyboardState = liftSystem (const $ KeyboardState <$> SDL.getKeyboardState)

-- | Checks if a key was pressed at the moment `KeyboardState` was sampled.
isPressed :: KeyboardState -> SDL.Scancode -> Bool
isPressed (KeyboardState f) scancode = f scancode

-- | Contains the SDL events which happened from the last frame to the current frame.
newtype SDLEvents = SDLEvents {events :: [SDL.Event]} deriving (Generic)

instance Component SDLEvents where
  type Store SDLEvents = Global

-- | A keyboard event which leaves out some information of the original SDL keyboard event
data KeyboardEvent = KeyboardEvent
  {key :: SDLCodes.Scancode, motion :: SDL.InputMotion}


-- | Gets all keyboard events which happened since the last time the SDL events were pulled.
getKeyboardEvents :: System () [KeyboardEvent]
getKeyboardEvents =
  getGlobal @SDLEvents
    >>> liftSystem
      ( pure . catMaybes . fmap (filterKeyboardEvents . SDL.eventPayload)
          . view #events
      )
  where
    filterKeyboardEvents (SDL.KeyboardEvent keyboardEvent) =
      Just $
        KeyboardEvent
          { key = SDL.keysymScancode (SDL.keyboardEventKeysym keyboardEvent),
            motion = SDL.keyboardEventKeyMotion keyboardEvent
          }
    filterKeyboardEvents _ = Nothing

-- | Adds the `SDLEvents` store and updates it each frame. SDLEvents will contain the event that happened
-- between the last frame and the current frame.
addSDLEvents :: System () ()
addSDLEvents =
  withSetup (\_ -> SDL.initialize [SDL.InitEvents]) (\_ -> pure ())
    >>> addGlobal (SDLEvents [])
    >>> liftSystem (const $ SDLEvents <$> SDL.pollEvents)
    >>> putGlobal