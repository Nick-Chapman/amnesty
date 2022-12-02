
module UsingSDL (Config(..),runTerm,runSDL) where

import Behaviour (Behaviour(..),Report(..))
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Data.List.Extra (groupSort)
import Data.Map (Map)
import Data.Word8 (Word8)
import Eff (Eff)
import Foreign.C.Types (CInt)
import Frame (Frame)
import NesFile (NesFile)
import SDL (Renderer,Rectangle(..),V2(..),V4(..),Point(P),($=))
import System.Clock (TimeSpec(..),Clock(Monotonic),getTime)
import Text.Printf (printf)
import Types (Picture(..),XY(..),RGB(..),Key(..),Keys(..))
import qualified Data.Map.Strict as Map (lookup,fromList,toList)
import qualified Data.Set as Set (empty,insert,delete)
import qualified Data.Text as Text (pack)
import qualified Emulate as Slow (emulate)
import qualified FastEm as Fast (emulate)
import qualified Frame (toPicture,toFrameHash)
import qualified SDL

data Config = Config
  { verb :: Bool
  , fast :: Bool
  }

emulate :: Config -> NesFile -> (forall p. Eff p ()) -> Behaviour
emulate Config{fast} = if
  | fast -> Fast.emulate
  | otherwise -> Slow.emulate

runTerm :: Config -> Maybe Int -> NesFile -> (forall p. Eff p ()) -> IO ()
runTerm conf@Config{verb} maxM nesFile the_effect = do
  let stop = case maxM of
        Nothing -> \_ -> False
        Just max -> \n -> n>max
  let keys = Keys { pressed = Set.empty }
  let
    loop :: TimeSpec -> Int -> Behaviour -> IO ()
    loop time0 n behaviour = if stop n then pure () else do
      case behaviour of
        Log s b -> do putStrLn s; loop time0 n b
        Poll f -> loop time0 n (f keys)
        Render frame report behaviour -> do
          let h = Frame.toFrameHash frame
          if h /= h then error "" else do -- hack, forcing!
            time <- getTime Monotonic
            printStatLine verb World { keys, frameCount = n } frame (time-time0) 0 report
            loop time (n+1) behaviour

  time0 <- getTime Monotonic
  loop time0 1 $ emulate conf nesFile the_effect


data ScreenSpec = ScreenSpec
  { sf ::Int
  , size :: XY Int
  }

data World = World
  { keys :: Keys
  , frameCount :: Int
  }

runSDL :: Config -> NesFile -> (forall p. Eff p ()) -> IO ()
runSDL conf@Config{verb} nesfile the_effect = do
  let accpix = False
  let ss = ScreenSpec {sf = 2,size = XY { x = 256, y = 240 } }
  let! _ = keyMapTable
  SDL.initializeAll
  let fi = fromIntegral
  let ScreenSpec{sf,size=XY{x=screenW,y=screenH}} = ss
  let border = 10
  let windowSize = V2 (fi sf * (2*border + fi screenW)) (fi sf * (2*border + fi screenH))
  let winConfig = SDL.defaultWindow { SDL.windowInitialSize = windowSize }
  win <- SDL.createWindow (Text.pack $ "NES") $ winConfig
  renderer <- SDL.createRenderer win (-1) SDL.defaultRenderer
  let assets = DrawAssets { renderer, ss, offset = border, accpix }
  let keys0 = Keys { pressed = Set.empty }
  let world0 = World { keys = keys0, frameCount = 1 }
  let behaviour0 = emulate conf nesfile the_effect

  let
    loop :: TimeSpec -> World -> Behaviour -> IO ()
    loop time world = \case
      Log s b -> do putStrLn s; loop time world b

      Poll f -> do
        events <- SDL.pollEvents
        let interesting = [ i | e <- events, i <- interestingOf e ]
        if Quit `elem` interesting then pure () else do --quit
          keys <- pure $ foldl insertInteresting (keys world) interesting
          loop time world { keys } (f keys)

      Render frame report behaviour  -> do
        let h = Frame.toFrameHash frame
        if h /= h then error "" else do -- hack, forcing!
          t1 <- getTime Monotonic
          drawEverything assets (Frame.toPicture frame)
          t2 <- getTime Monotonic
          printStatLine verb world frame (t1-time) (t2-t1) report
          let _ = threadDelay (1000000 `div` 60) -- 1/60 sec
          loop t2 world { frameCount = frameCount world + 1 } behaviour

  setColor renderer darkGrey
  SDL.clear renderer

  time0 <- getTime Monotonic
  loop time0 world0 behaviour0

  SDL.destroyRenderer renderer
  SDL.destroyWindow win
  SDL.quit


printStatLine :: Bool -> World -> Frame -> TimeSpec -> TimeSpec -> Report -> IO ()
printStatLine verb World{frameCount,keys} frame t1 t2
  Report{regs,vmemReadCount=vr,vramWriteCount=vw} = do
  if
    | verb -> do
        printf "%03d %s %s %s %s #vw=%d, #vr=%d, regs=%s\n"
          frameCount (show keys) (show $ Frame.toFrameHash frame)
          (seeTimeSpec t1) (seeTimeSpec t2)
          vw vr (show (Map.toList regs))
    | otherwise -> do
        printf "%03d %s\n" frameCount (show $ Frame.toFrameHash frame)



seeTimeSpec :: TimeSpec -> String
seeTimeSpec TimeSpec{sec,nsec} = do
  printf "%d.%03d" sec (nsec `div` million) :: String
  where million = 1_000_000


data InterestingEvent = Press Key | Release Key | Quit deriving Eq

insertInteresting :: Keys -> InterestingEvent -> Keys
insertInteresting ks@Keys{pressed} = \case
  Press key -> ks { pressed = Set.insert key pressed }
  Release key -> ks { pressed = Set.delete key pressed }
  Quit -> ks

interestingOf :: SDL.Event -> [InterestingEvent]
interestingOf = \case
  SDL.Event _t SDL.QuitEvent -> [Quit]
  SDL.Event _ (SDL.KeyboardEvent ke) -> do
    let code = SDL.keysymKeycode (SDL.keyboardEventKeysym ke)
    let motion = SDL.keyboardEventKeyMotion ke
    case (code,motion) of
      (SDL.KeycodeEscape,SDL.Pressed) -> [Quit]
      (SDL.KeycodeQ,SDL.Pressed) -> [Quit]
      -- TODO: Pause
      _ ->
        case Map.lookup code keyMapTable of
          Nothing -> []
          Just key -> do
            case motion of
              SDL.Pressed -> [Press key]
              SDL.Released -> [Release key]
  SDL.Event _ _ ->
    []

keyMapTable :: Map SDL.Keycode Key
keyMapTable = Map.fromList ys
  where
    xs = [ (by, key) | key <- [minBound..maxBound], by <- keyedBy key ]
    ys = [ (code, expectUnique code keys) | (code,keys) <- groupSort xs ]
    expectUnique code = \case
      [key] -> key
      keys -> error $
        unlines $
        ("bad keyMapTable: " <> show code) : [ "--> " <> show key | key <- keys ]

    -- | define the reverse mapping to be sure we are complete
    keyedBy :: Key -> [SDL.Keycode]
    keyedBy = \case
      KeyZ -> [SDL.KeycodeZ]
      KeyX -> [SDL.KeycodeX]
      KeyY -> [SDL.KeycodeY]
      KeyN -> [SDL.KeycodeN]
      KeyP -> [SDL.KeycodeP]
      KeyEnter -> [SDL.KeycodeReturn]
      KeyShift -> [SDL.KeycodeLShift, SDL.KeycodeRShift]

data DrawAssets = DrawAssets
  { renderer :: Renderer
  , ss :: ScreenSpec
  , offset :: CInt
  , accpix :: Bool
  }

drawEverything :: DrawAssets -> Picture -> IO ()
drawEverything assets@DrawAssets{renderer=r,accpix} picture = do
  when (not accpix) $ do
    setColor r darkGrey
    SDL.clear r
  renderPicture assets picture
  SDL.present r

renderPicture :: DrawAssets -> Picture  -> IO ()
renderPicture DrawAssets{renderer=r,ss,offset} = traverse
  where
    traverse :: Picture -> IO ()
    traverse = \case
      Pictures pics -> mapM_ traverse pics
      Draw (XY{x,y}) rgb -> do
        let fi = fromIntegral
        let ScreenSpec{sf,size=XY{x=maxX,y=maxY}} = ss
        when (x<0) $ error (show ("pixel:x<0",x))
        when (y<0) $ error (show ("pixel:y<0",y))
        when (x>=maxX) $ error (show ("pixel:x>=maxX",x,maxX))
        when (y>=maxY) $ error (show ("pixel:y>=maxY",y,maxY))
        setColor r rgb
        let x' = fi sf * (fi x + offset)
        let y' = fi sf * (fi y + offset)
        let rect = SDL.Rectangle (SDL.P (V2 x' y')) (V2 (fi sf) (fi sf))
        SDL.fillRect r (Just rect)

darkGrey :: RGB Word8
darkGrey = RGB { r = 50, g = 50, b = 50 }

setColor :: SDL.Renderer -> RGB Word8 -> IO ()
setColor r c = SDL.rendererDrawColor r $= fromRGB c
  where
    fromRGB RGB {r,g,b} = V4 r g b 255
