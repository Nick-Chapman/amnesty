
module UsingSDL (main,nopic) where

--import System.Clock (TimeSpec(..),Clock(Monotonic),getTime) -- TODO: reinstate?
import Behaviour (Behaviour(..),Report(..))
import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Data.List.Extra (groupSort)
import Data.Map (Map)
import Data.Word8 (Word8)
import Emulate (emulate)
import Foreign.C.Types (CInt)
import Frame (Frame)
import GHC.Int (Int64)
import NesFile (NesFile)
import SDL (Renderer,Rectangle(..),V2(..),V4(..),Point(P),($=))
import Text.Printf (printf)
import Types (Picture(..),XY(..),RGB(..),Key(..),Keys(..))
import qualified Data.Map.Strict as Map (lookup,fromList,toList)
import qualified Data.Set as Set (empty,insert,delete)
import qualified Data.Text as Text (pack)
import qualified Frame (toPicture,toFrameHash)
import qualified SDL
import qualified System (top)

nopic :: Bool -> Maybe Int -> NesFile -> IO ()
nopic doReport maxM context = do
  let stop = case maxM of
        Nothing -> \_ -> False
        Just max -> \n -> n==max
  let the_effect = System.top
  let keys0 = Keys { pressed = Set.empty }
  let
    loop :: Int -> Behaviour -> IO ()
    loop n = \case
      Log{} -> undefined -- TODO
      Poll f -> loop n (f keys0)
      Render frame report behaviour -> do
        let Report{vmemReadCount=vr,vramWriteCount=vw} = report
        let r = if doReport then printf " #vw=%d, #vr=%d" vw vr else ""
        printf "%03d %s%s\n" n (show $ Frame.toFrameHash frame) r
        if stop n then pure () else
          loop (n+1) behaviour
  loop 1 $ emulate context the_effect


data ScreenSpec = ScreenSpec
  { sf ::Int
  , size :: XY Int
  }

data World = World
  { keys :: Keys
  , frameCount :: Int
  , accNanos :: Int64
  }

main :: NesFile -> IO ()
main nesfile = do
  let the_effect = System.top
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
  let world0 = World { keys = keys0, frameCount = 0, accNanos = 0 }
  let behaviour0 = emulate nesfile the_effect
  let
    loop :: World -> Behaviour -> IO ()
    loop world = \case
      Log{} -> undefined -- TODO

      Poll f -> do
        events <- SDL.pollEvents
        let interesting = [ i | e <- events, i <- interestingOf e ]
        if Quit `elem` interesting then pure () else do --quit
          keys <- pure $ foldl insertInteresting (keys world) interesting
          loop world { keys } (f keys)

      Render frame report behaviour  -> do
        printStatLine world frame report
        drawEverything assets (Frame.toPicture frame)
        threadDelay (1000000 `div` 60) -- 1/60 sec
        loop world behaviour

  setColor renderer darkGrey
  SDL.clear renderer
  loop world0 behaviour0
  SDL.destroyRenderer renderer
  SDL.destroyWindow win
  SDL.quit

printStatLine :: World -> Frame -> Report -> IO ()
printStatLine World{frameCount,keys} frame
  Report{regs,vmemReadCount=vr,vramWriteCount=vw} = do
  printf "%03d %s %s #vw=%d, #vr=%d regs=%s\n"
    frameCount (show keys) (show frameHash) vw vr (show (Map.toList regs))
    where frameHash = Frame.toFrameHash frame


{-printStatLine :: World -> Emu.FrameHash -> IO ()
printStatLine World{frame,accNanos} frameHash = do
  let
    emuSecsPerFrame = 1.0 / 60.0
    gig :: Double = 1_000_000_000
    emulatedSecs :: Double = fromIntegral frame * emuSecsPerFrame
    elaspedSecs :: Double = fromIntegral accNanos / gig
    speedup = emulatedSecs / elaspedSecs
    line =
      printf "%d, emu %.03f, elap %.03f, speedupX %.3f, hash=%s"
      frame emulatedSecs elaspedSecs speedup (show frameHash)
  putStrLn line-}

{-measureNanos :: IO a -> IO (a, Int64) -- TODO: reinstate
measureNanos io = do
  before <- getTime Monotonic
  a <- io
  after <- getTime Monotonic
  let TimeSpec{sec,nsec} = after - before
  let nanos = gig * sec + nsec
  return (a,nanos)
  where gig = 1_000_000_000-}

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
      (SDL.KeycodeEscape,SDL.Released) -> [Quit]
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
