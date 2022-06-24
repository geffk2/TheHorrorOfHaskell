{-# LANGUAGE BlockArguments #-}
module MusicPlayer where 

import qualified SDL
import qualified SDL.Mixer  as Mix
import qualified Control.Monad.IO.Class
import SDL.Mixer (Channel, Chunk)


initMusicPlayer :: IO()
initMusicPlayer = do
  SDL.initialize [SDL.InitAudio]
  Mix.openAudio Mix.defaultAudio 256


data Sound 
  = Walking 
  | StopWalking
  | Doorbell 
  | Silence 
  | Menu
  deriving Eq

type Sounds = [(Sound, Mix.Chunk)]


loadSounds :: IO (Sound -> Mix.Chunk)
loadSounds = do
  walking  <- Mix.load "sounds/walking.wav"
  doorbell <- Mix.load "sounds/doorbell.wav"
  silence  <- Mix.load "sounds/silence.wav"
  menu     <- Mix.load "sounds/menu.wav"

  let f Walking = walking
      f Doorbell = doorbell
      f Silence = silence
      f StopWalking = silence
      f Menu = menu
  
  return f


soundToChannel :: Sound -> Channel
soundToChannel Silence     = 0
soundToChannel Walking     = 1
soundToChannel StopWalking = 1
soundToChannel Doorbell    = 2
soundToChannel Menu        = 3


playSound :: Control.Monad.IO.Class.MonadIO m => Sound -> (Sound -> Mix.Chunk) -> m (Channel)
playSound sound s2c = do 
  let channelId = soundToChannel sound

  isPlaying <- Mix.playing channelId

  case (isPlaying, sound) of
    (False, _)          -> Mix.playOn channelId 1 (s2c sound)
    (True, StopWalking) -> Mix.playOn channelId 1 (s2c Silence)
    (True, _)           -> Mix.playOn 0 0 (s2c Silence)


playSoundForever :: Control.Monad.IO.Class.MonadIO m => Sound -> (Sound -> Mix.Chunk) -> m (Channel)
playSoundForever sound s2c = do 
  let channelId = soundToChannel sound

  isPlaying <- Mix.playing channelId
  
  Mix.playOn channelId Mix.Forever (s2c sound)

