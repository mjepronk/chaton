{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    )
where

import Prelude hiding (putStrLn)
import Control.Monad (forever, void)
import Control.Monad.State.Strict (StateT, evalStateT, get, put, liftIO)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO (putStrLn)
import Data.Time.Clock (DiffTime, secondsToDiffTime)
import Evdev (Event(..), EventData(..), KeyEvent(..), Device, evdevDir,
    newDevice, nextEvent, deviceName, grabDevice, ungrabDevice)
import Evdev.Codes (Key(KeyWakeup))
import System.Process (readProcess)

data AppState = AppState
    { chatonMode   :: Bool  -- True if we are in Chaton mode (input disabled)
    , lastKeyPress :: DiffTime
    } deriving (Show)

-- RawFilePath (ByteString) for the keyboard device that should trigger Chaton mode
-- Note: evdevDir is usually `/dev/input/`
triggerDevice :: ByteString
triggerDevice = evdevDir <> "/" <> "event3"

-- RawFilePath for other devices that we want to disable in Chaton mode
otherDevices :: [ByteString]
otherDevices = fmap (\x -> evdevDir <> "/" <> x) ["event4", "event5"]

-- The key that will toggle Chaton mode on/off
triggerKey :: Key
triggerKey = KeyWakeup -- This is Fn on my ThinkPad T530

-- The number of seconds the `triggerKey` needs to be pressed
keyPressSeconds :: DiffTime
keyPressSeconds = secondsToDiffTime 1

main :: IO ()
main = do
    dev <- newDevice triggerDevice
    otherDevs <- traverse newDevice otherDevices
    void . flip evalStateT initialState . forever $ do
        event <- liftIO $ nextEvent dev
        st <- get
        case eventData event of
            KeyEvent k Pressed
                | k == triggerKey ->
                    put $ st { lastKeyPress = eventTime event }
            KeyEvent k Released
                | k == triggerKey &&
                  (eventTime event - lastKeyPress st) > keyPressSeconds -> do
                    -- `triggerKey` is pressed for more than `keyPressSeconds` seconds
                    let enabled = not (chatonMode st)
                    liftIO $ toggleChatonMode (dev : otherDevs) enabled
                    put $ st { chatonMode = enabled }
            _ ->
                pure ()
  where
    initialState = AppState False (secondsToDiffTime 0)

toggleChatonMode :: [Device] -> Bool -> IO ()
toggleChatonMode ds True  = do
    traverse grabDevice ds
    notify "enabled" "disable"
toggleChatonMode ds False = do
    traverse ungrabDevice ds
    notify "disabled" "enable"

notify :: Text -> Text -> IO ()
notify curr other = do
    putStrLn $ "Chaton mode " <> curr <> "."
    readProcess "/usr/bin/notify-send"
        [ "Chaton"
        , "Chaton mode " <> T.unpack curr <> " press Fn for 1 second to " <> T.unpack other <> "."
        ]
        []
    pure ()
