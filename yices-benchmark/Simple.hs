module Simple where
  
import CLaSH.Prelude

data Configuration = Configuration {
  e_lo :: Unsigned 16,
  e_hi :: Unsigned 16,
  delay :: Unsigned 32
} deriving Show

configurationControllerT :: Configuration -> (Bool, Configuration) -> (Configuration,Configuration)
configurationControllerT oldConfig (enable,config) = 
  if enable then (config,config) else (oldConfig,oldConfig)

configurationController :: Signal (Bool, Configuration) -> Signal Configuration
configurationController = mealy configurationControllerT (Configuration 63 191 127)

data SwitchState = On | Off deriving Show

data ControllerState = ControllerState {
  switchState :: SwitchState,
  cnt :: Unsigned 32
} deriving Show

data ControllerInput = ControllerInput {
  configuration :: Configuration,
  e :: Unsigned 16
} deriving Show

controllerT :: ControllerState -> ControllerInput -> (ControllerState,SwitchState)
controllerT (ControllerState switchState cnt) (ControllerInput (Configuration e_lo e_hi delay) e)
  | e > e_hi                = ((ControllerState On cntn),On)
  | e < e_lo && cnt > delay = ((ControllerState Off cntn),Off)
  | otherwise                    = ((ControllerState switchState cntn),switchState)
    where 
      cntn = case switchState of 
        On  -> cnt + 1
        Off -> 0

controller :: Signal ControllerInput -> Signal SwitchState
controller = mealy controllerT (ControllerState Off 0)

configuredController :: Signal (Bool,Configuration,Unsigned 16) -> Signal SwitchState
configuredController input = controller (fmap (uncurry ControllerInput) $ bundle (cfgOut,sensor))
  where (enable,config,sensor) = unbundle input
        cfgOut = configurationController (bundle (enable,config))

topEntity = configuredController