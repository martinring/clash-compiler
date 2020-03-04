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

data ControllerState = ControllerState {
  switchState :: Bool,
  cnt :: Unsigned 32
} deriving Show

data ControllerInput = ControllerInput {
  configuration :: Configuration,
  e :: Unsigned 16
} deriving Show

controllerT :: ControllerState -> ControllerInput -> (ControllerState,Bool)
controllerT (ControllerState switchState cnt) (ControllerInput (Configuration e_lo e_hi delay) e)
  | e < e_lo                 = ((ControllerState True cntn),True)
  | e > e_hi && cnt >= delay = ((ControllerState False cntn),False)
  | otherwise                = ((ControllerState switchState cntn),switchState)
    where 
      cntn = if e > e_hi then if cnt < delay then cnt + 1 else cnt else 0

controller :: Signal ControllerInput -> Signal Bool
controller = mealy controllerT (ControllerState False 0)

configuredController :: Signal (Bool,Configuration,Unsigned 16) -> Signal Bool
configuredController input = controller (fmap (uncurry ControllerInput) $ bundle (cfgOut,sensor))
  where (enable,config,sensor) = unbundle input
        cfgOut = configurationController (bundle (enable,config))

topEntity = configuredController