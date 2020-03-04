module Average where

import CLaSH.Prelude

data Configuration = Configuration {
  e_lo :: Unsigned 16,
  e_hi :: Unsigned 16,
  delay :: Unsigned 32,
  active :: Vec 512 Bool
} deriving Show

configurationControllerT :: Configuration -> (Bool, Configuration) -> (Configuration,Configuration)
configurationControllerT oldConfig (enable,config) =
  if enable then (config,config) else (oldConfig,oldConfig)

initialConfiguration :: Configuration
initialConfiguration = Configuration 63 191 127 (replicate SNat False)

configurationController :: Signal (Bool, Configuration) -> Signal Configuration
configurationController = mealy configurationControllerT initialConfiguration

data ControllerState = ControllerState {
  switchState :: Bool,
  cnt :: Unsigned 32
} deriving Show

data ControllerInput = ControllerInput {
  configuration :: Configuration,
  e :: Unsigned 32
} deriving Show

controllerT :: ControllerState -> ControllerInput -> (ControllerState,Bool)
controllerT (ControllerState switchState cnt) (ControllerInput (Configuration e_lo e_hi delay active) e)
  | e' < e_lo                 = (ControllerState True cntn,True)
  | e' > e_hi && cnt >= delay = (ControllerState False cntn,False)
  | otherwise                 = (ControllerState switchState cntn,switchState)
    where cntn    = if e' > e_lo then if cnt < delay then cnt + 1 else cnt else 0          
          nactive = foldl (\acc b -> if b then acc + 1 else acc) (0::Unsigned 32) active
          e'      = if nactive == 0 then 0 else resize (e `div` nactive)

controller :: Signal ControllerInput -> Signal Bool
controller = mealy controllerT (ControllerState False 0)

configuredController :: Signal (Bool,Configuration,Unsigned 32) -> Signal Bool
configuredController input = controller (fmap (uncurry ControllerInput) $ bundle (cfgOut,sensor))
  where (enable,config,sensor) = unbundle input
        cfgOut = configurationController (bundle (enable,config))

topEntity = configuredController