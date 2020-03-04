module WeightedAverage where

import CLaSH.Prelude

data Configuration = Configuration {
  e_lo :: Unsigned 32,
  e_hi :: Unsigned 32,
  delay :: Unsigned 32,
  weights :: Vec 32 (Unsigned 16)
} deriving Show

configurationControllerT :: Configuration -> (Bool, Configuration) -> (Configuration,Configuration)
configurationControllerT oldConfig (enable,config) =
  if enable then (config,config) else (oldConfig,oldConfig)

initialConfiguration :: Configuration
initialConfiguration = Configuration 63 191 127 (replicate SNat 0)

configurationController :: Signal (Bool, Configuration) -> Signal Configuration
configurationController = mealy configurationControllerT initialConfiguration

data ControllerState = ControllerState {
  switchState :: Bool,
  cnt :: Unsigned 32
} deriving Show

data ControllerInput = ControllerInput {
  configuration :: Configuration,
  es :: Vec 32 (Unsigned 16) 
} deriving Show

controllerT :: ControllerState -> ControllerInput -> (ControllerState,Bool)
controllerT (ControllerState switchState cnt) (ControllerInput (Configuration e_lo e_hi delay weights) es)
  | e < e_lo                 = (ControllerState True cntn,True)
  | e > e_hi && cnt >= delay = (ControllerState False cntn,False)
  | otherwise                = (ControllerState switchState cntn,switchState)
    where cntn    = if e > e_lo then if cnt < delay then cnt + 1 else cnt else 0    
          wsum :: Unsigned 64
          wsum    = sum $ map resize $ weights
          esum :: Unsigned 64
          esum    = sum $ zipWith (\a b -> resize a * resize b) es weights
          e       = if wsum == 0 then 0 else resize (esum `div` wsum)

controller :: Signal ControllerInput -> Signal Bool
controller = mealy controllerT (ControllerState False 0)

configuredController :: Signal (Bool, Configuration, Vec 32 (Unsigned 16)) -> Signal Bool
configuredController input = controller (fmap (uncurry ControllerInput) $ bundle (cfgOut,sensor))
  where (enable,config,sensor) = unbundle input
        cfgOut = configurationController (bundle (enable,config))

topEntity = configuredController