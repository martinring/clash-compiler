module Extended where

import CLaSH.Prelude

data LightConfiguration = LightConfiguration {
  e_lo :: Unsigned 32,
  e_hi :: Unsigned 32,
  delay :: Unsigned 32,
  weights :: Vec 32 (Unsigned 16)
} deriving Show

type Configuration = Vec 32 LightConfiguration    

configurationControllerT :: Configuration -> (Bool, Configuration) -> (Configuration,Configuration)
configurationControllerT oldConfig (enable,config) =
  if enable then (config,config) else (oldConfig,oldConfig)

initialConfiguration :: Configuration
initialConfiguration = replicate SNat (LightConfiguration 43 191 127 (replicate SNat 0))

configurationController :: Signal (Bool, Configuration) -> Signal Configuration
configurationController = mealy configurationControllerT initialConfiguration

data LightControllerState = LightControllerState {
  switchState :: Bool,
  cnt :: Unsigned 32
} deriving Show

type ControllerState = Vec 32 LightControllerState

data ControllerInput = ControllerInput {
  config :: Configuration,
  es :: Vec 32 (Unsigned 16)
}

controllerT :: ControllerState -> ControllerInput -> (ControllerState, Vec 32 Bool)
controllerT state (ControllerInput config es) = (state',map switchState state') where
  state' = zipWith controllerT' state config
  controllerT' (LightControllerState switchState cnt) (LightConfiguration e_lo e_hi delay weights)
    | e < e_lo                 = LightControllerState True cntn
    | e > e_hi && cnt >= delay = LightControllerState False cntn
    | otherwise                = LightControllerState switchState cntn
      where cntn    = if e > e_lo then if cnt < delay then cnt + 1 else cnt else 0    
            wsum :: Unsigned 64
            wsum    = sum $ map resize $ weights
            esum :: Unsigned 64
            esum    = sum $ zipWith (\a b -> resize a * resize b) es weights
            e       = if wsum == 0 then 0 else resize (esum `div` wsum)

controller :: Signal ControllerInput -> Signal (Vec 32 Bool)
controller = mealy controllerT $ replicate SNat (LightControllerState False 0)

configuredController :: Signal (Bool, Configuration, Vec 32 (Unsigned 16)) -> Signal (Vec 32 Bool)
configuredController input = controller (fmap (uncurry ControllerInput) $ bundle (cfgOut,sensors))
  where (enable,config,sensors) = unbundle input
        cfgOut = configurationController (bundle (enable,config))

topEntity = configuredController