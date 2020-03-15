module MaterialUI.Icons.SettingsIcon (settingsIcon) where

import React (ReactClass, ReactElement, unsafeCreateElement)

foreign import classSettingsIcon :: forall a. ReactClass a


settingsIcon :: ReactElement
settingsIcon = unsafeCreateElement classSettingsIcon {} []
