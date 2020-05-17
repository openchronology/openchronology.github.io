module MaterialUI.Icons.GetAppIcon (getAppIcon) where

import React (ReactClass, ReactElement, unsafeCreateElement)

foreign import classGetAppIcon :: forall a. ReactClass a

getAppIcon :: ReactElement
getAppIcon = unsafeCreateElement classGetAppIcon {} []
