module MaterialUI.Icons.AddIcon (addIcon) where

import React (ReactClass, ReactElement, unsafeCreateElement)

foreign import classAddIcon :: forall a. ReactClass a


addIcon :: ReactElement
addIcon = unsafeCreateElement classAddIcon {} []
