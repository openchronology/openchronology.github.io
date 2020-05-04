module MaterialUI.Icons.RemoveIcon (removeIcon) where

import React (ReactClass, ReactElement, unsafeCreateElement)

foreign import classRemoveIcon :: forall a. ReactClass a


removeIcon :: ReactElement
removeIcon = unsafeCreateElement classRemoveIcon {} []
