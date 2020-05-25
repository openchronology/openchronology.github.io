module MaterialUI.Icons.ExpandLessIcon (expandLessIcon) where

import React (ReactClass, ReactElement, unsafeCreateElement)

foreign import classExpandLessIcon :: forall a. ReactClass a

expandLessIcon :: ReactElement
expandLessIcon = unsafeCreateElement classExpandLessIcon {} []
