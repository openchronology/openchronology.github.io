module Components.Index where

import Components.AppBar (indexAppBar)
import WithRoot (withRoot)

import Prelude
import React (ReactElement, toElement)
import React.DOM (text)
import MaterialUI.Typography (typography)
import MaterialUI.Enums (title)



index :: ReactElement
index = withRoot $ toElement
  [ indexAppBar
  , typography {gutterBottom: true, variant: title} [text "Just a Test"]
  ]
