module Components.Index where

import Components.AppBar (indexAppBar)
import Components.Dialogs.Import (importDialog)
import WithRoot (withRoot)

import Prelude
import IOQueues (new) as IOQueues
import React (ReactElement, ReactClass, toElement, pureComponent, createLeafElement)
import React.DOM (text)
import MaterialUI.Typography (typography)
import MaterialUI.Enums (title)



index :: ReactElement
index = withRoot e
  where
    e = createLeafElement c {}
    c :: ReactClass {}
    c = pureComponent "Index" \this -> do
          qs <- IOQueues.new
          pure
            { state: {}
            , render: pure $ toElement
              [ indexAppBar {onImport: pure unit, onExport: pure unit}
              , typography {gutterBottom: true, variant: title} [text "Just a Test"]
              , importDialog qs
              ]
            }
