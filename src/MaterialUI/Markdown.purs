module MaterialUI.Markdown where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe)
import React (ReactElement, createLeafElement, statelessComponent, childrenToArray)
import React.DOM (code, pre, ul, ol, blockquote) as R
import React.DOM.Props (style, className) as RP
import React.DOM.Markdown (markdown) as RM
import React.DOM.Highlight (highlight)
import MaterialUI.Typography (typography)
import MaterialUI.Divider (divider')
import MaterialUI.Enums (headline, title, subheading, display1, display2, display3, body1, caption)
import Partial.Unsafe (unsafePartial)

markdown :: String -> ReactElement
markdown source =
  RM.markdown
    { source
    , renderers:
        { heading:
            statelessComponent \{ level, children } ->
              unsafePartial
                $ case level of
                    1 -> typography { variant: display3 } (childrenToArray children)
                    2 -> typography { variant: display2 } (childrenToArray children)
                    3 -> typography { variant: display1 } (childrenToArray children)
                    4 -> typography { variant: headline } (childrenToArray children)
                    5 -> typography { variant: title } (childrenToArray children)
                    6 -> typography { variant: subheading } (childrenToArray children)
        , paragraph: statelessComponent \{ children } -> typography { variant: body1, paragraph: true } (childrenToArray children)
        , thematicBreak: statelessComponent \_ -> divider' { style: { margin: "1em 0" } }
        , blockquote:
            statelessComponent \{ children } ->
              typography { variant: caption }
                [ R.blockquote [] (childrenToArray children) ]
        , list:
            statelessComponent \{ children, ordered } ->
              typography { variant: body1 }
                [ if ordered then
                    R.ol [] (childrenToArray children)
                  else
                    R.ul [] (childrenToArray children)
                ]
        , inlineCode: statelessComponent \{ children } -> R.code [ RP.style { border: "1px solid #ddd" } ] (childrenToArray children)
        , code:
            statelessComponent \{ language, value } ->
              let codeProps = case toMaybe language of
                    Nothing -> {language: ""}
                    Just l -> {language: l}

              in  highlight codeProps (childrenToArray value)
              -- let
              --   codeProps = case toMaybe language of
              --     Nothing -> []
              --     Just l -> [ RP.className ("language-" <> l) ]

              --   code = R.code codeProps (childrenToArray value)
              -- in
              --   R.pre [ RP.style { border: "1px solid #ddd" } ] [ code ]
        }
    }
