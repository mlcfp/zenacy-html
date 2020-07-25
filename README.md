# Zenacy HTML

Zenacy HTML is a library for parsing and processing HTML.

The purpose of this project:

* Implement and follow the HTML standard closely.

```haskell
import Zenacy.HTML

htmlRender $ htmlParseEasy "<div>HelloWorld</div>"
```

```haskell
"<html><head></head><body><div>HelloWorld</div></body></html>"
```

```haskell
```

```haskell
HTMLDocument ""
  [ HTMLElement "html" HTMLNamespaceHTML []
    [ HTMLElement "head" HTMLNamespaceHTML [] []
    , HTMLElement "body" HTMLNamespaceHTML []
      [ HTMLElement "div" HTMLNamespaceHTML []
        [ HTMLText "HelloWorld" ] ] ] ]
```

```haskell
HTMLDocument
  { htmlDocumentName = ""
  , htmlDocumentChildren =
    [ HTMLElement
      { htmlElementName = "html"
      , htmlElementNamespace = HTMLNamespaceHTML
      , htmlElementAttributes = []
      , htmlElementChildren =
        [ HTMLElement
          { htmlElementName = "head"
          , htmlElementNamespace = HTMLNamespaceHTML
          , htmlElementAttributes = []
          , htmlElementChildren = []
          }
        , HTMLElement
          { htmlElementName = "body"
          , htmlElementNamespace = HTMLNamespaceHTML
          , htmlElementAttributes = []
          , htmlElementChildren =
            [ HTMLElement
              { htmlElementName = "div"
              , htmlElementNamespace = HTMLNamespaceHTML
              , htmlElementAttributes = []
              , htmlElementChildren =
                [ HTMLText
                  { htmlTextData = "HelloWorld" }
                ] } ] } ] } ] }
```
