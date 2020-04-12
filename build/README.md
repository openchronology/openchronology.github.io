# Build Templates

This build suite uses [ltext](https://ltext.github.io) to link various text files together.

## Description

The [./build.sh](../build.sh) script manages the following:

- compiling and linking of the PureScript -> JavaScript and ECMAScript -> JavaScript
- JavaScript minification
- Web Source Code linking
- Static `.zip` Distribuition

## Files

- [index.template.html](./index.template.html) is the primary HTML template - if you want
  HTML elements to be on both the static and dynamic distribuitions, this is where it's put.
- [staticscripts.template.html](./staticscripts.template.html) is where JavaScript is linked,
  which will be in the static distribution.
  - JavaScript for the app itself
- [dynamicscripts.html](./dynamicscripts.html) is where JavaScript is _referenced_ - it is not
  a template for further text sources, but rather externally links via `<script src="...">`.
  - JavaScript for the app itself
- [dynamicscripts.production.html](./dynamicscripts.production.html) is where _minified_ JavaScript
  is referenced.
  - JavaScript for the app itself
- [staticstyles.template.html](./staticstyles.template.html) is where CSS is linked,
  which will be in the static distribution.
  - Fonts and Icons, locally sourced
- [dynamicstyles.template.html](./dynamicstyles.template.html) is where CSS is _referenced_ via CDNs
  - Fonts and Icons, externally sourced
