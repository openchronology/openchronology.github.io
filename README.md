# OpenChronology

[![Build Status](https://travis-ci.org/openchronology/openchronology.github.io.svg?branch=master)](https://travis-ci.org/openchronology/openchronology.github.io)


A web application for creating and sharing timelines.


## Usage

Everything should work out-of-the-box; you should only need to view `index.html` in a modern browser
to access the application. If you're also interested in offline use, there is a bundled
`openchronology-static.zip` file that has everything needed to run the app locally,
with no external dependencies.


## Building

This app is built with some very niche engineering tools and specific opinions; you may find some of them
unusual, but the tool suite has thus far been robust.

### Prerequisites

- a linux machine (sorry!)
- [PureScript](https://purescript.org) v0.13.2
- [Pulp](https://github.com/purescript-contrib/pulp) v13.0.0
- [nvm](https://github.com/nvm-sh/nvm) node version v12.16.2
- [ltext](http://ltext.github.io/) v0.1.3 (may require [stack](https://haskellstack.org))
- [browserify](http://browserify.org/) v16.5.0
- [babelify](https://www.npmjs.com/package/babelify) v10.0.0
- the `zip` command

### Description

There's a number of hoops being jumped through in this process:

1. PureScript gets compiled to JavaScript, with a lot of files (one per module) via `pulp build`
2. PureScript consolidates only the _used_ JavaScript files into one big one via `purs bundle`
3. Browserify and Babelify translate any residual ECMAScript 6 dependencies into JavaScript, and makes everything
   browser-safe
4. lText stitches the built files into a static single-file HTML file (and a dynamic one for hosting)
5. `zip` builds a static distribuition

### Procedure

After ensuring you have all of the prerequisites installed, the following commands should build the release:

```bash
$ npm install
$ bower install
$ ./build # ./build production for a minified release
```
