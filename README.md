# txmonad: A toy xmonad
[txmonad]() is a toy [xmonad](https://github.com/xmonad/xmonad), which is a wonderful tiling window manager written in Haskell. txmond simulates a part of xmonad major feature in the cli mode. The purpose of starting this project is that xmonad is a wonderful tiling window manager, as well as a famous Haskell product level code tutorial, but xmonad is tightly coupling with X11, which makes it harder for Haskell beginners from different platform/system play around.

Here, txmonad imitates xmonad architecture and type design to offer a playground for Haskell beginners. So you can play, change and run the code to see the response swiftly.

## Quick Start
We use [stack](https://github.com/commercialhaskell/stack) to build the project
After install stack, simply run under the project folder:
```
stack build
stack exec txmonad-exe
```
Press h to view more supported command

## Screenshots
![demo](https://i.postimg.cc/rwPYZSZK/txmonad.jpg)


## What do txmonad xmond have in common
* Architecture design for layout.
* Functional data stucture design for worspaces.
* Type-level programming and type design.
* Product-level code design

## What txmonad is different from xmond
* txmonad is NOT a window manager.
* txmonad doesn't dependent on X11.
* txmonad simplifies event and message handling process for operations. (Maybe over-simplied!)

## Future develop plan
* user configurable config and custom layout algorithm.
* quick-check support
* A tutorial for haskell beginners
