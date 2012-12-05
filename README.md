# The Dormouse Library


`cl-dormouse` is a windowing 'graphical' user interface library, built on top of
the [Doryen Library (libtcod)][libtcod].


## What is the Doryen Library?


The [Doryen Library][libtcod], or `libtcod`, is a library that implements a
truecolour console. It can be thought of as a souped-up alternative to
[Curses][]. The console can be of any size that fits on the screen. Both
keyboard and mouse events are handled. There is support for arbitrarily large
character sets, and font antialiasing. BMP and PNG images can be displayed
alongside text.

While libtcod is well-suited for any application where a console-based
interface is desired, the main purpose of the library is to support the
development of [roguelike games][roguelike]. As such, libtcod also provides
line-of-sight calculations, pathfinding, perlin noise, height maps, binary
space partitioning (BSP), parsing of configuration files, and other features
useful to game developers.

Libtcod is written in C and C++. Bindings for libtcod are currently available
for several programming languages including Python, Lua, C#, D, and
[Common Lisp](http://bitbucket.org/eeeickythump/cl-tcod/).

[libtcod]: http://doryen.eptalys.net/libtcod/
[Curses]: <http://en.wikipedia.org/wiki/Curses_(programming_library)>
[roguelike]: http://en.wikipedia.org/wiki/Roguelike_game


## What is `cl-dormouse`?


`cl-dormouse` is a windowing "graphical" user interface built on top of
libtcod. The programmer creates Window objects which are displayed on the root
console. The mouse can be used to move, resize, and close windows. Keyboard and
mouse events are sent to whichever window object has the focus.


## Features


* **Full mouse support**, including drag and drop events from one window to
  another, and moving and resizing windows with the mouse.
* **True colour support**: Each window's foreground, background, and text can
  be displayed in any 32-bit colour.
* **Simple inline formatting of strings**: Text strings can contain formatting
  directives, similar to HTML, which can:

  *  instruct the library about the colours in which parts of the string should
     be displayed;
  *  compose accented characters;
  *  define 'live' areas of the string which generate special events when
     clicked with the mouse (acting like dialog buttons).

* **Specialised window types**: there are many subclasses of the base 'Window'
  class with specialised behaviour, including:

  * _Viewports_: windows which provide a view onto an underlying map (array),
    which may be much larger. Dragging with the mouse moves the viewport around
    the map. Intended for the main display area in roguelike games.
  * _List windows_: browse and select from a list of values. Items can have
    'hotkeys' defined for quick selection.
  * _Filtered windows_: list windows that allow the user to type a 'filter
    string', only displaying items which contain the string.
  * _Menu windows_: simplified list window where a single mouse click selects and
    closes the window.
  * _Log windows_: a scrolling 'buffer' of text, with new messages added at the
    bottom.
  * _Modal windows_: prevent access to other windows, forcing the user to respond
    to a message or dialog.
  * _Ghost windows_: cannot be interacted with, useful for simply displaying some
    information in an area of the screen.
  * _Dialog windows_: present strings containing 'dialog buttons' to the user.
  * _Tooltip windows_: display a message when the mouse hovers over items within
    the window.
  * _Context windows_: window in which right-clicking on an item brings up a list
    of commands which can be applied to the item, where the commands presented
    will vary according to the characteristics of the right-clicked item.
  * And any combination of the above, using multiple inheritance...
* **Window dependencies**: Windows can be created as 'children' of other
  windows, allowing hiding, showing, creation and destruction of a complex
  group of windows as a unit.

The latest version of `cl-dormouse` can be found at <http://bitbucket.org/eeeickythump/cl-dormouse/>.


## Dependencies


* The Doryen library, which can be found at <http://doryen.eptalys.net/libtcod/>.
* The following Common Lisp libraries:

  * `ASDF` or `Quicklisp`
  * `cl-tcod`: <http://bitbucket.org/eeeickythump/cl-tcod/>
  * `iterate`: <http://common-lisp.net/project/iterate/>
  * `alexandria`: <http://common-lisp.net/project/alexandria/>


## Installation


* Install all dependencies (see above).
* Download `cl-dormouse` from its
  [repository](http://bitbucket.org/eeeickythump/cl-dormouse/).
* Run your lisp and make sure you can load `ASDF`, and `ASDF` can load
  `cl-tcod` and `cl-dormouse`.


## Getting started


The following is a minimal 'hello world' application:
Save it in a file, load it, and run `(mypkg:my-test)` at the lisp prompt
to try it.

        :::cl
        (in-package :cl-user)
        (defpackage :mypkg
        (:use :cl :tcod :dormouse)
        (:export #:my-test))

        (in-package :mypkg)

        (defun my-test ()
          (let ((msgwin nil))
            (dormouse:start-gui :title "Testing")
            (setf msgwin
              (make-instance '<Log-Window> :tlx 30 :tly 10 :width 20 :height 6
                        :title "log" :foreground :cornsilk
                        :background :dark-blue))
              (add-message msgwin "Press control-F1 or control-Esc to quit")
              (main-gui-loop)))

## Demonstration

The files `gui-demo.lisp` and `gui-demo.asd` in the root directory contain a
demonstration of `cl-dormouse`. Run the demonstration by issuing the following
commands at the Lisp prompt:

    :::cl
    (asdf:oos 'asdf:load-op "gui-demo")
    (gui-demo:gui-demo)
