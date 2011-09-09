;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; coding: utf-8-unix -*- ;;;;;;;;80
;;;;
;;;;    This file is part of DORMOUSE, by Paul Sexton
;;;;    Released under the Gnu Public License version 3
;;;;
;;;;    DORMOUSE is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;
;;;;    DORMOUSE is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU General Public License
;;;;    along with DORMOUSE.  If not, see <http://www.gnu.org/licenses/>.
;;;;
;;;;
;;;; DORMOUSE
;;;; "Graphical" user interface for the libtcod console library.
;;;;
;;;; TODO
;;;; Finish assertions for all tcod wrapper functions.
;;;; Colours don't return to default in log windows.
;;;; REPL.
;;;; Finish documentation.
;;;;
;;;; Table of contents
;;;; =================
;;;;
;;;; (Emacs users: org-mode will allow you to navigate using these
;;;; hyperlinks)
;;;;
;;;; [[Package definition]]
;;;; (@> "Utility functions")
;;;; (@> "Constants")
;;;; (@> "Global variables")
;;;; [[Keys]]
;;;; (@> "Colourising strings")

;;;; - [[Window class]]
;;;;      - [[List Window]]
;;;;           - [[Filtered Window]]
;;;;           - [[Menu Window]]
;;;;           - [[Log Window]]
;;;;             - [[Terminal Window]]
;;;;           - [[Pager Window]]
;;;;           - [[Hypertext Window]]
;;;;           - [[Terminal Window]]
;;;;      - [[Modal Window]]
;;;;           - [[Alert Window]]
;;;;           - [[Yes/No Window]]
;;;;      - [[Dialog Window]]
;;;;           - [[Tooltip Window]]
;;;;                uses [[Floating Window]]
;;;;      - [[Background window]]
;;;;      - (@> "Ghost-Window")
;;;;      - [[Window With Context Menu]]
;;;;      - (@> "Viewport")
;;;; - [[Window Themes]]
;;;;
;;;; The documentation for this package is generated using the CLDOC library.
;;;;
;;;; The following command is used: (dormouse and cldoc systems must be
;;;; loaded into the running lisp image first)
;;;;
;;;; (cldoc:extract-documentation 'cldoc:html
;;;;       "/home/paul/lisp/wormwood/src/tcod-gui/html"
;;;;       (asdf:find-system :dormouse)
;;;;       :table-of-contents-title
;;;;       "Dormouse, a graphical user interface using the Doryen library"
;;;;       :section-names '("Arguments:" "Returns:" "Examples:" "See Also:"
;;;;                        "Example:" "Notes:" "Description:"))
;;;;

;;; <<Package definition>> ====================================================

(in-package :cl-user)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defpackage :dormouse
  (:nicknames :dormouse :tcod-gui :tcod.gui)
  (:use :cl :tcod ;;:util
        :iterate)
  (:documentation
"DORMOUSE is a windowing `graphical' user interface library, built on top of
the [[http://doryen.eptalys.net/libtcod/][Doryen Library]] (libtcod).

* What is the Doryen Library?

The Doryen Library, or /libtcod/, is a library that implements a truecolour
console. It can be thought of as a souped-up alternative to
[[http://en.wikipedia.org/wiki/Curses_(programming_library)][Curses]]. The
console can be of any size that fits on the screen. Both keyboard and mouse
events are handled. There is support for artibrarily large character sets, and
font antialiasing. BMP and PNG images can be displayed alongside text.

While libtcod is well-suited for any application where a console-based
interface is desired, the author's motivation is to support the development of
roguelike games (see [[http://en.wikipedia.org/wiki/Roguelike_game]]). As such,
libtcod also provides line-of-sight calculations, pathfinding, perlin noise,
height maps, BSP, parsing of configuration files, and other features useful
to game developers.

Libtcod is written in C and C++. Bindings for libtcod are currently available
for the python, C#, D, and
[[http://bitbucket.org/eeeickythump/cl-tcod/][Common Lisp]] languages.

* What is DORMOUSE?

Dormouse ('Doryen' + 'Mouse', groan) is a windowing `graphical' user interface
built on top of libtcod. The programmer creates Window objects which are
displayed on the root console. The mouse can be used to move, resize, and close
windows. Keyboard and mouse events are sent to whichever window object has the
focus.

* Features

- Full mouse support, including drag and drop events from one window to another,
  moving and resizing windows with the mouse.
- Each window's foreground, background, and text can be displayed in any 32-bit
  colour.
- Text strings can contain formatting directives, similar to HTML, which can:
  -  instruct the library about the colours in which parts of the string should
     be displayed
  -  compose accented characters
  -  define 'live' areas of the string which generate special events when
     clicked with the mouse (acting like dialog buttons).
- Many subclasses of the base 'Window' class with specialised behaviour,
  including:
  - Viewports: windows which provide a view onto an underlying map (array),
    which may be much larger. Dragging with the mouse moves the viewport around
    the map. Intended for the main display area in roguelike games.
  - List windows: browse and select from a list of values. Items can have
    'hotkeys' defined for quick selection.
  - Filtered windows: list windows that allow the user to type a 'filter
    string', only displaying items which contain the string.
  - Menu windows: simplified list window where a single mouse click selects and
    closes the window.
  - Log windows: a scrolling 'buffer' of text, with new messages added at the
    bottom.
  - Modal windows: prevent access to other windows, forcing the user to respond
    to a message or dialog.
  - Ghost windows: cannot be interacted with, useful for simply displaying some
    information in an area of the screen.
  - Dialog windows: present strings containing 'dialog buttons' to the user.
  - Tooltip windows: display a message when the mouse hovers over items within
    the window.
  - Context windows: window in which right-clicking on an item brings up a list
    of commands which can be applied to the item, where the commands presented
    will vary according to the characteristics of the right-clicked item.
  - And any combination of the above, using multiple inheritance...
- Windows can be created as 'children' of other windows, allowing hiding,
  showing, creation and destruction of a complex group of windows as a unit

The latest version of DORMOUSE can be found at:
- [[http://bitbucket.org/eeeickythump/cl-dormouse/]]

* Dependencies

- The Doryen library, which can be found at http://doryen.eptalys.net/libtcod/]
- CL-TCOD: http://bitbucket.org/eeeickythump/cl-tcod/
- ITERATE: http://common-lisp.net/project/iterate/
- ASDF

* Installation

- Install all dependencies (see above).
- Download =DORMOUSE= from its
  [[http://bitbucket.org/eeeickythump/cl-dormouse/][repository.]]
- Run your lisp and make sure you can load asdf, and asdf can load
  =CL-TCOD= and =DORMOUSE=.
- The following is a minimal 'hello world' application:
;;; (in-package :cl-user)
;;; (defpackage :mypkg
;;;  (:use :cl :tcod :dormouse)
;;;  (:export #:my-test))
;;;
;;; (in-package :mypkg)
;;;
;;; (defun my-test ()
;;;    (let ((msgwin nil))
;;;      (dormouse:start-gui :title \"Testing\")
;;;      (setf msgwin
;;;        (make-instance '<Log-Window> :tlx 30 :tly 10 :width 20 :height 6
;;;                      :title \"log\" :foreground :cornsilk
;;;                      :background :dark-blue))
;;;      (add-message msgwin \"Press control-F1 or control-Esc to quit\")
;;;      (main-gui-loop)))
- Save it in a file, load it, and run (mypkg:my-test) at the lisp prompt
  to try it.
")
  (:export #:start-gui
           #:main-gui-loop
           #:resume-gui
           #:*shift*
           #:*ctrl*
           #:*alt*
           #:*exit-gui?*
           #:+OPAQUE+
           #:+INVISIBLE+
           #:+DIMMED+
           #:screen-width
           #:screen-height
           #:legal-window-coordinates?
           #:colour
           #:destroy-window
           #:destroy-all-windows
           #:copy-windows-to-console
           #:prepare-window
           #:process-window
           #:redraw-window
           #:redraw-window-area
           #:redraw-all-windows
           #:prepare-windows-by-type
           #:do-for-windows-by-type
           #:save-interface-state
           #:restore-interface-state
           #:destroy-interface-state
           #:hide-window
           #:hide-all-windows
           #:raise-window
           #:move-window
           #:resize-window
           #:*window-stack*
           #:all-windows
           #:*mouse-x*
           #:*mouse-y*
           ;; === events ===
           #:send-to-window
           #:send-key-to-window
           #:<GUI-Event>
           #:<Key-Event>
           #:<Mouse-Event>
           #:<Mouse-Hover-Event>
           #:<GUI-Dialog-Event>
           #:<GUI-Select-Event>
           #:<GUI-Mouse-Drag-Event>
           #:gui-event-winx
           #:gui-event-winy
           #:gui-event-keypress
           #:gui-event-mouse-state
           #:gui-event-focus
           #:gui-event-string
           #:mouse-drag
           #:key->string
           #:<Window-Theme>
           #:*window-theme*
           #:<Window>
           #:<Modal-Window>
           #:<Ghost-Window>
           #:<Background-Window>
           #:<List-Window>
           #:<Menu-Window>
           #:<Alert-Window>
           #:<Yes/No-Window>
           #:<Tooltip-Window>
           #:<Filtered-Window>
           #:filter-string
           #:clear-filter
           #:binding->key
           #:character->vk
           #:character->shift
           #:<Window-With-Context-Menu>
           #:get-menu-items-for-context
           #:command-from-context-menu
           #:tooltip-text
           #:calculate-floating-window-width
           #:<Dialog-Window>
           #:window-cursor
           #:move-cursor-to
           #:cursor-moved-to-item
           #:window-offset
           #:wrap-coloured-text
           #:coloured-string-length
           #:window-draw-char-at
           #:draw-string-at
           #:add-item
           #:window-items
           #:window-items-lines
           #:clear-items
           #:list-item
           #:list-item-item
           #:list-item-hotkey
           #:list-item-p
           #:move-cursor-to-end
           #:window-show-tail-by-default?
           #:window-fades-when-unfocussed?
           #:window-can-close?
           #:window-can-drag?
           #:window-can-resize?
           #:window-item-at-cursor
           ;; Log window
           #:<Log-Window>
           #:clear-messages
           #:add-message
           #:add-message-and-redraw
           #:bottom-message
           ;; Pager window
           #:<Pager-Window>
           #:add-browser-line
           #:clear-browser-lines
           ;; [[Hypertext window]] ============================================
           #:<Hypertext-Window>
           #:make-autolinks-in-hypertext-database
           #:hyperlink-foreground-colour
           #:open-hypertext-topic
           ;; Terminal window
           #:<Terminal-Window>
           #:window-input-function
           #:window-echo-input?
           #:window-input-active?
           #:window-input-history
           ;; Viewports
           #:<Viewport>
           #:clear-map
           #:share-map
           #:unshare-map
           #:map-xdim
           #:map-ydim
           #:map-draw-char-at
           #:map-char-at
           #:map-set-foreground-at
           #:map-set-background-at
           #:in-view?
           #:centre-viewport-on
           #:window-foreground
           #:window-background
           #:window-width
           #:window-height
           #:window-tlx
           #:window-tly
           #:window-brx
           #:window-bry
           #:window-framed?
           #:window-children
           #:window-raise-children-with-parent?
           #:window-hidden?
           #:window-changed?
           #:window-auto-redraw?
           #:view-tlx
           #:view-tly
           #:view-brx
           #:view-bry
           #:viewport-width
           #:viewport-height
           #:in-view?
           #:in-viewport-map?
           #:winx->mapx
           #:winy->mapy
           #:winx->rootx
           #:winy->rooty
           #:rootx->winx
           #:rooty->winy
           #:bar-chart
           ))

(in-package :dormouse)

(defgeneric destroy-window (win)
  (:documentation
   "* Arguments
- WIN :: a [[<Window>]]
* Returns
Ignored.
* Description
Destroy the window object =WIN=, hiding it first if it is not
already hidden."))
(defgeneric touch-windows (win)
  (:documentation
   "* Arguments
- WIN :: a [[<Window>]]
* Returns
None.
* Description
Internal function. Makes =WIN= refresh its list of all other windows it is
touching, i.e. overlapping."))
(defgeneric untouch-windows (win)
  (:documentation "TODO document."))
(defgeneric dirty-window (win))
(defgeneric move-window (win tlx tly)
  (:documentation
   "* Arguments
- WIN :: a [[<Window>]]
- TLX, TLY :: coordinates on the screen where the top left corner of =WIN=
  is to be moved to.
* Returns
None.
* Description
Move the window =WIN= so that its top left corner is located
at =(TLX, TLY)= relative to the top left corner of the screen."))
(defgeneric window-touches-spot? (win x y)
  (:documentation
     "* Arguments
- WIN :: a [[<Window>]]
- X, Y :: Integer coordinates of a point on the root console.
* Returns
Boolean.
* Description
Predicate. Does this window overlap the spot at =(X, Y)= ?"))
(defgeneric windows-touching? (win other)
  (:documentation
     "* Arguments
- WIN :: a [[<Window>]]
- OTHER :: a [[<Window>]]
* Returns
Boolean.
* Description
Predicate. Do these two windows overlap somewhere?"))
(defgeneric window-parent (win)
  (:documentation "* Arguments
- WIN :: a [[<Window>]]
* Returns
A [[<Window>]] or nil.
* Description
Finds and returns the parent window of =WIN=, if it has one."))
(defgeneric windows-below (win)
  (:documentation
     "* Arguments
- WIN :: a [[<Window>]]
* Returns
A list of [[<Window>]] objects, or nil.
* Description
Returns list of all windows 'under' =WIN= in the window stack."))
(defgeneric windows-above (win)
  (:documentation
     "* Arguments
- WIN :: a [[<Window>]]
* Returns
A list of [[<Window>]] objects, or nil.
* Description
Returns list of all windows 'above' =WIN= in the window stack."))
(defgeneric windows-overlying (win)
  (:documentation
     "* Arguments
- WIN :: a [[<Window>]]
* Returns
A list of [[<Window>]] objects, or nil.
* Description
List of all windows that both overlap with WIN and are above it in
the stack."))
(defgeneric windows-underlying (win)
  (:documentation
     "* Arguments
- WIN :: a [[<Window>]]
* Returns
A list of [[<Window>]] objects, or nil.
* Description
List of all windows that both overlap with WIN and are below it in
the stack."))
(defgeneric copy-window-to-console (win con)
  (:documentation "* Arguments
- WIN :: a [[<Window>]]
- CON :: a [[console]]
* Returns
Ignored.
* Description
Copies the contents of =(window-console WIN)= onto another console, =CON=."))
(defgeneric redraw-window (win)
  (:documentation   "* Arguments
- WIN :: a [[<Window>]]
* Returns
Ignored.
* Description
Force =WIN= to copy itself onto the root console."))
(defgeneric window-redraw-at (win rootx rooty)
  (:documentation "* Arguments
- WIN :: a [[<Window>]]
- ROOTX, ROOTY :: Coordinates.
* Returns
Ignored.
* Description
Force =WIN= to copy itself into a rectangle on the root console.
The top left corner of the rectangle is at =(ROOTX, ROOTY)=."))
(defgeneric redraw-window-in-area (win x1 y1 x2 y2 &key fade)
  (:documentation "* Arguments
- WIN :: a [[<Window>]]
- X1, Y1 :: Coordinates.
- X2, Y2 :: Coordinates.
- FADE :: A float between 0.0 and 1.0, or nil.
* Returns
Ignored.
* Description
Force =WIN= to copy itself into a rectangle on the root console,
bounded by =(X1, Y1)= and =(X2, Y2)=."))
(defgeneric redraw-window-intersection (win1 win2 &key fade)
  (:documentation "* Arguments
- WIN1 :: a [[<Window>]]
- WIN2 :: a [[<Window>]]
- FADE :: A float between 0.0 and 1.0, or nil.
* Returns
Ignored.
* Description
Force =WIN1= and =WIN2= to each redraw the area where they overlap with
each other, onto the root console."))
(defgeneric redraw-intersecting-windows-below (win)
  (:documentation "TODO document."))
(defgeneric redraw-intersecting-windows-above (win)
  (:documentation "TODO document."))
(defgeneric prepare-window (win)
  (:documentation
     "Redraw window contents, but don't actually copy the window console anywhere."))
(defgeneric process-window (win)
  (:documentation "TODO document."))
(defgeneric resize-window (win width height)
  (:documentation "TODO document."))
(defgeneric mouse-drag-window (win rodent)
  (:documentation
   "* Arguments:
- WIN: an instance of {defclass dormouse:<Window>}
- RODENT: a mouse object.
* Returns: None.
* Description: Internal function, called by {defun dormouse:main-gui-loop}
when the user uses the mouse to move a window across the screen."))
(defgeneric mouse-resize-window (win rodent)
  (:documentation
   "* Arguments:
- WIN: an instance of {defclass dormouse:<Window>}
- RODENT: a mouse object.
* Returns: None.
* Description: Internal function, called by {defun dormouse:main-gui-loop}
when the user uses the mouse to resize a window."))
(defgeneric raise-window (win &key redraw &allow-other-keys)
  (:documentation
   "* Arguments:
- WIN: an instance of {defclass dormouse:<Window>}
- REDRAW: boolean value indicating whether the area of the screen occupied
by the window should be redrawn.
* Returns: None.
* Description: Put the window WIN at the top of the window stack, so that
it is displayed as overlying any other overlapping windows.
* Examples:
* See Also: "))
(defgeneric hide-window (win &key redraw &allow-other-keys)
  (:documentation
   "* Arguments:
- WIN: an instance of {defclass dormouse:<Window>}
- REDRAW: boolean value indicating whether the area of the screen occupied
by the window should be redrawn.
* Returns: None.
* Description: Hide the window WIN, making it invisible and unable to
receive events. The window is not destroyed.
* Examples:
;;; (hide-window mywin :redraw t)
* See Also: "))
(defgeneric send-to-window (win event)
  (:documentation "* Arguments:
- WIN: an instance of [[<Window>]]
- EVENT :: an instance of [[<GUI-Event>]]
* Returns: Non-nil if the event is handled, nil otherwise.
* Description: Send an event to the window WIN. The event is described by EVENT.
* Examples:
;;; (send-to-window mywin (make-instance '<GUI-Mouse-Event>
;;;                 :mouse-state (get-mouse-state))
* See Also: ")
  (:method ((win <Window>) event) (declare (ignore event)) nil)
  (:method :around ((win <Window>) (event t))
    (if (or (null (window-event-handler win))
            (not (funcall (window-event-handler win) win event)))
        (call-next-method))))
(defgeneric on-border? (win x y)
  (:documentation
   "* Arguments:
- WIN:
- X, Y: Coordinates.
* Returns:
* Description:
* Examples:
* See Also: "))
(defgeneric on-lower-window-border? (win x y)
  (:documentation "TODO document."))
(defgeneric on-upper-window-border? (win x y)
  (:documentation "TODO document."))
(defgeneric window-draw-char-at (win ch winx winy
                                &key background-flag fg bg redraw
                                &allow-other-keys)
  (:documentation "TODO document."))
(defgeneric console-draw-char-at (con ch winx winy
                             &key background-flag fg bg &allow-other-keys)
  (:documentation "TODO document."))
(defgeneric draw-string-at (win str x y
                           &key fg bg redraw &allow-other-keys)
  (:documentation
   "* Arguments:
- WIN: an instance of {defclass dormouse:<Window>}
- STR: A string, which may contain formatting directives (see below).
- X, Y: coordinates where the string should be printed, relative to
the top left corner of WIN.
- FG, BG: foreground and background colours for the string.
- REDRAW: Boolean.
* Returns: None.
* Description: Draw the string STR on the window object WIN at
position X,Y. The string STR can contain colour-changing directives - see the
documentation for {defun dormouse:make-coloured-string} for details.
* Examples:
;;; (draw-string-at mywin ``Hello, {blue}world!{/}'' 1 1 :fg :green)
* See Also:
- {defun dormouse:make-coloured-string}"))
(defgeneric colour->control-string (col background? &optional win)
  (:documentation
   "* Arguments:
- COL :: A colour specifier (keyword, number, or string.)
- BACKGROUND? :: Boolean.
- WIN :: Optional. The window on which the colour control string will
  be printed.

* Description:
Given a colour, returns a string of control codes which will change a
console's foreground colour (or background colour if BACKGROUND? is
non-nil) to the specified colour, when printed to the console."))
(defgeneric window-item-lines (win item)
  (:documentation   "Returns the number of lines that the item ITEM takes up in
the displayed list. Most items take up only 1 line."))
(defgeneric window-items-lines (win)
  (:documentation   "Returns the total number of lines needed to display all
the items in the window's list."))
(defgeneric add-item (win item str &optional k)
  (:documentation   "Add an item to the end of the window's list.  ITEM is the
'value' of the item itself. It can be any lisp value.  STR is a string
representing ITEM, which is what is displayed in the window.  HOTKEY is an
optional hotkey which quickly selects the item."))
(defgeneric clear-items (win)
  (:documentation   "Delete all items in the list belonging to WIN."))
(defgeneric draw-item-at (win item x y cursor?)
  (:documentation   "Draw list-item ITEM at WINX,WINY in the window WIN.  If
CURSOR? is non-nil, make it visually apparent that the cursor is currently
positioned over this item."))
(defgeneric window-item-at-cursor (win)
  (:documentation   "Return the list-item that is at the current cursor
position in this window (the 'cursor' moves up and down the list and always
points to one of the items in the list)."))
(defgeneric move-cursor-to-end (win)
  (:documentation "TODO document."))
(defgeneric wrap-items (win)
  (:documentation "TODO document."))
(defgeneric add-message (win fmt &rest args)
  (:documentation   "Adds the string produced by calling (APPLY #'FORMAT nil
FMT ARGS) to the end of the message list, and moves the display so that the
tail of the list is visible (including the new message)."))
(defgeneric add-message-and-redraw (win fmt &rest args)
  (:documentation   "Calls ADD-MESSAGE, then forces a redraw of WIN so
that the message is immediately visible."))
(defgeneric clear-messages (win)
  (:documentation "TODO document."))
(defgeneric clear-filter (win))
(defgeneric calculate-floating-window-width (win)
  (:documentation "TODO document."))
(defgeneric tooltip-text (win datum winx winy)
  (:documentation   "Accepts an arbitrary value. Returns nil if the datum
should not be associated with a tooltip for this window. Otherwise returns a
list of strings, which can contain colour fields.  Each string can be an
arbitrary length, as they are treated as separate messages by the floating
window."))
(defgeneric copy-map-to-viewport (win)
  (:documentation "TODO document."))
(defgeneric clear-map (win &key redraw &allow-other-keys)
  (:documentation "TODO document."))
(defgeneric centre-viewport-on (win mapx mapy)
  (:documentation "TODO document."))
(defgeneric share-map (receiver giver tlx tly)
  (:documentation "TODO document."))
(defgeneric unshare-map (receiver giver)
  (:documentation "TODO document."))
(defgeneric map-draw-char-at (win ch x y
                                &key background-flag fg bg redraw
                             &allow-other-keys)
  (:documentation   "Draw character with ASCII code CH at position MAPX, MAPY
on the map console of WIN."))
(defgeneric map-char-at (win mapx mapy)
  (:documentation
  "* Arguments:
- WIN:
- MAPX, MAPY: Coordinates on the map associated with WIN.
* Returns: A character.
* Description: Returns the character stored at (MAPX, MAPY) on the
map that is 'viewed' by the viewport WIN."))
(defgeneric get-context-at (win winx winy)
  (:documentation "TODO document."))
(defgeneric get-menu-items-for-context (win context-item)
  (:documentation "TODO document."))
(defgeneric command-from-context-menu (win command context-item)
  (:documentation "TODO document."))
(defgeneric mouse-drag (from-win to-win wfromx wfromy wtox wtoy)
  (:method ((from-win t) (to-win t) wfromx wfromy wtox wtoy)
    (declare (ignore wfromx wfromy wtox wtoy))
    nil)
  (:documentation "TODO document."))
(defgeneric windows-overlapping (win &key)
  (:documentation "TODO document."))
(defgeneric window-item-at (win winx winy)
  (:documentation   "Return the list-item that would be selected if the user
clicked on position WINX, WINY in window WIN."))
(defgeneric move-cursor-to (win cursor)
  (:documentation   "Move cursor to point to item number CURSOR in the list."))
(defgeneric cursor-moved-to-item (win item)
  (:documentation "TODO document."))
(defgeneric move-cursor-by (win increment)
  (:documentation   "Move cursor by INCREMENT items in the list. A positive
number moves further down the list, a negative number moves up."))
(defgeneric item-matches-filter-string? (win item)
  (:documentation "TODO document."))
(defgeneric ok-to-show-tooltip? (win)
  (:documentation "TODO document.")
  (:method ((win t)) nil))
(defgeneric window-page-length (win)
  (:documentation "TODO document."))
(defgeneric make-context-menu (win)
  (:documentation "TODO document."))
(defgeneric fill-to-end-of-window (win)
  (:documentation "TODO document."))
(defgeneric add-browser-line (win line)
  (:documentation "TODO document."))
(defgeneric clear-browser-lines (win)
  (:documentation "TODO document."))
(defgeneric hypertext-current-topic (win))
(defgeneric open-hypertext-topic (win topic))
(defgeneric render-input-string (win))
(defgeneric send-string-to-terminal (win &optional str))
(defgeneric window-insert-character (win ch))
(defgeneric window-delete-character (win))
(defgeneric window-backspace-character (win))
(defgeneric map-set-background-at (win colour mapx mapy &key redraw))
(defgeneric map-set-foreground-at (win colour mapx mapy &key redraw))
(defgeneric window-prompt-foreground (win/theme))
(defgeneric window-prompt-background (win/theme))
(defgeneric window-input-foreground (win/theme))
(defgeneric window-input-background (win/theme))


;;;; <<User-defined types>> ===================================================


(deftype =positive-integer= ()
  "Type consisting of all positive integers."
  `(integer 1))
(deftype =accent-specifier= ()
  "Type consisting of characters which are able to specify composition
of accented characters within strings."
  `(member #\^ #\: #\` #\' #\0))
;; (deftype =window-event-type= ()
;;   "Type for the =DATA= argument to [[SEND-TO-WINDOW]]."
;;   `(or (member :left :right :middle :wheel-up :wheel-down :select
;;                :dialog)
;;        tcod:key))


;;;; <<Constants>> ============================================================


(defconstant +OPAQUE+ 0
  "Value of =WINDOW-TRANSPARENCY= for a window that is not at all
transparent.")
(defconstant +INVISIBLE+ 100
  "Value of =WINDOW-TRANSPARENCY= for a window that is completely
transparent (invisible).")
(defconstant +DIMMED+ 75
  "Value of =WINDOW-TRANSPARENCY= for a window that is dimmed.")
(defconstant +BOLD-FACTOR+ 1.4
  "Scalar (number) by which a colour is multiplied to produce a 'bold'
version of that colour.")


;;;; <<Global variables>> =====================================================


;;;; Information about the default font file.
(defparameter *default-font-file*  "MDA9x14.png"
  "Filename of the default font file to be used by libtcod.")
;; (defparameter *default-font-file-chars-in-columns?* t
;;   "TODO document.")
;; (defparameter *default-font-file-background-colour* :true-pink
;;   "TODO document.")
(defparameter *default-font-layout*  :font-layout-ascii-in-row
  "Argument to pass to =TCOD:CONSOLE-SET-CUSTOM-FONT=, that describes the
layout of the [[*default font file*]]. Can be either a single keyword, or a
list of keywords.")

(defvar *window-stack* (list)
  "Stack (list) of all existing non-hidden windows. The 'topmost' window is at
the top of the stack.")
(defvar *hidden-windows* (list)
  "Stack (list) of all existing hidden windows. The 'topmost' window is at the
top of the stack.")
(defvar *scratch* nil
  "TCOD console used as a 'scratch' buffer.")
(defvar *temp-con* nil
  "TCOD console used as a 'scratch' buffer.")
(defvar *shift* nil
  "Global variable which is set to =T= while the shift key is being pressed,
and =NIL= while it is released.")
(defvar *ctrl* nil
  "Global variable which is set to =T= while the ctrl key is being pressed,
and =NIL= while it is released.")
(defvar *alt* nil
  "Global variable which is set to =T= while the alt/meta key is being pressed,
and =NIL= when it is released.")
(defvar *mouse-x* 0
  "Global variable set to the current absolute X-coordinate of the mouse
cursor, relative to the top left corner of the root console.")
(defvar *mouse-y* 0
  "Global variable set to the current absolute Y-coordinate of the mouse
cursor, relative to the top left corner of the root console.")
(defvar *auto-redraw* t
  "Do operations such as [[RAISE-WINDOW]] automatically copy the new window
appearance to [[*ROOT*]]?")
(defvar *drag-delay* 400
  "Delay before a down-LMB becomes a drag, in milliseconds.")
(defvar *text-format-start-char* #\{
  "Character which, when found in a string that is an argument to [[draw-string]],
heralds the beginning of a formatting instruction. The end of the instruction
is signalled by a second such character. The string between the characters must
be the name of a colour, or two colours separated by a comma.")
(defvar *text-format-end-char* #\}
  "Character which, when found in a string that is an argument to [[draw-string]],
heralds the end of an instruction that began with [[*TEXT-FORMAT-START-CHAR*]].")
(defvar *exit-gui?* nil
  "Setting this to non-nil will cause the GUI event-handling
loop to exit, and control to return to wherever the event-handling loop was
originally called from.")
(defvar *focus-changed?* nil
  "Transiently set to true when a new window has taken over the focus.")
(defvar *focus-fade-mode* :together
  "Option that controls fading behaviour of non-focussed windows.

=:TOGETHER= means that all non-background windows gain and lose opacity
together. When one of these windows has focus, all the others also become
more visible.

=:SEPARATE= means that windows gain and lose transparency individually. When
a window gains focus, /it/ becomes more opaque, but other windows do not.")


;;; <<Utility functions>> =====================================================


(defmacro translate-negative-coordinates (x y)
  "* Usage
: (translate-negative-coordinates X Y)
* Arguments
- X, Y :: Coordinates.
* Description
=X= and =Y= are PLACES holding X and Y screen coordinates. If
either of them is negative, assume they are specified relative to the bottom
right corner of the screen rather than the top left corner as is usual, and
change the coordinates stored there into normal coordinates specified relative
to the top left corner."
  `(progn
     (assert (and (integerp ,x) (integerp ,y)))
     (if (< ,x 0)
         (incf ,x (screen-width)))
     (if (< ,y 0)
         (incf ,y (screen-height)))))


(defmacro do-for-windows-by-type ((winvar wtype &key (include-hidden? nil))
                                  &body body)
  "* Usage
: (do-for-windows-by-type (WIN WTYPE &key INCLUDE-HIDDEN?)
:     ...body...)
* Arguments
- WINVAR :: a symbol.
- WTYPE :: symbol naming a subclass of [[<Window>]]
- include-hidden? :: boolean.
* Description
Iterate through all existing windows that are of type =WTYPE=,
which must be a non-quoted symbol naming a class. If =INCLUDE-HIDDEN?= is true,
iterate through hidden as well as visible windows.

Within the body of the iteration, the symbol given as =WINVAR= is bound to each
successive window.

* Example
;;; (do-for-windows-by-type (win <Message-Window>)
;;;   (draw-string-at win 1 1 ``Here is a message.''))"
  `(dolist (,winvar (if ,include-hidden? (all-windows) *window-stack*))
     (when (typep ,winvar ',wtype)
       ,@body)))



(defmacro push-end (item list-place)
  "* Usage
: (push-end ITEM LIST-PLACE)
* Arguments
- ITEM :: a value.
- LIST-PLACE :: a setf-able place, containing a list.
* Returns
A list.
* Description
Destructively appends =ITEM= to the end of the list stored in =LIST-PLACE=.
In other words, like =PUSH=, but adds to the end of the list rather than
the start."
    `(setf ,list-place (append ,list-place (list ,item))))



(defun constrain (n lower-limit upper-limit)
  "* Arguments:
- N: a number.
- LOWER-LIMIT: a number which is the lowest value that the function will return.
- UPPER-LIMIT: a number which is the highest value that the function will return.

* Description: Given a number N, Return N if the number is between LOWER-LIMIT
and UPPER-LIMIT inclusive, otherwise return whichever of LOWER-LIMIT or
UPPER-LIMIT is nearest to N.
* See Also: {defmacro dormouse:constrain!}"
  (cond
    ((< n lower-limit)
     lower-limit)
    ((> n upper-limit)
     upper-limit)
    (t n)))



(defmacro constrain! (place lower-limit upper-limit)
  "* Arguments:
- PLACE: a setf-able place that contains a number.
- LOWER-LIMIT: a number which is the lowest value that the function will return.
- UPPER-LIMIT: a number which is the highest value that the function will return.

* Description: Destructively modifies PLACE, replacing its value with
the value returned by (CONSTRAIN PLACE LOWER-LIMIT UPPER-LIMIT).
* See Also: {defun dormouse:constrain}"
  `(setf ,place (constrain ,place ,lower-limit ,upper-limit)))



(defun keyword->string (kwd)
  "Given :MY-KEYWORD, returns the string 'MY-KEYWORD'."
  (string-trim '(#\:) (format nil "~A" kwd)))


 (defun make-keyword (&rest parts)
    "Concatenate the parts and intern as a symbol in the KEYWORD package,
creating a keyword called :SYMBOL."
    (intern (string-upcase (format nil "~{~A~}" parts)) :keyword))


(defun string-tokenise (bag str)
  "* Arguments:
- BAG: list of characters.
- STR: A string

* Returns: A list of strings.

* Description: Divides the string STR wherever any of the characters
in BAG occur within it. The resulting substrings will not contain the
characters in BAG. Returns the resulting list of substrings."
  (let ((tokens nil)
        (found-pos nil))
    (check-type str string)
    (setf str (string-trim bag str))
    (iterate
      (while (> (length str) 0))
      (setf found-pos
            (car
             (sort
              (remove nil
                      (mapcar #'(lambda (c) (position c str)) bag))
              #'<)))
      (when found-pos
        (push (subseq str 0 found-pos) tokens)
                                        ;(setf found-ch t)
        (setf str (string-left-trim bag
                                    (subseq str found-pos nil))))
      (unless (or found-pos
                  (= 0 (length str)))
        (push str tokens)
        (setf str "")))
    (nreverse tokens)))



(defun spaces (n)
  "* Arguments:
- N: a positive integer.
* Returns: A string of N spaces.
* Description: Given an integer N, return a string of spaces N characters
  long."
  (make-sequence 'string n :initial-element #\space))


(defun centre-string (str width)
  "Returns string =STR= padded by spaces on either side, so that the length
of the returned string is at least =WIDTH=."
  (check-type str string)
  (check-type width (integer 0))
  (cond
    ((>= (length str) width)
     str)
    (t
     (concatenate 'string
                  (spaces (floor (- width (length str)) 2))
                  str
                  (spaces (ceiling (- width (length str)) 2))))))




(defun word-wrap (text &key (width 80) respect-newlines respect-hyphens
                  exclude-start-char exclude-end-char)
  "* Usage
: (word-wrap TEXT &key WIDTH RESPECT-NEWLINES RESPECT-HYPHENS
:    EXCLUDE-START-CHAR EXCLUDE-END-CHAR)
* Arguments
- TEXT :: A string.
- WIDTH :: An integer. The maximum length of lines once TEXT is wrapped.
Default is 80.
- RESPECT-NEWLINES :: Boolean. Should newline characters within the string
be treated as unbreakable? (=NIL=)
- RESPECT-HYPHENS :: Boolean. Should we refrain from breaking hyphenated
words? (=NIL=)
- EXCLUDE-START-CHAR :: A character, or nil.
- EXCLUDE-END-CHAR :: A character, or nil.

* Return Value
A list of strings.

* Description
Given a string =TEXT=, breaks the string into a series of
smaller strings, none of which is longer than =WIDTH=. Returns the list of
strings.

If =EXCLUDE-START-CHAR= and =EXCLUDE-END-CHAR= are supplied, those characters
will be treated as demarcating sections of the string whose length is to
be ignored (treated as zero)."
  (iterate
    (with counted = 0)
    (with breakpoint = nil)
    (with skipping = nil)
    (for c :in-string text)
    (for actual :upfrom 0)
    (cond
      ((eql c exclude-start-char)
       (setf skipping t))
      ((eql c exclude-end-char)
       (setf skipping nil)))
    (when (not skipping)
      (incf counted)
      (if (or (eql c #\space) (eql c #\tab)
              (and (eql c #\Newline) (not respect-newlines))
              (and (eql c #\-) (not respect-hyphens)))
          (setf breakpoint actual))
      (when (and (eql c #\Newline) respect-newlines)
        (setf breakpoint actual)
        (setf counted (1+ width))))
    (when (>= counted width)
      (return (cons (substitute-if #\space
                                   #'(lambda (ch)
                                       (or (eql ch #\tab)
                                           (eql ch #\newline)))
                                   (subseq text 0
                                           (or breakpoint actual)))
                    (word-wrap (subseq text (if breakpoint
                                                (1+ breakpoint)
                                                actual))
                               :width width
                               :respect-newlines respect-newlines
                               :respect-hyphens respect-hyphens
                               :exclude-start-char exclude-start-char
                               :exclude-end-char exclude-end-char))))
    (finally (return (list text)))))


(defun all-hashtable-keys (hashtable)
  "Return a list of every key in HASHTABLE."
  (iterate
    (for (key nil) in-hashtable hashtable)
    (collect key)))



;;; <<Events>> ================================================================


(defclass <GUI-Event> ()
  ((gui-event-type :accessor gui-event-type :initform nil :initarg :event-type)
   (gui-event-window :accessor gui-event-window :initform nil :initarg :window)
   (gui-event-winx :accessor gui-event-winx :initform nil :initarg :winx)
   (gui-event-winy :accessor gui-event-winy :initform nil :initarg :winy)))


(defclass <Key-Event> (<GUI-Event>)
  ((gui-event-type :initform :keypress)
   (gui-event-keypress :accessor gui-event-keypress
                       :type (or null tcod:key) :initform nil
                       :initarg :keypress)))


(defclass <Mouse-Event> (<GUI-Event>)
  ((gui-event-type :initform :mouse)
   (gui-event-mouse-state :accessor gui-event-mouse-state
                          :initform nil :initarg :mouse-state)))


(defclass <GUI-Mouse-Drag-Event> (<Mouse-Event>)
  ((gui-event-type :initform :mouse-drag)))


(defclass <Mouse-Hover-Event> (<Mouse-Event>)
  ((gui-event-type :initform :mouse-hover)
   (gui-event-focus :initform nil :accessor gui-event-focus :initarg :focus
                    :documentation "If the mouse is hovering over some particular
item or value, it can be stored here.")))


(defclass <GUI-Dialog-Event> (<GUI-Event>)
  ((gui-event-type :initform :dialog)
   (gui-event-string :initform nil :accessor gui-event-string :initarg :string)))


(defclass <GUI-Select-Event> (<GUI-Event>)
  ((gui-event-type :initform :select)
   (gui-event-focus :initform nil :accessor gui-event-focus :initarg :focus)))


;;; <<Keys>> ==================================================================


(defun character->vk (ch)
  "Given a character =CH=, return the value of the 'VK' field that is expected
when the key for that character is pressed by the user."
  (case ch
    (#\space
     :space)
    ((#\1 #\!)
     :key-1)
    ((#\2 #\@)
     :key-2)
    ((#\3 #\#)
     :key-3)
    ((#\4 #\$)
     :key-4)
    ((#\5 #\%)
     :key-5)
    ((#\6 #\^)
     :key-6)
    ((#\7 #\&)
     :key-7)
    ((#\8 #\*)
     :key-8)
    ((#\9 #\()
     :key-9)
    ((#\0 #\))
     :key-0)
    (otherwise
     :char)))


(defun character->shift (ch)
  "Given a character CH, return the value of the 'SHIFT' field that is expected
when the key for that character is pressed by the user."
  (if (or (upper-case-p ch)
          (find ch '(#\~ #\! #\@ #\# #\$ #\% #\^ #\& #\*
                     #\( #\) #\_ #\+ #\{ #\} #\| #\: #\"
                     #\< #\> #\?)))

      t nil))


(defun binding->key (binding)
  "BINDING is a list of the form:
  (KEYCODE [:ctrl CTRL] [:alt ALT] [:shift SHIFT])
Where:
  -- KEYCODE is a character or a VK code
  -- SHIFT, CTRL and ALT are boolean values
Return the TCOD key structure that we expect to be produced when the key
combination described by BINDING is pressed."
  (destructuring-bind (keycode &key ctrl shift alt) binding
      (tcod:make-key
       :vk (if (characterp keycode)
               (character->vk keycode)
               keycode)
       :c (if (characterp keycode)
              keycode
              #\null)
       :pressed t
       :shift (if (not (characterp keycode))
                  shift
                  (character->shift keycode ))
       :lctrl ctrl :rctrl ctrl :lalt alt :ralt alt)))


(defun key->string (k)
  "Return a string that describes the key combination represented by the TCOD
key structure, K, in human-readable form."
  (check-type k key)
  (concatenate 'string
               (format nil "~A~A~A"
                       (if (key-shift k) "shift-" "")
                       (if (or (key-rctrl k) (key-lctrl k)) "ctrl-" "")
                       (if (or (key-ralt k) (key-lalt k)) "alt-" ""))
               (case (key-vk k)
                 (:key-0 "0")
                 (:key-1 "1")
                 (:key-2 "2")
                 (:key-3 "3")
                 (:key-4 "4")
                 (:key-5 "5")
                 (:key-6 "6")
                 (:key-7 "7")
                 (:key-8 "8")
                 (:key-9 "9")
                 (:char
                  (format nil "~C" (key-c k)))
                 (otherwise
                  (format nil "~A" (key-vk k))))))


(let ((key-event nil))
  (defun send-key-to-window (win key winx winy)
    "Return non-nil if the key is handled, nil if not handled."
    (unless key-event
      (setf key-event (make-instance '<Key-Event>)))
    (setf (gui-event-keypress key-event) key
          (gui-event-winx key-event) winx
          (gui-event-winy key-event) winy
          (gui-event-window key-event) win)
    (send-to-window win key-event)))


;;; <<Colourising strings>> ===================================================


(defun make-coloured-string (str &key (dialog? nil) (win nil))
  "* Usage
: (make-coloured-string STR &key DIALOG? WIN)
* Arguments
- STR :: A string.
- DIALOG? :: boolean.
- WIN :: a [[<Window>]] or nil.
* Returns
A string.
* Description

=STR= is a string that may contain formatting directives. Each directive is
enclosed within pairs of the characters [[*TEXT-FORMAT-START-CHAR*]] and
[[*TEXT-FORMAT-END-CHAR*]] (these are `{'and `}' by
default.). =MAKE-COLOURED-STRING= returns a string that is the same as =STR=,
except that formatting directives have been replaced with control characters
that libtcod will understand.

The directives can be used to change the colour in which the string is printed.
: {COLOURNAME} -- set foreground colour to COLOURNAME.
: {bg:COLOURNAME} -- set background colour to COLOURNAME.
: {fg:COLOURNAME,bg:COLOURNAME} -- change foreground and background colours.

Examples of colour directives:
: {green} -- change foreground colour to :GREEN
: {fg:green,bg:dark-blue} -- change foreground to :GREEN and background to
:                            :DARK-BLUE
: {bg:yellow} -- change background to :YELLOW

The delimiters can also be used to 'compose' accented characters:
: {:a} - output a lowercase a with diaeresis.
: {'a} - output a lowercase a with acute accent.
: {`a} - output a lowercase a with grave accent.
: {^a} - output a lowercase a with circumflex accent.
: {0a} - output a lowercase a with 'o' accent.

The directives can be used to mark parts of the string as 'clickable', like
dialog buttons:
: {click:MYLABEL}Click me!{/}

Finally:
: {/} - return to default foreground and background colours for this window.
: {{ - output a single `{'.
: }} - output a single `}'.

* Examples
;;;   \"Lisp is the {red}red{/} pill.\"
;;;   \"G{'i}mli, son of Glo{'i}n\"
"
  (let ((pos@ nil)
        (pos@@ 0))
    (setf pos@ (position *text-format-start-char* str))
    (cond
      ((null pos@)
       str)
      ((and (> (length str) 1)
            (member (char str 0) (list *text-format-start-char*
                                       *text-format-end-char*))
            (eql (char str 0) (char str 1)))
       (format nil "~C~A" (char str 0)
               (make-coloured-string (subseq str 2)
                                     :dialog? dialog? :win win)))
      ((> pos@ 0)
       (concatenate 'string (subseq str 0 pos@)
                    (make-coloured-string (subseq str pos@)
                                          :dialog? dialog? :win win)))
      ((null (setf pos@@ (position *text-format-end-char*
                                   (subseq str 1))))
       (warn "Missing format-end character `~C' in string ~s"
             *text-format-end-char* str)
       str)
      (t
       (incf pos@@)
       (concatenate 'string
                    (if dialog?
                        (string->dialog-codes (subseq str 1 pos@@))
                        ;; else
                        (string->tcod-colour-codes (subseq str 1 pos@@)
                                                   win))
                    (if (>= pos@@ (length str))
                        ""
                        (make-coloured-string
                         (subseq str (+ 1 pos@@)) :dialog? dialog?
                         :win win)))))))


(defmethod colour->control-string ((col integer) background?
                                   &optional win)
  (declare (ignore win))
  (multiple-value-bind (r g b) (decompose-colour col)
    (format nil "~C~C~C~C"
            (colctrl->char (if background? :COLCTRL-BACK-RGB :COLCTRL-FORE-RGB))
            (code-char (max r 1))
            (code-char (max g 1))
            (code-char (max b 1)))))


(defmethod colour->control-string ((col symbol) background?
                                   &optional win)
  (colour->control-string (colour col) background? win))


(defmethod colour->control-string ((col string) background?
                                   &optional win)
  (cond
    ((and win
          (find (string-upcase col) '("B" "BOLD") :test #'string=))
     (colour->control-string
      (tcod:colour-multiply-scalar
       (tcod:colour (if background? (window-background win)
                        (window-foreground win)))
       +BOLD-FACTOR+) background? win))
    (t
     (colour->control-string (string->colournum col) background? win))))


(defun string->tcod-colour-codes (str &optional win)
  "Given the contents of a formatting directive STR (a string), return the TCOD
control codes that are needed to use the colours specified in the string.

A formatting directive is a series of terms separated by commas. Each term is
either the single character '/', or the name of a colour, or the name of a
colour prefixed by one of the labels FOREGROUND:, BACKGROUND:, FG:, or BG:."
  (with-output-to-string (s)
    (cond
      ((string= str "/")
       (cond
         (win
         ;; Just outputting COLCTRL-STOP seems not to work if the stop code
         ;; is a different function call from the original colour-changing
         ;; codes. It seems that if the colours are changed within a string
         ;; and not changed back, then the new colours become the new default
         ;; colours for the console.
         ;; Therefore, if WIN is supplied, reset the console colours to the
         ;; remembered default colours for the window object, prior to
         ;; outputting COLCTRL-STOP.
          (format s (colour->control-string (window-foreground win) nil win))
          (format s (colour->control-string (window-background win) t win)))
         (t
          (format s "~C" (colctrl->char :COLCTRL-STOP)))))
       ;;   (console-set-foreground-colour (window-console win)
       ;;                                  (colour (window-foreground win)))
       ;;   (console-set-background-colour (window-console win)
       ;;                                  (colour (window-background win))))
       ;; (format s "~C" (colctrl->char :COLCTRL-STOP)))
      ((and (= 2 (length str))
            (member (char str 0) '(#\^ #\: #\' #\`))
            (member (char str 1) '(#\A #\E #\I #\O #\U #\a #\e #\i #\o #\u)))
       (format s "~C" (compose-accented-char (char str 1) (char str 0))))
      (t
       (let* ((props (string->properties str :foreground))
              (foreground (or (getf props :foreground)
                              (getf props :fg)))
              (background (or (getf props :background)
                              (getf props :bg))))
         (when foreground
           (format s (colour->control-string foreground nil win)))
           ;; (multiple-value-bind
           ;;       (fr fg fb) (decompose-colour (string->colournum foreground))
           ;;   (format s "~C~C~C~C"
           ;;           (colctrl->char :COLCTRL-FORE-RGB)
           ;;           (code-char (max fr 1))
           ;;           (code-char (max fg 1))
           ;;           (code-char (max fb 1)))))
         (when background
           (format s (colour->control-string background t win))))))))
           ;; (multiple-value-bind
           ;;       (br bg bb) (decompose-colour (string->colournum background))
           ;;   (format s "~C~C~C~C"
           ;;           (colctrl->char :COLCTRL-BACK-RGB)
           ;;           (code-char (max br 1))
           ;;           (code-char (max bg 1))
           ;;           (code-char (max bb 1))))))))))


(defun compose-accented-char (ch accent)
  "* Arguments:
- CH -- an alphabetic character.
- ACCENT -- another character which must be a member of the set #\^, #\:, #\`,
#\', #\0.
-- #\^ -- circumflex
-- #\: -- diaeresis
-- #\` -- grave
-- #\' -- acute
-- #\0 -- ring (small 'o' as seen above Scandinavian 'a')

* Returns:
The ASCII character for CH accented according to ACCENT.

* Examples:
;;; (compose-accented-char #\a #\')  ; returns a with acute accent (ASCII 160)."
  (declare (character ch) (type =accent-specifier= accent))
  ;; todo this may break with some other character sets.
  (case accent
    (#\^
     ;; ^a 131 ^e 136 ^i 140 :A 142 ^o 147 ^u 150
     (case ch
       (#\a (code-char 131))
       (#\e (code-char 136))
       (#\i (code-char 140))
       (#\o (code-char 147))
       (#\u (code-char 150))
       (#\A (code-char 142))
       (otherwise ch)))
    (#\:
     ;; :u 129 :a 132 :e 137 :i 139 :o 148 :O 153 :U 154
     (case ch
       (#\a (code-char 132))
       (#\e (code-char 137))
       (#\i (code-char 139))
       (#\o (code-char 148))
       (#\u (code-char 129))
       (#\O (code-char 153))
       (#\U (code-char 154))
       (otherwise ch)))
    (#\`  ;; \
     ;; `a 133 `e 138 `i 141 `o 149 `u 151
     (case ch
       (#\a (code-char 133))
       (#\e (code-char 138))
       (#\i (code-char 141))
       (#\o (code-char 149))
       (#\u (code-char 151))
       (otherwise ch)))
    (#\'  ;; /
     ;; 'e 130 'E 144 'a 160 'i 161 'o 162 'u 163
     (case ch
       (#\a (code-char 160))
       (#\e (code-char 130))
       (#\i (code-char 161))
       (#\o (code-char 162))
       (#\u (code-char 163))
       (#\E (code-char 144))
       (otherwise ch)))
    (#\0
     ;; 0a 134 0A 143
     (case ch
       (#\a (code-char 134))
       (#\A (code-char 143))
       (otherwise ch)))
    (otherwise
     ch)))


(defun string->dialog-codes (str)
  "Similar to [[string->tcod-colour-codes]], but deals only with
the directives that create clickable 'dialog buttons' within a string.

STR is a string containing a series of terms separated by commas. Each term
is either the single character '/', or the name of a colour, or the name of a
colour prefixed by one of the labels FOREGROUND:, BACKGROUND:, FG:, or BG:, or
a label prefixed by one of the labels BUTTON:, BTN: or CLICK:."
  (with-output-to-string (s)
    (cond
      ((string= str "/")
       (format s "~C" (colctrl->char :COLCTRL-STOP)))
      (t
       (let* ((props (string->properties str :foreground))
              (click-label (or (getf props :button)
                               (getf props :btn)
                               (getf props :click))))
         (when click-label
           (multiple-value-bind
                 (fr fg fb) (decompose-colour
                             (string->dialog-colour click-label))
             (format s "~C~C~C~C"
                     (colctrl->char :COLCTRL-FORE-RGB)
                     (code-char (max fr 1))
                     (code-char (max fg 1))
                     (code-char (max fb 1))))))))))


(defun string->properties (str &optional (default-label :unlabelled))
  "Given a string found within a pair of TEXT-FORMAT-CHARs, break it
down into a property list.
The string is considered to be a list of terms separated by commas.
Each term may be prefixed with a label and ':', in which case the label
becomes the keyword associated with the rest of the term in the returned
property list."
  (declare (string str))
  (let ((properties nil))
    (dolist (term (string-tokenise '(#\,) str))
      (cond
        ((and (position #\: term)
              (> (position #\: term) 0))
         (setf (getf properties
                     (make-keyword (subseq term 0 (position #\: term))))
               (subseq term (1+ (position #\: term)))))
        (t                              ; default - no label given
         (setf (getf properties default-label) term))))
    properties))



(defun string->colournum (str)
  "Given a string STR, return the colournum that is associated with the
corresponding keyword."
  (or (colour (make-keyword str))
      0))


(defun coloured-string-length (str)
  "Return the number of printable characters in the string STR. In other words,
return the length of STR when formatting directives are excluded."
  (iterate
    (with cnt = 0)
    (with counting = t)
    (for c :in-string str)
    (cond
      (counting
       (if (eql c *text-format-start-char*)
           (setf counting nil)
           ;; else
           (incf cnt)))
      ((eql c *text-format-end-char*)
       (setf counting t))
      (t nil))
    (finally (return cnt))))


(defun right-trim-coloured-string (str n)
  "Return the N rightmost characters of string STR, ignoring fields
surrounded by { and }."
  (let ((len (coloured-string-length str)))
    (cond
      ((<= len n)
       str)
      (t
       (iterate
         (with cnt = 0)
         (with c = nil)
         (with counting = t)
         (for actual :from (1- (length str)) :downto 0)
         (setf c (char str actual))
         (cond
           (counting
            (cond
              ((eql c *text-format-end-char*)
               (setf counting nil))
              ;; else
              (t
               (incf cnt))))
           ((eql c *text-format-start-char*)
            (setf counting t))
           (t nil))
         (when (and counting (>= cnt (- len n)))
           (return (subseq str actual)))
         (finally (return str)))))))


(defun left-trim-coloured-string (str n)
  "Return the N leftmost characters of string STR, ignoring fields
surrounded by { and }."
  (let ((len (coloured-string-length str)))
    (cond
      ((<= len n)
       str)
      (t
       (iterate
         (with cnt = 0)
         (with counting = t)
         (for c :in-string str)
         (for actual :from 0)
         (cond
           (counting
            (cond
              ((eql c *text-format-start-char*)
               (setf counting nil))
              ;; else
              (t
               (incf cnt))))
           ((eql c *text-format-end-char*)
            (setf counting t))
           (t nil))
         (when (and counting (>= cnt n))
           (return (subseq str 0 (1+ actual))))
         (finally (return str)))))))



(defun colourise (val fg &optional bg)
  "Given a value, return it as a string wrapped in directives for
  {defgeneric dormouse:write-to-window-at} which will make the value appear in
  colours FG[,BG] when written to the screen."
  (cond
    (bg
     (format nil "{fg:~A,bg:~A}~A{/}"
             (keyword->string fg)
             (keyword->string bg)
             val))
    (t
     (format nil "{fg:~A}~A{/}"
             (keyword->string fg)
             val))))



(defun bar-chart (width num denom
                  &key (text nil)
                  (bar-colour :red) (empty-bar-colour :black)
                  (text-colour :white))
  "Returns a colourised string which, when printed using
  draw-string-at, will produce a string of solid blocks
  WIDTH characters long, coloured BAR-COLOUR for NUM/DENOM * the string's
  length, and EMPTY-BAR-COLOUR for the rest of the string.

If TEXT is supplied, then some text will appear in the centre of the bar chart,
with a foreground colour of TEXT-COLOUR. Possible values for TEXT are:

- A string
- :FRACTION - ``NUM/DENOM''
- :PERCENT - a percentage calculated from NUM/DENOM * 100
- nil (default) - no text.

* Example:
;;; (bar-chart 20 (hit-points *player*) (max-hit-points *player*)
;;;    :text :fraction :text-colour :cornsilk) "
  (declare (type (integer 1) width) (real num denom))
  (when (zerop denom)
    (setf denom 1))
  (let ((bar
         (cond
           ((eql text :fraction)
            (centre-string (format nil "~D/~D" num denom) width))
           ((eql text :percent)
            (centre-string (format nil "~D%" (floor (* num 100) denom)) width))
           ((stringp text)
            (centre-string text width))
           (t   ;; just a bar
            (spaces width))))
        (filled-spaces (round (* width (/ (constrain num 0 denom) denom)))))
    (concatenate 'string
                 "{fg:" (keyword->string text-colour)
                 ",bg:" (keyword->string bar-colour) "}"
                 (subseq bar 0 filled-spaces)
                 (colourise (subseq bar filled-spaces) text-colour
                            empty-bar-colour))))



;;; <<Window class>> ==========================================================


(defclass <Window> ()
  ((window-tlx :initform 0 :accessor window-tlx :type integer
               :initarg :tlx :documentation "X-coordinate of top left corner of
the window. If a negative number is given, then the coordinate is calculated
relative to the bottom right corner of the screen.")
   (window-tly :initform 0 :accessor window-tly :type integer
               :initarg :tly :documentation "Y-coordinate of top left corner of
the window. If a negative number is given, then the coordinate is calculated
relative to the bottom right corner of the screen.")
   (window-width :initform 0 :accessor window-width :type =positive-integer=
                 :initarg :width :documentation "Width of the window in
                 columns.")
   (window-height :accessor window-height :initform 0 :type =positive-integer=
                  :initarg :height :documentation "Height of the window in
                  rows.")
   (window-console :accessor window-console :initform nil :initarg :console
                   :documentation "Contains the external C console pointer for
                   the window.")
   (window-foreground :writer (setf window-foreground)
                      :type keyword
                      :initarg :foreground
                      :documentation
                      "Default foreground colour for the window.")
   (window-background :writer (setf window-background)
                      :type keyword
                      :initarg :background
                      :documentation
                      "Default background colour for the window.")
   (window-children :accessor window-children :initform nil
                    :type list :initarg :children
                    :documentation "List of windows which are
'dependent' on this window. They are hidden, unhidden and
destroyed along with the window. Also see `WINDOW-RAISE-CHILDREN-WITH-PARENT?'
below.")
   (window-raise-children-with-parent?
    :accessor window-raise-children-with-parent? :initform t
    :type boolean :documentation  "Are child windows to be raised and
hidden when the same is done to the parent?")
   (window-auto-redraw? :accessor window-auto-redraw? :initform nil
                        :type boolean
                        :documentation "Should this window automatically redraw
itself if it finds WINDOW-CHANGED? is non-nil?")
   (window-framed? :writer (setf window-framed?)
                   :type boolean :initarg :framed?
                   :documentation "Should a frame be drawn in the edges of the
                   window?")
   (window-can-resize? :accessor window-can-resize? :initform t :type boolean
                       :initarg :can-resize? :documentation "Can window be
resized by dragging on the lower right corner with the mouse?")
   (window-can-drag? :accessor window-can-drag? :initform t :type boolean
                     :initarg :can-drag?
                     :documentation "Can the window be dragged around
the screen with the mouse?")
   (window-can-close? :accessor window-can-close? :initform t :type boolean
                      :initarg :can-close?
                      :documentation "Can the window be closed by clicking on
an 'X' in its top right corner?")
   (window-ephemeral? :accessor window-ephemeral? :initform nil
                      :type boolean :initarg :ephemeral?
                      :documentation "Ephemeral windows are destroyed
upon being hidden.")
   (window-draw-function :accessor window-draw-function :initform nil
                         :type (or null function) :initarg :draw
                         :documentation "Function taking one argument -- the
window -- that is called to 'fill in' the interior of the window when it is
drawn or redrawn.")
   (window-event-handler :accessor window-event-handler
                         :initform nil
                         :initarg :event-handler
                         :documentation "Must be either nil or a Function
accepting two arguments:
WIN and EVENT. Called when an input event is to be sent to the
window. WIN is the window in question, EVENT is an instance of GUI-EVENT.
The function must return non-nil if the event was 'handled', nil if it was
not handled and should be passed on to other possible handlers.")
   (window-title :accessor window-title :initform nil
                 :type (or null string) :initarg :title
                 :documentation "Title of the window.")
   (window-transparency :writer (setf window-transparency)
                        :type (integer 0 100)
                        :initarg :transparency
                        :documentation "Amount of transparency of the window,
from 0-100, where 100 = invisible.")
   (window-transparency-unfocussed
    :writer (setf window-transparency-unfocussed)
    :type (or null (integer 0 100)) :initarg :transparency-unfocussed
    :documentation "When this window does not have the focus, what does its
transparency become? (NIL = no change in transparency)")
   ;; Internal bookkeeping slots, usually not accessed by the user.
   (window-hidden? :accessor window-hidden? :initform nil :type boolean
                   :documentation "Is this window hidden?")
   (window-changed? :accessor window-changed? :initform nil :type boolean
                    :documentation "Can be set to T if the window needs to be
updated, as a signal to process-window.")
   (window-alive? :accessor window-alive? :initform t :type boolean
                  :documentation "Set to nil when a window has been destroyed.")
   (window-touching :accessor window-touching :initform (list) :type list
                    :documentation "List of windows that overlap this window."))
  (:documentation   "* Description: Class whose instances represent windows on
the screen.
* Examples: TODO
* See Also: "))


;;; <<Window Themes>> =========================================================


(defclass <Window-Theme> ()
  (;; Colours =================================================================
   (window-foreground :accessor window-foreground :initform :white
                      ;;:type keyword
                      :initarg :foreground
                      :documentation
                      "Default foreground colour for window contents.")
   (window-background :accessor window-background
                      :initform :dark-slate-gray :type keyword
                      :initarg :background
                      :documentation
                      "Default background colour for window contents.")
   (hyperlink-foreground-colour
    :initform :light-blue :type keyword :initarg :hyperlink-fg
    :accessor hyperlink-foreground-colour)
   (window-prompt-foreground :initarg :prompt-fg
                             :writer (setf window-prompt-foreground)
                             :documentation "Foreground colour for prompts in
Terminal Windows. Defaults to WINDOW-FOREGROUND.")
   (window-prompt-background :initarg :prompt-bg
                             :writer (setf window-prompt-background))
   (window-input-foreground :initarg :input-fg
                            :writer (setf window-input-foreground)
                            :documentation "Foreground colour for input typed
by the user in Terminal Windows. Defaults to WINDOW-FOREGROUND.")
   (window-input-background :initarg :input-bg
                            :writer (setf window-input-background))
   ;; Other settings ==========================================================
   (window-framed? :accessor window-framed? :initform t
                   :type boolean :initarg :framed?
                   :documentation "Should a frame be drawn around the edges of
                   windows?")
   (window-transparency :accessor window-transparency :initform 25
                        :type (integer 0 100)
                        :initarg :transparency
                        :documentation "Amount of window transparency,
from 0-100, where 100 = invisible.")
   (window-transparency-unfocussed
    :accessor window-transparency-unfocussed :initform +DIMMED+
    :type (or null (integer 0 100))
    :initarg :transparency-unfocussed
    :documentation "When windows do not have the focus, what does their
transparency become? (NIL = no change in transparency)"))
  (:documentation
   "Object containing global appearance information, which all windows
will use unless overridden."))


(defvar *default-window-theme* (make-instance '<Window-Theme>))
(defvar *window-theme* *default-window-theme*)


;; Readers for Window slots that override Theme defaults.


(defmethod window-foreground ((win <Window>))
  (if (slot-boundp win 'window-foreground)
      (slot-value win 'window-foreground)
      (window-foreground *window-theme*)))

(defmethod window-background ((win <Window>))
  (if (slot-boundp win 'window-background)
      (slot-value win 'window-background)
      (window-background *window-theme*)))

(defmethod window-framed? ((win <Window>))
  (if (slot-boundp win 'window-framed?)
      (slot-value win 'window-framed?)
      (window-framed? *window-theme*)))

(defmethod window-transparency ((win <Window>))
  (if (slot-boundp win 'window-transparency)
      (slot-value win 'window-transparency)
      (window-transparency *window-theme*)))

(defmethod window-transparency-unfocussed ((win <Window>))
  (if (slot-boundp win 'window-transparency-unfocussed)
      (slot-value win 'window-transparency-unfocussed)
      (window-transparency-unfocussed *window-theme*)))

;;; Readers for window theme slots, allowing these slots to
;;; default to FG/BG values.

(defmethod window-prompt-foreground ((theme <Window-Theme>))
  (if (slot-boundp theme 'window-prompt-foreground)
      (slot-value theme 'window-prompt-foreground)
      (window-foreground theme)))

(defmethod window-prompt-background ((theme <Window-Theme>))
  (if (slot-boundp theme 'window-prompt-background)
      (slot-value theme 'window-prompt-background)
      (window-background theme)))

(defmethod window-input-foreground ((theme <Window-Theme>))
  (if (slot-boundp theme 'window-input-foreground)
      (slot-value theme 'window-input-foreground)
      (window-foreground theme)))

(defmethod window-input-background ((theme <Window-Theme>))
  (if (slot-boundp theme 'window-input-background)
      (slot-value theme 'window-input-background)
      (window-background theme)))


(defun screen-width ()
  "* Arguments: None.
* Returns: Integer.
* Description: Returns the width of the screen (root console) in columns."
  (tcod:console-get-width tcod:*root*))


(defun screen-height ()
  "* Arguments: None.
* Returns: Integer.
* Description: Returns the height of the screen (root console) in rows."
  (tcod:console-get-height tcod:*root*))



(defmethod initialize-instance :after ((win <Window>) &key (hidden? nil)
                                       &allow-other-keys)
  (setf (window-console win)
        (console-new (window-width win) (window-height win)))
  (console-set-default-foreground (window-console win)
                                 (colour (window-foreground win)))
  (console-set-default-background (window-console win)
                                 (colour (window-background win)))
  (console-set-background-flag (window-console win) :set)
  ;; Translate negative numbers for TLX,TLY to be relative to the
  ;; bottom right corner of the screen.
  (translate-negative-coordinates (window-tlx win) (window-tly win))
  (cond
    (hidden?
     (setf (window-hidden? win) t)
     (push win *hidden-windows*))
    (t
     (touch-windows win)
     ;; put WIN onto stack
     (push win *window-stack*))))



(defmethod process-window ((win <Window>))
  (unless (window-hidden? win)
    (cond
      ((and (window-auto-redraw? win)
            (window-changed? win))
       (prepare-window win)
       (redraw-window-area win :draw-window t)
       (dirty-window win)   ; (console-flush)
       (setf (window-changed? win) nil))
      (*focus-changed?*
       (redraw-window-area win :draw-window t)))))


(defmethod dirty-window ((win <Window>))
  (console-set-dirty (window-tlx win) (window-tly win)
                     (window-width win) (window-height win)))


(defmethod window-parent ((win <Window>))
  ;; TODO inefficient
  (find-if #'(lambda (parent) (member win (window-children parent)))
                (all-windows)))


(defmethod destroy-window ((win <Window>))
  (if (find win *window-stack*)
      (hide-window win))
  (when (and (window-console win)
             (not (equal *root* (window-console win)))))
  ;; If it has a parent, remove it from parent's list.
  (let ((parent (window-parent win)))
    (if parent
        (setf (window-children parent)
              (remove win (window-children parent)))))
  ;; If it has children, remove them
  (when (window-children win)
    (dolist (child (window-children win))
      (destroy-window child)))
  (setf *hidden-windows* (remove win *hidden-windows*))
  (setf (window-alive? win) nil))




(defun destroy-all-windows ()
  "* Arguments: None.
* Returns: None.
* Description: Destroy all existing window objects."
  (iterate
    (with wins = nil)
    (while (setf wins (all-windows)))
    (destroy-window (car wins))))


(defstruct window-configuration
  (visible-windows nil)
  (hidden-windows nil))


(defun save-interface-state ()
  "* Returns:
A WINDOWS-CONFIGURATION struct, which contains information sufficient to
restore the interface to its current state in future via a call to
`restore-interface-state'.

 Intended to be called if we want to make substantial changes to the interface
setup, but might later want to restore the current window setup."
  (make-window-configuration :visible-windows (copy-list *window-stack*)
                             :hidden-windows (copy-list *hidden-windows*)))


(defun restore-interface-state (state)
  "* Arguments:
- STATE :: A WINDOWS-CONFIGURATION struct, usually one created previously
by a call to `save-interface-state'.
* Returns:
A new WINDOWS-CONFIGURATION struct, containing information about the overwritten
interfact state."
  (let ((cfg (save-interface-state)))
    (setf *window-stack* (copy-list (window-configuration-visible-windows state)))
    (setf *hidden-windows* (copy-list (window-configuration-hidden-windows state)))
    (dolist (win (all-windows))
      (dirty-window win))
    cfg))


(defun destroy-interface-state (state)
  (dolist (win (window-configuration-visible-windows state))
    (destroy-window win))
  (dolist (win (window-configuration-hidden-windows state))
    (destroy-window win))
  (setf (window-configuration-visible-windows state) nil
        (window-configuration-hidden-windows state) nil)
  state)


(defmethod touch-windows ((win <Window>))
  (dolist (other *window-stack*)
    (when (and (not (equal other win))
             (windows-touching? win other))
        (pushnew other (window-touching win))
        (pushnew win (window-touching other)))))


(defmethod untouch-windows ((win <Window>))
        (dolist (other (window-touching win))
                (setf (window-touching other)
                        (remove win (window-touching other) :test #'equal)))
        (setf (window-touching win) (list)))


(defmethod move-window ((win <Window>) tlx tly)
  (translate-negative-coordinates tlx tly)
  (untouch-windows win)
  (setf (window-tlx win) tlx)
  (setf (window-tly win) tly)
  (touch-windows win))



(defun window-areas-overlap? (a-tlx a-tly a-brx a-bry
                       b-tlx b-tly b-brx b-bry)
  "* Arguments:
- A-TLX, A-TLY: coordinates of top left corner of the first area.
- A-BRX, A-BRY: coordinates of bottom right corner of the first area.
- B-TLX, B-TLY: coordinates of top left corner of the second area.
- B-BRX, B-BRY: coordinates of bottom right corner of the second area.
* Returns: Boolean.
* Description: Do the two rectangular areas A-TLX,A-TLY==>A-BRX,A-BRY and
B-TLX,B-TLY==>B-BRX,B-BRY overlap?"
  (translate-negative-coordinates a-tlx a-tly)
  (translate-negative-coordinates a-brx a-bry)
  (translate-negative-coordinates b-tlx b-tly)
  (translate-negative-coordinates b-brx b-bry)
  (not (or (< a-brx b-tlx)
           (< a-bry b-tly)
           (> a-tlx b-brx)
           (> a-tly b-bry))))


(defmethod window-touches-spot? ((win <Window>) x y)
  (translate-negative-coordinates x y)
  (and (<= (window-tlx win) x (window-brx win))
       (<= (window-tly win) y (window-bry win))))


(defmethod windows-touching? ((win <Window>) (other <Window>))
  (window-areas-overlap?
   (window-tlx win)  (window-tly win)
   (window-brx win) (window-bry win)
   (window-tlx other)  (window-tly other)
   (window-brx other) (window-bry other)))


(defmethod windows-below ((win <Window>))
  (rest (member win *window-stack*)))


(defmethod windows-above ((win <Window>))
  (subseq *window-stack* 0 (position win *window-stack*)))


(defmethod windows-overlying ((win <Window>))
  (remove-if-not #'(lambda (w) (member w (window-touching win)))
                 (windows-above win)))


(defmethod windows-underlying ((win <Window>))
  (remove-if-not #'(lambda (w) (member w (window-touching win)))
                 (windows-below win)))


(defmethod windows-overlapping ((win <Window>) &key (include-window? t))
  (remove-if #'(lambda (win2)
                 (or (and (not include-window?) (eql win win2))
                     (not (windows-touching? win win2))))
             *window-stack*))


(defmethod copy-window-to-console ((win <Window>) con)
      ;; (warn "xsrc: ~D ysrc: ~D wsrc: ~D hsrc: ~D xdest: ~D ydest: ~D srcw: ~D srch: ~D destw: ~D desth: ~D~%"
      ;;     0 0 (window-width win) (window-height win)
      ;;     (window-tlx win) (window-tly win)
      ;;     (console-get-width (window-console win))
      ;;     (console-get-height (window-console win))
      ;;     (console-get-width con)
      ;;     (console-get-height con)
      ;;     )
      ;; (break)
  (console-blit (window-console win) 0 0 (window-width win) (window-height win)
                con (window-tlx win) (window-tly win)
                (window-transparency->fade win) (window-transparency->fade win)))



(defmethod redraw-window ((win <Window>))
  (copy-window-to-console win *root*))


(defun redraw-all-windows ()
  "* Arguments: None.
* Returns: None.
* Description: Copy all visible windows onto the root console."
  (dolist (win *window-stack*)
    (prepare-window win))
  (console-clear *root*)
  (copy-windows-to-console *window-stack* *root*)
  (console-set-dirty 0 0 (screen-width) (screen-height)))


(defun copy-windows-to-console (window-list con)
  "* Arguments:
- WINDOW-LIST: a list of instances of {defclass dormouse:<Window>}
- CON: a C console pointer.
* Returns: None.
* Description: Copy (draw) each window in WINDOW-LIST onto the console CON.
The FIRST window in the list is treated as the 'topmost' window and so
is copied LAST."
  (dolist (win (reverse window-list))
    (copy-window-to-console win con)))


(defun copy-console-to-console (src dest)
  "* Arguments:
- SRC, DEST: C console pointers.
* Returns: None.
* Description: Copy the entire contents of console SRC onto console DEST."
  (console-blit src 0 0
                (console-get-width src) (console-get-height src)
                dest 0 0 1.0 1.0))


(defun transparency->fade (tran)
  (- 1.0 (/ tran 100.0)))


(defun window-transparency->fade (win)
  "* Arguments:
- WIN: an instance of {defclass dormouse:<Window>}
* Returns: A real number between 0 and 1, representing a TCOD 'fade' value.
* Description: Given a window WIN, convert the value of its WINDOW-TRANSPARENCY
slot (0-100) to a FADE value (0-1) accepted by the TCOD library."
  (transparency->fade (window-transparency win)))



(defmethod prepare-window ((win <Window>))
  (cond
    ((window-framed? win)
     (cond
         ((eql win (car *window-stack*))
         (console-print-double-frame
          (window-console win) 0 0
          (window-width win) (window-height win)
          t :set (or (window-title win) "")))
         (t
         (console-print-frame
          (window-console win) 0 0
          (window-width win) (window-height win)
          t :set (or (window-title win) ""))))

     (when (window-can-close? win)
       (console-set-char (window-console win) (1- (window-width win))
                         0 (char-code #\X)))

     (when (window-can-resize? win)
         (console-set-char (window-console win) (1- (window-width win))
                           (1- (window-height win)) 29)))
    (t
     (console-rect (window-console win) 0 0
                   (window-width win) (window-height win) t :set)))
  (when (window-draw-function win)
       (funcall (window-draw-function win) win))
  (when (window-children win)
      (dolist (child (window-children win))
        (when (or (window-raise-children-with-parent? win)
                  (not (window-hidden? child)))
          (prepare-window child)))))



(defun prepare-windows-by-type (winclass)
  "* Arguments:
- WINCLASS: A symbol naming a subclass of {defclass dormouse:<Window>}.
* Returns: None.
* Description: Calls {defgeneric dormouse:prepare-window} for each existing
window object that inherits from WINCLASS.
* Examples:
;;; (prepare-windows-by-type '<Message-Window>)"
  (dolist (win *window-stack*)
    (when (typep win winclass)
      (prepare-window win))))


(defun top-window-at (x y &key (windows *window-stack*)
                      (override-modal? nil))
  "* Arguments:
- X, Y: Coordinates of a point on the screen (root console).
- WINDOWS: List of window objects to consider (default is to consider
all non-hidden windows).
- OVERRIDE-MODAL?: Boolean.
* Returns: A window object or nil.
* Description:
Return the window nearest the top of *WINDOW-STACK* which touches X,Y.
If OVERRIDE-MODAL? is true, then disregard whether a window is modal or
not in deciding which window to return. If this parameter is nil (default)
then whenever a modal window is at the top of WINDOW-STACK we can only
return that window from this function. If that window does not touch X,Y
then NIL is returned."
  (translate-negative-coordinates x y)
  (let ((top (car windows)))
    (cond
      ((and (not override-modal?) (modal? top))
       (if (window-touches-spot? top x y)
           top
           ;; else
           nil))
      (t
       (find-if #'(lambda (win) (and (window-touches-spot? win x y)
                                     (not (ghost-window? win))))
                windows)))))



(defun window-with-focus ()
  (top-window-at *mouse-x* *mouse-y*))



(defun windows-at (x y)
  "* Arguments:
- X, Y: Coordinates of a point on the screen (root console).
* Returns: A list of window objects, or nil.
* Description:
Return a list of all non-hidden windows that overlie the point at X,Y."
  (translate-negative-coordinates x y)
  (remove-if-not #'(lambda (win) (window-touches-spot? win x y))
                 *window-stack*))


(defun window-brx (win)
  "* Arguments:
- WIN: an instance of {defclass dormouse:<Window>}
* Returns: An X-coordinate.
* Description:
Return the X-coordinate of the bottom right corner of the window.
* See Also: {defun dormouse:window-bry}"
  (+ (window-tlx win) (1- (window-width win))))


(defun window-bry (win)
  "* Arguments:
- WIN: an instance of {defclass dormouse:<Window>}
* Returns: A Y-coordinate.
* Description:
Return the Y-coordinate of the bottom right corner of the window.
* See Also: {defun dormouse:window-brx}"
  (+ (window-tly win) (1- (window-height win))))


(defmethod resize-window ((win <Window>) width height)
  "Change the dimensions of window WIN to WIDTH x HEIGHT."
  (untouch-windows win)
  (setf (window-console win) (console-new width height))
  (console-set-default-foreground (window-console win)
                                 (colour (window-foreground win)))
  (console-set-default-background (window-console win)
                                 (colour (window-background win)))
  (setf (window-width win) width)
  (setf (window-height win) height)
  (touch-windows win))



(defmethod mouse-drag-window ((win <Window>) (rodent mouse))
  (let ((offsetx (- (mouse-cx rodent) (window-tlx win)))
        (offsety (- (mouse-cy rodent) (window-tly win)))
        (tlx 0) (tly 0)
        (width (window-width win))
        (height (window-height win))
        (root-width (console-get-width *root*))
        (root-height (console-get-height *root*)))
    ;; draw everything but the window
    (raise-window win)
    ;; Draw all windows but this one onto SCRATCH.
    ;; SCRATCH now represents ROOT "minus" WIN.
    (copy-windows-to-console (remove win *window-stack*) *scratch*)
    (console-flush)
    ;; Save part of root console covered by WIN
    (console-blit *scratch* (window-tlx win) (window-tly win)
                  width height
                  *temp-con* 0 0 1.0 1.0)
    (copy-console-to-console *scratch* *root*)
    (copy-window-to-console win *root*)
    (console-flush)
    (iterate
      (while (mouse-lbutton (setf rodent (mouse-get-status))))
      ;; Update position of WIN based on mouse position
      (setf tlx (constrain (- (mouse-cx rodent) offsetx)
                           0 (- root-width width 1)))
      (setf tly (constrain (- (mouse-cy rodent) offsety)
                           0 (- root-height height 1)))
      ;;(format t "~A ~A ~A ~A~%" tlx tly width height)
      (unless (and (= tlx (window-tlx win)) (= tly (window-tly win)))
        ;; copy saved win to root  at WIN's old position (erasing WIN)
        (console-blit *temp-con* 0 0
                      width height
                      *root* (window-tlx win) (window-tly win) 1.0 1.0)
        ;; "move" WIN to the new position
        (move-window win tlx tly)
        ;; save the part of the root console which WIN covers
        (console-blit *scratch* tlx tly
                      width height
                      *temp-con* 0 0 1.0 1.0)
        ;; copy WIN to root
        (copy-window-to-console win *root*))
      ;; Refresh root console
      (console-flush))))


(defmethod mouse-resize-window ((win <Window>) (rodent mouse))
  (let ((brx 0) (bry 0))
    ;; draw everything but the window
    (raise-window win)
    (copy-windows-to-console (remove win *window-stack*) *scratch*)
    ;; Save part of root console covered by WIN
    (console-blit *scratch* (window-tlx win) (window-tly win)
                  (window-width win) (window-height win)
                  *temp-con* 0 0 1.0 1.0)
    (copy-console-to-console *scratch* *root*)
    (copy-window-to-console win *root*)
    (console-flush)
    (iterate
      (while (mouse-lbutton (setf rodent (mouse-get-status))))
      ;; Update position of WIN based on mouse position.  Don't allow the mouse
      ;; to go above or to left of the top left corner of the window.
      (setf brx (constrain (mouse-cx rodent) (window-tlx win)
                           (1- (console-get-width *root*))))
      (setf bry (constrain (mouse-cy rodent) (window-tly win)
                           (1- (console-get-height *root*))))
      (unless (and (= brx (window-brx win)) (= bry (window-bry win)))
        ;; copy saved win to root  at WIN's old position (erasing WIN)
        (console-blit *temp-con* 0 0
                      (window-width win) (window-height win)
                      *root* (window-tlx win) (window-tly win) 1.0 1.0)
        ;; Resize WIN (might be smaller)
        (resize-window win (1+ (- brx (window-tlx win)))
                       (1+ (- bry (window-tly win))))
        (prepare-window win)
        ;; save the part of the root console which WIN now covers
        (console-blit *scratch* (window-tlx win) (window-tly win)
                      (window-width win) (window-height win)
                      *temp-con* 0 0 1.0 1.0)
        ;; copy WIN to root
        (copy-window-to-console win *root*))
      ;; Refresh root console
      (console-flush))))


(defmethod raise-window ((win <Window>) &key (redraw *auto-redraw*)
                         (simple-redraw? nil) &allow-other-keys)
  (setf *window-stack* (remove win *window-stack* :test #'equal))
  (setf *hidden-windows* (remove win *hidden-windows* :test #'equal))
  (setf (window-hidden? win) nil)
  (setf (window-changed? win) t)
  (push win *window-stack*)
  (dirty-window win)
  (when (window-children win)
    (dolist (child  (window-children win))
      (when (or (window-raise-children-with-parent? win)
                (not (window-hidden? child)))
      (raise-window child :redraw redraw :simple-redraw? simple-redraw?))))
  (when redraw
    (if simple-redraw?
        (copy-window-to-console win *root*)
        ;; else
        (redraw-window-area win))))



(defmethod hide-window ((win <Window>) &key (redraw *auto-redraw*))
  (when redraw
    (redraw-window-area win :draw-window nil))
  (untouch-windows win)
  (setf *window-stack* (remove win *window-stack* :test #'equal))
  (if (and (window-raise-children-with-parent? win)
           (window-children win))
      (dolist (child (window-children win))
        (hide-window child)))
  (cond
    ((window-ephemeral? win)
     (destroy-window win))
    (t
     (setf (window-hidden? win) t)
     (pushnew win *hidden-windows* :test #'equal))))


(defun hide-all-windows ()
  (dolist (win (copy-list *window-stack*))
    (hide-window win)))


(defmethod send-to-window :before ((win <Window>) (event <Key-Event>))
  ;; Ctrl-Esc and Ctrl-F1 are defined here as universal "shut down the
  ;; gui loop" keys.
  (let ((k (gui-event-keypress event)))
    (when (key-pressed k)
      (when (and (member (key-vk k) (list :escape :f1))
                 (or (key-lctrl k)
                     (key-rctrl k))
                 (not (key-shift k))
                 (not (key-lalt k))
                 (not (key-ralt k)))
        (setf *exit-gui?* t)))))




(defmethod on-border? ((win <Window>) winx winy)
  (or (= winx 0)
      (= winx (1- (window-width win)))
      (= winy 0)
      (= winy (1- (window-height win)))))



(defmethod on-lower-window-border? ((win <Window>) winx winy)
  (and (= winy (1- (console-get-height (window-console win))))
       (> winx 0)
       (< winx (1- (console-get-width (window-console win))))))



(defmethod on-upper-window-border? ((win <Window>) winx winy)
  (and (= winy 0)
       (> winx 0)
       (< winx (1- (console-get-width (window-console win))))))



(defmethod window-draw-char-at ((win <Window>) (ch integer) winx winy
                                &key (background-flag :set)
                                (fg nil) (bg nil) (redraw *auto-redraw*))
  (console-draw-char-at (window-console win) ch winx winy
                        :background-flag background-flag
                        :fg fg :bg bg)
  (when redraw
    (redraw-all-at (winx->rootx win winx) (winy->rooty win winy))))
;;    (window-redraw-at win (winx->rootx winx) (winy->rooty winy))))



(defmethod console-draw-char-at (con (ch integer) winx winy
                             &key (background-flag :set)
                                (fg nil) (bg nil))
  (cond
    ((and fg bg)
     ;; New in libtcod 1.4.3
     (console-put-char-ex con winx winy ch (colour fg) (colour bg)))
    ((or fg bg)
     (console-set-char con winx winy ch)
     (cond
       (fg
        (console-set-char-foreground con winx winy (colour fg)))
       (bg
        (console-set-char-background con winx winy (colour bg) background-flag))))
    (t
     (console-put-char con winx winy ch background-flag)))
  ;; todo restore old values after printing
  )


(defmethod window-draw-char-at ((win <Window>) (ch character) winx winy
                                &key (background-flag :set) (fg nil) (bg nil)
                                (redraw *auto-redraw*))
  (window-draw-char-at win (char-code ch) winx winy
                       :background-flag background-flag :fg fg :bg bg
                       :redraw redraw))


(defmethod draw-string-at ((win <Window>) str winx winy
                           &key (fg nil) (bg nil) (background-flag :set)
                           (redraw *auto-redraw*))
  (declare (ignorable redraw))
  (cond
    ((or fg bg)
     (if fg
         (console-set-default-foreground (window-console win) (colour fg)))
     (if bg
         (console-set-default-background (window-console win) (colour bg)))
     (console-print-ex (window-console win) winx winy
                       background-flag :left
                    (make-coloured-string str :win win))
     (if fg
         (console-set-default-foreground (window-console win)
                                        (colour (window-foreground win))))
     (if bg
         (console-set-default-background (window-console win)
                                        (colour (window-background win)))))
    (t
     (console-print-ex (window-console win) winx winy
                       background-flag :left
                    (make-coloured-string str :win win)))))




(defun rootx->winx (win rootx)
  "* Arguments:
- WIN: an instance of {defclass dormouse:<Window>}.
- ROOTX: An X-coordinate on the screen (root console).
* Returns: Integer.
* Description: Given the screen X-coordinate ROOTX, return the X-coordinate
of the same screen point relative to the top left corner of the window WIN.
* Examples:
;;; (rootx->winx mywin 10)  ; if top left corner of mywin is at (8,8)
;;;                         ; then this returns 2
* See Also:
- {defun dormouse:rooty->winy}"
  (- rootx (window-tlx win)))

(defun rooty->winy (win rooty)
  "* Arguments:
- WIN: an instance of {defclass dormouse:<Window>}.
- ROOTY: A Y-coordinate on the screen (root console).
* Returns: Integer.
* Description: Given the screen Y-coordinate ROOTY, return the Y-coordinate
of the same screen point relative to the top left corner of the window WIN.
* Examples:
;;; (rooty->winy mywin 10)  ; if top left corner of mywin is at (8,8)
;;;                         ; then this returns 2
* See Also:
- {defun dormouse:rootx->winx}"
  (- rooty (window-tly win)))`<

(defun winx->rootx (win winx)
  "* Arguments:
- WIN: an instance of {defclass dormouse:<Window>}.
- WINX: An X-coordinate relative to the top left corner of WIN.
* Returns: Integer.
* Description: Given the X-coordinate WINX, which is relative to the top left
corner of WIN, return the absolute X-coordinate of the same screen point, i.e.
its console X-coordinate.
* Examples:
;;; (winx->rootx mywin 5)  ; if top left corner of mywin is at (8,8)
;;;                         ; then this returns 13
* See Also:
- {defun dormouse:winy->rooty}"
  (+ winx (window-tlx win)))

(defun winy->rooty (win winy)
  "* Arguments:
- WIN: an instance of {defclass dormouse:<Window>}.
- WINY: A Y-coordinate relative to the top left corner of WIN.
* Returns: Integer.
* Description: Given the Y-coordinate WINY, which is relative to the top left
corner of WIN, return the absolute Y-coordinate of the same screen point, i.e.
its console Y-coordinate.
* Examples:
;;; (winy->rooty mywin 5)  ; if top left corner of mywin is at (8,8)
;;;                         ; then this returns 13
* See Also:
- {defun dormouse:winx->rootx}"
  (+ winy (window-tly win)))



(defmethod window-redraw-at ((win <Window>) rootx rooty)
  (translate-negative-coordinates rootx rooty)
  (if (window-touches-spot? win rootx rooty)
      (console-blit (window-console win)
                    (rootx->winx win rootx)
                    (rooty->winy win rooty)
                    1 1 *root* rootx rooty
                    (window-transparency->fade win)
                    (window-transparency->fade win))))



(defmethod redraw-window-in-area (win1 rootx1 rooty1 rootx2 rooty2
                                  &key fade)
  "Redraw that portion of WIN which lies within the rectangle X1,Y1 -> X2,Y2
on the root console."
  (translate-negative-coordinates rootx1 rooty1)
  (translate-negative-coordinates rootx2 rooty2)
  (let* ((tlx (max (window-tlx win1) rootx1))
         (tly (max (window-tly win1) rooty1))
         (brx (min (window-brx win1) rootx2))
         (bry (min (window-bry win1) rooty2)))
    (console-fill-char *root*
                       #\space
                       tlx tly
                       (- brx (1- tlx)) (- bry (1- tly)))
    (console-blit (window-console win1)
                  (rootx->winx win1 tlx)
                  (rooty->winy win1 tly)
                  (- brx (1- tlx)) (- bry (1- tly))
                  *root* tlx tly
                  (or fade (window-transparency->fade win1))
                  (or fade (window-transparency->fade win1))
                  )))


(defmethod redraw-window-intersection ((win1 <Window>) (win2 <Window>)
                                       &key fade)
  (redraw-window-in-area win1 (window-tlx win2) (window-tly win2)
                         (window-brx win2) (window-bry win2)
                         :fade fade))


(defmethod redraw-intersecting-windows-below ((win <Window>))
  (dolist (w (reverse (windows-underlying win)))
    (redraw-window-intersection w win)))


(defmethod redraw-intersecting-windows-above ((win <Window>))
  (dolist (w (reverse (windows-overlying win)))
    (redraw-window-intersection w win)))


(defun fade-for-window (win)
  (let ((focus (window-with-focus)))
    (cond
      ((or (null (window-transparency-unfocussed win))
           (eql win focus))
       (window-transparency->fade win))
      ((and (eq *focus-fade-mode* :together)
            (not (typep focus '<background-window>)))
       (window-transparency->fade win))
      ((and (typep focus '<window>)
            (window-raise-children-with-parent? win)
            (or (find win (window-children focus))
                (find focus (window-children win))))
       (window-transparency->fade win))
      (t
       (transparency->fade
        (window-transparency-unfocussed win))))))



(defun redraw-window-area (win &key (draw-window t))
  "* Arguments:
- WIN: an instance of {defclass dormouse:<Window>}
- DRAW-WINDOW: Boolean (default: T)
* Returns: None.
* Description: Force the area of the screen covered by WIN to be redrawn.
If DRAW-WINDOW is non-nil, also redraw WIN itself; otherwise just redraw
all the other windows underlying WIN."
  (unless draw-window
    (console-set-default-background *root* (colour :true-black))  ; needed?
    (console-rect *root* (window-tlx win) (window-tly win)
                  (window-width win) (window-height win) t :set))
  (when (and draw-window (window-children win)) ;
    (dolist (child (window-children win))
      (unless (window-hidden? child)
        (redraw-window-area child)) :draw-window t))
  (dolist (w (nreverse (windows-overlapping win :include-window? draw-window)))
    (redraw-window-intersection w win
                                :fade (fade-for-window w)))
  (dirty-window win))


(defun redraw-all-at (rootx rooty)
  "* Arguments:
- ROOTX, ROOTY: Coordinates of a point on the screen (root console).
* Returns: None.
* Description: Force all windows which touch the screen at ROOTX, ROOTY
to be redrawn."
  (dolist (w (reverse *window-stack*))
    (window-redraw-at w rootx rooty)))


(defmethod send-to-window ((win <Window>) (event <GUI-Mouse-Drag-Event>))
  (let ((topwin nil)
        (rodent nil))
    ;; draw everything but the window
    (raise-window win)
    (iterate
      (while (mouse-lbutton (setf rodent (mouse-get-status))))
      (setf topwin (top-window-at (mouse-cx rodent) (mouse-cy rodent)))
      (when topwin
        (raise-window topwin :redraw t))
      (console-flush))
    ;; Stopped dragging.
    (when topwin
      (mouse-drag win topwin
                  (gui-event-winx event)
                  (gui-event-winy event)
                  (rootx->winx topwin (mouse-cx rodent))
                  (rooty->winy topwin (mouse-cy rodent))))))



;;; <<Modal Window>> ==========================================================


(defclass <Modal-Window> (<Window>)
  ()
  (:documentation "These monopolise the mouse and keyboard whenever they are
the topmost window. The user must cause the modal window to close before events
can be sent to any other windows."
  ))

(defun modal? (win)
  "* Arguments:
- WIN: an instance of {defclass dormouse:<Window>}
* Returns: Boolean.
* Description: Returns T if WIN inherits from
{defclass dormouse:<Modal-Window>}."
    (typep win '<Modal-Window>))


(defmethod send-to-window :around ((win <Modal-Window>) (event <Key-Event>))
  (let ((k (gui-event-keypress event)))
    (when (key-pressed k)
      (case (key-vk k)
        (:escape
         (hide-window win)
         k)
        (otherwise
         (call-next-method))))))



;;; <<Alert Window>> ==========================================================


(defclass <Alert-Window> (<Modal-Window> <Log-Window>)
  ((window-title :initform "ALERT")
   (window-initial-text :accessor window-initial-text :initform nil
                        :initarg :text)
   (window-auto-redraw? :initform t))
  (:documentation "TODO: document"))


(defmethod initialize-instance :after ((win <Alert-Window>) &key)
  (when (window-initial-text win)
    (add-message win (window-initial-text win))))


;;; <<Yes/No Window>> =========================================================


(defclass <Yes/No-Window> (<Modal-Window> <Dialog-Window>)
  ((window-title :initform "Yes or No")
   (window-prompt :accessor window-prompt :initform "Yes or No" :initarg :prompt)
   (window-tlx :initform 30)
   (window-tly :initform 20)
   (window-width :initform 30)
   (window-height :initform 5)
   (window-auto-redraw? :initform t)
   (window-choice-function :accessor window-choice-function
                           :initform nil :initarg :choice-function))
  (:documentation   "Modal dialog window that waits for the user to click
on (yes) or (no) or to press Y or N."))



(defmethod prepare-window :after ((win <Yes/No-Window>))
  (when (window-prompt win)
    (draw-string-at win (window-prompt win) 1 1))
  (draw-string-at win
               (concatenate 'string
                            "{fg:dark-grey,bg:grey,click:yes}[Yes]{/}   "
                            "{fg:dark-grey,bg:grey,click:no}[No]{/}")
               1 2))


(defmethod send-to-window :around ((win <Yes/No-Window>)
                                   (event <GUI-Dialog-Event>))
  (let ((str (gui-event-string event)))
    (cond
      ((and (string= str "yes")
            (window-choice-function win))
       (hide-window win)
       (funcall (window-choice-function win) t))
      ((and (string= str "no")
            (window-choice-function win))
       (hide-window win)
       (funcall (window-choice-function win) nil))
      (t
       (call-next-method)))))


(defmethod send-to-window :around ((win <Yes/No-Window>) (event <Key-Event>))
  (let ((k (gui-event-keypress event)))
    (when (key-pressed k)
      (cond
        ((and (eql :char (key-vk k))
              (member (key-c k) '(#\Y #\y))
              (window-choice-function win))
         (hide-window win)
         (funcall (window-choice-function win) t)
         k)
        ((and (eql :char (key-vk k))
              (member (key-c k) '(#\N #\n))
              (window-choice-function win))
         (hide-window win)
         (funcall (window-choice-function win) nil)
         k)
        (t
         (call-next-method))))))


;;;===========================================================================
;;; (@> "Ghost-Window")
;;;===========================================================================

(defclass <Ghost-Window> (<Window>)
  ()
  (:documentation "Window that cannot be interacted with. Athough it may be
raised to the top of the window stack, it cannot receive any messages from the
mouse or keyboard. All such messages pass through to the window below it."))


(defun ghost-window? (win)
  "* Arguments:
- WIN: an instance of {defclass dormouse:<Window>}
* Returns: Boolean.
* Description: Returns T if WIN inherits from
{defclass dormouse:<Ghost-Window>}."
    (typep win '<Ghost-Window>))


;;; <<Background window>> =====================================================


(defclass <Background-Window> (<Window>)
  ((window-can-resize? :initform nil)
   (window-can-drag? :initform nil)
   (window-framed? :initform nil)
   (window-transparency :initform +OPAQUE+)
   (window-fades-when-unfocussed? :initform nil))
  (:documentation "Window that always makes itself the lowest in the stack."))


(defmethod initialize-instance :after ((win <Background-Window>) &key)
  ;; Background windows always sink to the bottom of the stack.
  (setf *window-stack* (append (remove win *window-stack*) (list win))))


(defmethod raise-window ((win <Background-Window>) &key &allow-other-keys)
  (unless (find win *window-stack* :test #'equal)
    (push-end win *window-stack*)))



;;; <<List Window>> ===========================================================


(defclass <List-Window> (<Window>)
  ((window-items :accessor window-items :initform nil :type list)
   (window-offset :accessor window-offset :initform 0
                  :initarg :offset :type integer)
   (window-cursor :accessor window-cursor :initform 0 :type integer))
  (:documentation
   "Window that displays a list of strings which can be scrolled.

Up and down arrows move the cursor up and down the list.

Page-up and page-down keys move the cursor a page at a time.

Home and end keys move the cursor to the first and last item in the list.

Left clicking on an item in the list, moves the cursor to that item.

Pressing a 'hotkey' associated with an item, moves the cursor to that item.

Left and right clicks on the lower border of the window move the display down or
up a page at a time.

The enter key selects the item under the cursor."))



(defstruct list-item
  "* Description: TODO"
  (str nil)
  (item nil)
  (hotkey nil))


(defmethod window-item-lines ((win <List-Window>) (item list-item))
  1)


(defmethod window-items-lines ((win <List-Window>))
  (apply #'+
         (mapcar #'(lambda (item) (window-item-lines win item))
                 (window-items win))))



(defmethod add-item ((win <List-Window>) item str &optional hotkey)
  (push-end (make-list-item :str str :item item :hotkey hotkey)
            (window-items win)))


(defmethod clear-items ((win <List-Window>))
  (setf (window-items win) nil)
  (move-cursor-to win 0)
  (setf (window-offset win) 0))



(defmethod window-item-at ((win <List-Window>) winx winy)
  (let ((offset (+ (window-offset win) (1- winy))))
    (cond
      ((on-border? win winx winy)
       nil)
      ((> offset (window-items-lines win))
       nil)
      (t
       (nth offset (window-items win))))))



(defmethod window-item-at-cursor ((win <List-Window>))
  (nth (window-cursor win) (window-items win)))


(defmethod window-page-length ((win <List-Window>))
  (- (window-height win) 2))


(defmethod prepare-window :after ((win <List-Window>))
  (let* ((pagelen (window-page-length win))
         (offset 0))
    ;; Configure window
    (if (>= pagelen (window-items-lines win))
        (setf (window-offset win) 0))
    (constrain! (window-cursor win) 0 (window-items-lines win))
    (if (< (window-cursor win) (window-offset win))
        (setf (window-offset win) (window-cursor win)))
    (if (>= (window-cursor win) (+ (window-offset win) pagelen))
        (setf (window-offset win) (- (window-cursor win) (1- pagelen))))
    (setf offset (window-offset win))
    ;; Draw window
    (loop for i from offset to (+ offset (1- pagelen))
       when (and (<= 0 i (1- (length (window-items win))))
                 (nth i (window-items win)))
       do (draw-item-at win (nth i (window-items win))
                        1 (1+ (- i offset))
                        (= i (window-cursor win))))))



(defmethod draw-item-at ((win <List-Window>) (item list-item) winx winy cursor?)
  (let ((pagewidth (- (window-width win) 2)))
    (draw-string-at win
                    (format nil "~vA"
                            pagewidth
                            (left-trim-coloured-string
                             (format nil "~A" (list-item-str item))
                             pagewidth))
                    winx winy
                    :bg (if cursor?
                            (invert-colour
                             (console-get-char-background
                              (window-console win) 0 0))
                            nil))))



(defmethod send-to-window ((win <List-Window>) (event <Mouse-Event>))
  (with-slots ((winx gui-event-winx) (winy gui-event-winy)
               (mouse gui-event-mouse-state)) event
    (let ((left (mouse-lbutton-pressed mouse))
          (right (mouse-rbutton-pressed mouse))
          (wheel-up (mouse-wheel-up mouse))
          (wheel-down (mouse-wheel-down mouse)))
      (cond
        ((and (on-lower-window-border? win winx winy)
              (or left right))
         (send-key-to-window win (make-key :vk
                                           (cond
                                             (left :pagedown)
                                             (right :pageup)))
                             winx winy))
        ((and (not (on-border? win winx winy))
              wheel-up)
         (send-key-to-window win (make-key :vk :up) winx winy))
        ((and (not (on-border? win winx winy))
              wheel-down)
         (send-key-to-window win (make-key :vk :down) winx winy))
        ((and left
              (not (on-border? win winx winy)))
         (let ((newc (- (1- winy) (window-offset win))))
           (if (<= newc (window-items-lines win))
               (move-cursor-to win newc)))
         (prepare-window win)
         (redraw-window-area win))
        (t
         (call-next-method))))))




(defmethod move-cursor-to ((win <List-Window>) (cursor integer))
  (let ((oldcursor (window-cursor win)))
    (setf (window-cursor win)
          (constrain cursor 0 (max 0 (1- (length (window-items win))))))
    (when (and (/= oldcursor (window-cursor win))
               (window-item-at-cursor win))
      (cursor-moved-to-item win (window-item-at-cursor win)))))


(defmethod cursor-moved-to-item ((win <List-Window>) (item t))
  nil)



(defmethod move-cursor-by ((win <List-Window>) (increment integer))
  (move-cursor-to win (+ (window-cursor win) increment)))



(defmethod send-to-window ((win <List-Window>) (event <Mouse-Hover-Event>))
  (with-slots ((winx gui-event-winx) (winy gui-event-winy)) event
  (cond
    ((null (gui-event-focus event))
     (setf (gui-event-focus event) (window-item-at win winx winy))))))



(defmethod send-to-window :around ((win <List-Window>) (event <Key-Event>))
  (with-slots ((winx gui-event-winx) (winy gui-event-winy)
               (k gui-event-keypress)) event
    (when (key-pressed k)
      (let ((pagelen (- (window-height win) 2))
            (num-items (length (window-items win))))
        (case (key-vk k)
          (:up
           (move-cursor-by win -1) ;;(decf (window-cursor win))
           (if (< (window-cursor win) (window-offset win))
               (decf (window-offset win))))
          (:down
           (move-cursor-by win 1) ;;(incf (window-cursor win))
           (if (>= (window-cursor win) (+ (window-offset win) pagelen))
               (incf (window-offset win))))
          (:pageup
           (decf (window-offset win) pagelen)
           (move-cursor-by win (- pagelen))) ;;(decf (window-cursor win) pagelen))
          (:pagedown
           (incf (window-offset win) pagelen)
           (move-cursor-by win pagelen)) ;;(incf (window-cursor win) pagelen))
          (:home
           (setf (window-offset win) 0)
           (move-cursor-to win 0)) ;;(setf (window-cursor win) 0))
          (:end
           (setf (window-offset win) (- num-items pagelen))
           (move-cursor-to-end win)) ;; (setf (window-cursor win) (1- num-items)))
          (:enter
           (send-to-window win (make-instance
                                '<GUI-Select-Event>
                                :focus (list-item-item (window-item-at-cursor win))
                                :winx winx :winy winy)))
          (otherwise
           (let ((matching (find-if #'(lambda (item)
                                        (and (key-p (list-item-hotkey item))
                                             (same-keys? (list-item-hotkey item) k)))
                            (window-items win))))
             (cond
               (matching
                   (move-cursor-to win (position matching (window-items win))))
               (t
                (return-from send-to-window (call-next-method)))))))
        ;;(constrain! (window-cursor win) 0 (max 0 (1- num-items)))
        (constrain! (window-offset win)
                    (1+ (- (window-cursor win) pagelen))
                    (window-cursor win))
        (constrain! (window-offset win) 0 (max 0 (- num-items pagelen)))
        (prepare-window win)
        (redraw-window-area win)
        k))))


(defmethod move-cursor-to-end ((win <List-Window>))
  (let ((num-items (length (window-items win)))
        (pagelen (- (window-height win) 2)))
    (setf (window-offset win) (- num-items pagelen))
    (move-cursor-to win (1- num-items))
    ;;(constrain! (window-cursor win) 0 (max 0 (1- num-items)))
    (constrain! (window-offset win)
                (1+ (- (window-cursor win) pagelen))
                (window-cursor win))
    (constrain! (window-offset win) 0 (max 0 (- num-items pagelen)))))



;;; <<Filtered Window>> =======================================================


(defclass <Filtered-Window> (<List-Window>)
  ((filter-string :accessor filter-string :initform nil)
  (window-all-items :accessor window-all-items :initform nil)
  (window-auto-redraw? :initform t))
  (:documentation
   "List window which can selectively display only those items whose strings
contain the substring FILTER-STRING.  The active FILTER-STRING is displayed in
the lower border of the window.

Typing normal characters in the window will add those characters to the end of
FILTER-STRING.

Pressing BACKSPACE will erase a character from the end of FILTER-STRING.

Pressing DELETE will erase all characters in FILTER-STRING."))



(defmethod clear-filter ((win <Filtered-Window>))
  (setf (filter-string win) nil))


(defmethod clear-items :after ((win <Filtered-Window>))
  (setf (window-all-items win) nil))



(defmethod add-item :around ((win <Filtered-Window>) itemdata str &optional k)
  (let ((item (make-list-item :str str :item itemdata :hotkey k)))
    (push-end item (window-all-items win))
    (if (item-matches-filter-string? win item)
        (call-next-method))))



(defmethod item-matches-filter-string? ((win <Filtered-Window>) item)
  (or (null (filter-string win))
      (search (string-upcase (filter-string win))
              (string-upcase (list-item-str item)))))




(defmethod prepare-window :after ((win <Filtered-Window>))
  (draw-string-at win
                  ;; Uppercase version of filter-string, padded by *
                  (format nil "~A~:@([~V,,,'*A]~)~A"
                          (if (filter-string win) "{fg:dark-grey,bg:white}" "")
                          (- (window-width win) 4)
                          (or (filter-string win) "")
                          (if (filter-string win)
                              "{/} " ""))
                  1 (1- (window-height win))))




(defmethod send-to-window :after ((win <Filtered-Window>) (event <Key-Event>))
  (with-slots ((winx gui-event-winx) (winy gui-event-winy)
               (k gui-event-keypress)) event
    (let ((added? nil))
      (when (key-pressed k)
        (cond
          ((and (eql (key-vk k) :char)
                (graphic-char-p (key-c k)))
           ;; add char to filter-string
           (setf (filter-string win)
                 (concatenate 'string (filter-string win) (list (key-c k))))
           (setf added? t))
          ((eql (key-vk k) :backspace)
           ;; remove one char from end of filter-string
           (if (> (length (filter-string win)) 0)
               (setf (filter-string win)
;;; STRLEFT
                     (subseq (filter-string win) 0
                             (min (length (filter-string win))
                                  (1- (length (filter-string win))))))))
          ((eql (key-vk k) :delete)
           ;; clear filter-string
           (setf (filter-string win) nil))
          (t
           (return-from send-to-window nil)))
        ;; Still here means we changed filter-string.
        ;; Therefore need to recalculate lines.
        (cond
          ((and (filter-string win)
                (string> (filter-string win) ""))
           (setf (window-items win)
                 (iterate
                   (for item in (if added?
                                    (window-items win)
                                    (window-all-items win)))
                   (if (item-matches-filter-string? win item)
                       (collect item)
                       ;; else
                       nil))))
          (t
           (setf (window-items win) (copy-list (window-all-items win)))))
        (move-cursor-to win 0)
        (setf (window-changed? win) t)))))





;;; <<Menu Window>> ===========================================================


(defclass <Menu-Window> (<List-Window>)
  ((window-framed? :initform nil)
  (window-can-resize? :initform nil)
  (window-can-drag? :initform nil))
  (:documentation
     "List window that cannot be scrolled or resized. Clicking on an item
closes the menu window and returns the item. Hovering the mouse over an item
moves the cursor to that item."))


(defmethod send-to-window :before ((win <Menu-Window>) (event <Mouse-Hover-Event>))
  (with-slots ((winx gui-event-winx) (winy gui-event-winy)) event
    (unless (on-border? win winx winy)
      (move-cursor-to win (+ (window-offset win) (1- winy)))
      (prepare-window win)
      (redraw-window-area win))))


(defmethod send-to-window ((win <Menu-Window>) (event <Mouse-Event>))
  (with-slots ((winx gui-event-winx) (winy gui-event-winy)
               (mouse gui-event-mouse-state)) event
    (cond
      ((mouse-rbutton-pressed mouse)
       (hide-window win))
      ((and (mouse-lbutton-pressed mouse)
            (not (on-border? win winx winy)))
       (move-cursor-to win (+ (window-offset win) (1- winy)))
       (when (list-item-p (window-item-at-cursor win))
         ;; (warn "Menu - selected item ~S"
         ;;            (list-item-item
         ;;             (window-item-at-cursor win)))
         (hide-window win)
         (send-to-window win (make-instance
                              '<GUI-Select-Event>
                              :focus (list-item-item (window-item-at-cursor win))
                              :winx winx :winy winy)))))))


(defmethod send-to-window :after ((win <Menu-Window>) (event <Key-Event>))
  (with-slots ((winx gui-event-winx) (winy gui-event-winy)
               (k gui-event-keypress)) event
    (when (and (key-pressed k)
               (window-item-at-cursor win)
               (list-item-hotkey (window-item-at-cursor win))
               (same-keys? k (list-item-hotkey (window-item-at-cursor win))))
      (hide-window win)
      (send-to-window win (make-instance
                           '<GUI-Select-Event>
                           :focus (list-item-item (window-item-at-cursor win))
                           :winx winx :winy winy)))))



;;; <<Window with Context Menu>> ==============================================


(defparameter +DEFAULT-CONTEXT-MENU-INITARGS+
  '(:tlx 0 :tly 0 :width 20 :height 8
    :title "Context"
    :foreground :light-blue
    :background :dark-grey))


(defclass <Window-With-Context-Menu> (<Window>)
  ((window-raise-children-with-parent? :initform nil)
   (context-menu :accessor context-menu :initform nil
                 :documentation "Context menu window associated with this
window.")
   (context-menu-class :accessor context-menu-class :initform '<Context-Menu>)
   (context-menu-initargs :initform +DEFAULT-CONTEXT-MENU-INITARGS+
                          :accessor context-menu-initargs))
  (:documentation
   "Window where right-clicking brings up a menu of commands that the user
can choose to apply to the item that was clicked on."))


(defclass <Context-Menu> (<Menu-Window>)
  (;;(window-transparency :initform +OPAQUE+)
   (window-width :initform 15)
   (window-height :initform 8)
   (context-item :accessor context-item :initform nil
                 :documentation "Item (eg thing) which is the 'context' of the
current window activation.")))


(defmethod make-context-menu ((win <Window-With-Context-Menu>))
  (setf (context-menu win)
        (apply #'make-instance (context-menu-class win)
               :hidden? t (context-menu-initargs win)))
  (push (context-menu win) (window-children win)))


(defmethod initialize-instance :after ((win <Window-With-Context-Menu>)
                                       &key)
  (make-context-menu win))



(defmethod destroy-window :before ((win <Context-Menu>))
  (let ((parent (window-parent win)))
    (when (and parent
               (eql (context-menu parent) win))
      (setf (context-menu parent) nil)
      (setf (window-children parent)
            (remove win (window-children parent))))))



(defmethod raise-window :before ((win <Window-With-Context-Menu>)
                                 &key redraw &allow-other-keys)
  (declare (ignore redraw))
  (unless (or (null (context-menu win))
              (window-hidden? (context-menu win)))
    (hide-window (context-menu win))))


(defmethod hide-window :before ((win <Window-With-Context-Menu>)
                                 &key redraw &allow-other-keys)
  (declare (ignore redraw))
  (unless (or (null (context-menu win))
              (window-hidden? (context-menu win)))
    (hide-window (context-menu win))))



(defmethod send-to-window :before ((win <Window-With-Context-Menu>)
                                   (event <Mouse-Event>))
  (with-slots ((winx gui-event-winx) (winy gui-event-winy)
               (mouse gui-event-mouse-state)) event
    (when (mouse-rbutton-pressed mouse)
    (setf (context-item (context-menu win))
          (get-context-at win winx winy)))))



(defmethod send-to-window :after ((win <Window-With-Context-Menu>)
                                  (event <Mouse-Event>))
  (with-slots ((mouse gui-event-mouse-state)) event
    (when (mouse-rbutton-pressed mouse)
      (cond
        ((window-hidden? (context-menu win))
         (move-window-beside-mouse (context-menu win))
         (prepare-window (context-menu win))
         (raise-window (context-menu win) :redraw t))
        (t
         (hide-window (context-menu win) :redraw t))))))




(defmethod get-context-at ((win <List-Window>) winx winy)
  (let ((item (window-item-at win winx winy)))
    (cond
      (item
       (list-item-item item))
      (t
       nil))))



(defmethod prepare-window :around ((win <Context-Menu>))
  (let ((cursor (window-cursor win))
        (items (get-menu-items-for-context (window-parent win)
                                           (context-item win))))
    (clear-items win)
    (cond
      ((null items)
       (resize-window win (window-width win) 3)
       (add-item win nil "(nothing)"))
      (t
       (resize-window win (window-width win) (min (- (screen-height) 2)
                                                  (+ (length items) 2)))
       (loop for item-desc-binding in items do
            (destructuring-bind (item desc &optional binding) item-desc-binding
              (add-item win item
                        (format nil "~20A{yellow}~A{/}"
                                desc
                                (if binding
                                    (key->string (binding->key binding))
                                    ""))
                        (if binding (binding->key binding)))))))
    (move-cursor-to win (min (length items) cursor))
    (call-next-method)))



(defmethod send-to-window :after ((win <Context-Menu>) (event <GUI-Select-Event>))
  (command-from-context-menu (window-parent win) (gui-event-focus event)
                             (context-item win)))


(defmethod get-menu-items-for-context ((win <Window-With-Context-Menu>)
                                       context-item)
  (declare (ignore context-item))
  nil)

(defmethod command-from-context-menu ((win <Window-With-Context-Menu>)
                                      command context-item)
  (declare (ignore command context-item))
  nil)


;;; <<Log Window>> ============================================================


(defclass <Log-Window> (<List-Window>)
  ((window-can-resize? :initform t)
   (window-can-drag? :initform t)
   (window-show-tail-by-default? :accessor window-show-tail-by-default?
                                 :initform nil
                                 :documentation "If true, show the BOTTOM of
the list of messages rather than returning to the top, when the window is
refreshed.")
   (window-max-messages :accessor window-max-messages :initform 100)
   (window-raw-messages :accessor window-raw-messages :initform (list)))
  (:documentation
     "A kind of list-window where messages are appended to the end of the list."))


(defun wrap-coloured-text (text &key width)
  "TODO document"
  (word-wrap text :width width :respect-newlines t
             :respect-hyphens t
             :exclude-start-char #\{
             :exclude-end-char #\}))


(defmethod wrap-items ((win <Log-Window>))
  (let* ((pagewidth (- (window-width win) 2))
         (wrapped (apply #'append
                         (mapcar #'(lambda (msg)
                                     (wrap-coloured-text msg :width pagewidth))
                                 (window-raw-messages win)))))
    (setf (window-items win)
          (mapcar #'(lambda (str) (make-list-item :str str :item nil :hotkey nil))
                  wrapped))))


(defmethod prepare-window :before ((win <Log-Window>))
  (wrap-items win)
  (if (window-show-tail-by-default? win)
      (move-cursor-to-end win)))



(defmethod draw-item-at ((win <Log-Window>) (item list-item)
                         winx winy (cursor? (eql t)))
  (draw-item-at win item winx winy nil))



(defmethod send-to-window :around ((win <Log-Window>) (event <Key-Event>))
  (with-slots ((winx gui-event-winx) (winy gui-event-winy)
               (k gui-event-keypress)) event
    (when (key-pressed k)
      (let ((pagelen (- (window-height win) 2))
            (num-items (length (window-items win))))
        (case (key-vk k)
          (:up
           (decf (window-offset win)))
          (:down
           (incf (window-offset win)))
          (otherwise
           (return-from send-to-window (call-next-method))))
        ;; (warn "offset = ~D, cursor = ~D, num-items = ~D"
        ;;            (window-offset win) (window-cursor win) num-items)
        (constrain! (window-offset win) 0 (max 0 (- num-items pagelen)))
        (move-cursor-to win (window-offset win))
        ;;(constrain! (window-cursor win) 0 (max 0 (1- num-items)))
        ;; (warn "Now offset = ~D, cursor = ~D, num-items = ~D"
        ;;            (window-offset win) (window-cursor win) num-items)
        (prepare-window win)
        (redraw-window-area win))
      k)))



(defmethod add-message ((win <Log-Window>) fmt &rest args)
  (when fmt
    (let ((msg (apply #'format nil fmt args)))
      (push-end msg (window-raw-messages win))
      (iterate
        (while (> (length (window-raw-messages win)) (window-max-messages win)))
        (pop (window-raw-messages win)))
      (if (> (length msg) (- (window-width win) 2))
          (wrap-items win)
          ;; else
          (add-item win msg msg nil))
      (move-cursor-to-end win))))


(defmethod add-message-and-redraw ((win <Log-Window>) fmt &rest args)
  (apply #'add-message win fmt args)
  (prepare-window win)
  (redraw-window-area win)
  (console-flush))


(defmethod fill-to-end-of-window ((win <Log-Window>))
  (let ((hgt (+ -2 (window-height win)))
        (num-items (length (window-items win))))
    (when (< num-items hgt)
      (dotimes (i (- hgt num-items))
        (add-item win "" "" nil))
      (move-cursor-to-end win))))



;;;     (setf (window-offset win) (- (length (window-items win))
;;;                              (- (window-height win) 2)))
;;;     (setf (window-cursor win) (1- (length (window-items win))))
;;;     (constrain! (window-cursor win) 0 (max 0 (1- (length (window-items win)))))
;;;     (constrain! (window-offset win) 0 (max 0 (- (length (window-items win))
;;;                                             (- (window-height win) 2))))
;;;     (prepare-window win)
;;;     (redraw-window-area win)))


(defmethod clear-messages ((win <Log-Window>))
  (setf (window-raw-messages win) nil)
  (setf (window-items win) nil)
  (move-cursor-to win 0))

(defun bottom-message (fmt &rest args)
  "* Arguments:

- FMT: String, which may contain formatting directives for the Common
Lisp function FORMAT
 (see {http://www.lispworks.com/documentation/HyperSpec/Body/22_c.htm}.)
- ARGS: Zero or more arguments matching the formatting directives
in FMT.

* Returns: None.

* Description: Prints the string returned by (APPLY #'FORMAT NIL ARGS)
onto the bottom line of the root console.

* Example:
;;; (bottom-message \"Mouse position: (~D, ~D)\" *mouse-x* *mouse-y*)"
  (declare (string fmt))
  (console-rect *root* 0 (1- (console-get-height *root*))
                (console-get-width *root*) 1 t :set)
  (console-print *root* 0 (1- (console-get-height *root*))
                 (format nil "~vA" (console-get-width *root*)
                         (apply #'format nil fmt args))))


;;; <<Pager Window>> ==========================================================


(defclass <Pager-Window> (<Log-Window>)
  ())


(defmethod add-browser-line ((win <Pager-Window>) line)
  (typecase line
    (string  (add-message win line))
    (list
     (destructuring-bind (item desc) line
       (declare (ignore item))
       (add-message win desc)))))


(defmethod clear-browser-lines ((win <Pager-Window>))
    (clear-messages win))


(defmethod send-to-window :around ((win <Pager-Window>) (event <Key-Event>))
  (with-slots ((k gui-event-keypress)) event
    (when (key-pressed k)
      (case (key-vk k)
        ((:enter :kpenter)
         nil)
        (:escape
         (hide-window win))
        (otherwise
         (call-next-method))))))


;;; <<Hypertext window>> ======================================================


(defclass <Hypertext-Window> (<Dialog-Window> <Pager-Window>)
  ((hypertext-lookup-function
    :initform nil
    :initarg :lookup-function
    :accessor hypertext-lookup-function
    :documentation "Function that takes a single string as its argument.
Returns the text of the hypertext database entry whose title is the same
as the string. Returns nil if the topic does not exist.")
   (hyperlink-foreground-colour
    :initarg :hyperlink-fg
    :writer (setf hyperlink-foreground-colour))
   (hypertext-start-topic :initform "Start" :initarg :start-topic
                          :accessor hypertext-start-topic)
   (hypertext-history :initform nil :accessor hypertext-history))
  (:documentation
   "Press BACKSPACE or LEFT arrow to go back to the last topic.
Press HOME to go back to the 'root' or 'start' topic."))


(defmethod hyperlink-foreground-colour ((win <Hypertext-Window>))
  (if (slot-boundp win 'hyperlink-foreground-colour)
      (slot-value win 'hyperlink-foreground-colour)
      (hyperlink-foreground-colour *window-theme*)))


(defmethod hypertext-current-topic ((win <Hypertext-Window>))
  (car (hypertext-history win)))


(defmethod window-title ((win <Hypertext-Window>))
  (let ((topic (car (hypertext-history win))))
    (format nil "~A: ~:(~A~)"
            (slot-value win 'window-title)
            topic)))


(defun mark-hyperlinks-in-string (str &key (fg :blue))
  (cl-ppcre:regex-replace-all "\\[(.+?)\\]" str
                              (format nil "{fg:~A,click:\\1}\\1{/}" fg)
                              :preserve-case t))


(defun word-unwrap (str &key (no-wrap-prefix "|"))
  "* Description
Given a string, gets rid of line breaks within 'paragraphs' in the string.
Paragraphs are defined as groups of lines separated by blank lines.
If a line begins with the string NO-WRAP-PREFIX, it is left as is."
  (with-input-from-string (s str)
    (let ((lines nil))
      (iterate
        (with accum = nil)
        (for line = (read-line s nil :eof))
        (while (not (eq :eof line)))
        (print line)
        (cond
          ((zerop (length line))
           (when accum
             (push accum lines)
             (setf accum nil))
           (push line lines))
          ((eq 0 (search no-wrap-prefix line))
           (when accum
             (push accum lines)
             (setf accum nil))
           (push line lines))
          (t
           (setf accum (concatenate
                        'string (or accum "") line " "))))
        (finally
         (if accum (push accum lines))))
      (apply #'concatenate 'string
             (iterate
               (for line in (reverse lines))
               (collect (format nil "~A~%" line)))))))


(defmethod open-hypertext-topic ((win <Hypertext-Window>) topic)
  "If TEXT takes the form (= NEWTOPIC), then we are redirected
to NEWTOPIC.
If TEXT is a function, it will be called with no arguments, and
should return a string which will be used as the text of the topic."
  (let ((text (funcall (hypertext-lookup-function win) topic)))
    (cond
      ((null topic)
       nil)
      ((null text)
       nil)
      ((and (listp text)
            (eql (car text) '=)
            (stringp (second text)))
       (open-hypertext-topic win (second text)))
      ((or (stringp text)
           (functionp text))
       (unless (equal topic (hypertext-current-topic win))
         (push topic (hypertext-history win)))
       (clear-messages win)
       (add-message-and-redraw
        win (mark-hyperlinks-in-string
             (word-unwrap
              (if (functionp text) (funcall text) text))))))))


(defmethod initialize-instance :after ((win <Hypertext-Window>) &key)
  (unless (hypertext-current-topic win)
    (open-hypertext-topic win (hypertext-start-topic win))))


(defun make-autolinks-in-hypertext-database
    (db &key (fg (hyperlink-foreground-colour *window-theme*)))
  "* Arguments
- DB :: A hashtable of topic titles keyed to text strings.
* Returns
Nil.
* Description
Iterates through every topic in the hashtable DB. Within the text of each
topic, 'marks up' any unmarked occurrence of the title of another topic
in the same database. Finally, converts all marked hyperlinks to
the internal format recognised by the GUI."
  (iterate
    (with topics = (sort (remove-if-not #'stringp (all-hashtable-keys db))
                         #'> :key #'length))
    (for (topic text) in-hashtable db)
    (unless (stringp text) (next-iteration))
    (iterate
      (with matches = nil)
      (for topic2 in topics)
      (if (equal topic2 topic) (next-iteration))
      (if (some (lambda (match) (search topic2 match)) matches)
          (next-iteration))
      (multiple-value-bind (newtext matched?)
          (cl-ppcre:regex-replace-all
           ;; The (?i) turns case sensitivity off
           (concatenate 'string "(?i)([^[}])(" topic2 ")([^]{])")
           text (format nil "\\1{fg:~A,click:\\2}\\2{/}\\3" fg)
           :preserve-case t)
        (setf text newtext)
        (if matched? (push topic2 matches))))
    (setf text (mark-hyperlinks-in-string text :fg fg))
    (setf (gethash topic db) text)))


(defmethod send-to-window ((win <Hypertext-Window>) (event <GUI-Dialog-Event>))
  (let ((topic (gui-event-string event)))
    (open-hypertext-topic win topic)))


(defmethod send-to-window :around ((win <Hypertext-Window>)
                                   (event <Key-Event>))
  (let ((k (gui-event-keypress event)))
    (when (key-pressed k)
      (print k)
      (cond
        ((and (hypertext-history win)
              (find (key-vk k) '(:backspace :left :kp4)))
         (pop (hypertext-history win))
         (open-hypertext-topic win (hypertext-current-topic win))
         k)
        ((eql (key-vk k) :home)
         (open-hypertext-topic win (hypertext-start-topic win))
         k)
        (t
         (call-next-method))))))



;;; <<Terminal Window>> =======================================================


(defclass <Terminal-Window> (<Log-Window>)
  ((window-show-tail-by-default? :initform t)
   (window-prompt :initform "> " :initarg :prompt :accessor window-prompt)
   ;; Colours for the prompt, and the input text
   (window-prompt-foreground :initarg :prompt-fg
                             :writer (setf window-prompt-foreground))
   (window-prompt-background :initarg :prompt-bg
                             :writer (setf window-prompt-background))
   (window-input-foreground :initarg :input-fg
                             :writer (setf window-input-foreground))
   (window-input-background :initarg :input-bg
                             :writer (setf window-input-background))
   (window-echo-input? :initform t :initarg :echo-input?
                       :accessor window-echo-input?
                       :documentation "After enter is pressed, should the
input be 'printed' in the window?")
   (window-input-active? :initform t :type (member t nil :transient)
                         :accessor window-input-active?
                         :documentation "")
   (window-input-string :initform nil :accessor window-input-string)
   (window-input-rendered :initform nil
                          :documentation "List of strings, updated whenever
the current 'input string' changes. Internal slot.")
   (window-input-history :initform (list "") :accessor window-input-history
                         :documentation "List of previous input strings.")
   (window-input-history-position :initform 0
                                  :accessor window-input-history-position)
   (window-input-cursor :initform 0 :accessor window-input-cursor)
   (window-input-function :initform #'identity
                          :accessor window-input-function
                          :documentation
                          "Function which takes a string argument. Called
when a line of input is entered in the window.")))


(defmethod window-prompt-foreground ((win <Terminal-Window>))
  (if (slot-boundp win 'window-prompt-foreground)
      (slot-value win 'window-prompt-foreground)
      (window-prompt-foreground *window-theme*)))

(defmethod window-prompt-background ((win <Terminal-Window>))
  (if (slot-boundp win 'window-prompt-background)
      (slot-value win 'window-prompt-background)
      (window-prompt-background *window-theme*)))

(defmethod window-input-foreground ((win <Terminal-Window>))
  (if (slot-boundp win 'window-input-foreground)
      (slot-value win 'window-input-foreground)
      (window-input-foreground *window-theme*)))

(defmethod window-input-background ((win <Terminal-Window>))
  (if (slot-boundp win 'window-input-background)
      (slot-value win 'window-input-background)
      (window-input-background *window-theme*)))


(defmethod initialize-instance :after ((win <Terminal-Window>) &key)
  )
  ;; (unless (window-prompt-foreground win)
  ;;   (setf (window-prompt-foreground win) (window-foreground win)))
  ;; (unless (window-prompt-background win)
  ;;   (setf (window-prompt-background win) (window-background win)))
  ;; (unless (window-input-foreground win)
  ;;   (setf (window-input-foreground win) (window-foreground win)))
  ;; (unless (window-input-background win)
  ;;   (setf (window-input-background win) (window-background win))))


(defmethod window-page-length ((win <Terminal-Window>))
  (if (window-input-active? win)
      (- (window-height win)
         (max 1 (length (slot-value win 'window-input-rendered))))
      (call-next-method)))


(defmethod prepare-window :after ((win <Terminal-Window>))
  (when (window-input-active? win)
    (iterate
      (with numlines = (length (slot-value win 'window-input-rendered)))
      (for line in (slot-value win 'window-input-rendered))
      (for i from 0 below numlines)
      (if (zerop i)
          (draw-string-at win (colourise (window-prompt win)
                                         (window-prompt-foreground win)
                                         (window-prompt-background win))
                          1 (- (window-height win) 1 numlines)))
      (draw-string-at win (colourise line (window-input-foreground win)
                                     (window-input-background win))
                      (if (zerop i) (+ (length (window-prompt win)) 1) 1)
                      (+ i (- (window-height win) 1 numlines))))))


(defmethod (setf window-input-string) :after (value (win <Terminal-Window>))
  (render-input-string win))


(defmethod render-input-string ((win <Terminal-Window>))
  (let ((str0 (if (window-input-string win)
                  (copy-seq (window-input-string win)))))
    (cond
      ((>= (window-input-cursor win) (length str0))
       (setf str0 (concatenate 'string str0 (string (code-char 1)))))
      ((and str0
            (plusp (length str0)))
       (setf (char str0 (window-input-cursor win))
             (code-char 1)))
      (t
       (setf str0 (string (code-char 1)))))
    ;;(break)
    (let* ((str (concatenate 'string (window-prompt win) str0))
           (width (- (window-width win) 2))
           (lines nil))
      (setf lines
            (iterate
              (while (plusp (length str)))
              (for line = (subseq str 0 (min width (length str))))
              (setf str (subseq str (min width (length str))))
              ;;(format t "~S ~S~%" line str)
              (if (find (code-char 1) line)
                  (setf line (concatenate
                              'string
                              (subseq line 0 (position (code-char 1) line))
                              (colourise
                               (cond
                                 ((and (window-input-string win)
                                       (< (window-input-cursor win)
                                          (length (window-input-string win))))
                                  (string (char (window-input-string win)
                                                (window-input-cursor win))))
                                 (t
                                  " "))
                               (window-input-background win)
                               (window-input-foreground win))
                              (subseq line (1+ (position (code-char 1) line))))))
              (collect line)))
      (setf (nth 0 lines) (subseq (nth 0 lines) (length (window-prompt win))))
      (setf (slot-value win 'window-input-rendered)
            lines)
      lines)))


(defmethod send-string-to-terminal ((win <Terminal-Window>) &optional str)
  (funcall (window-input-function win)
           (or str
               (window-input-string win)
               "")))


(defmethod send-string-to-terminal :around ((win <Terminal-Window>)
                                            &optional str)
  (declare (ignore str))
  (let ((retval (call-next-method)))
    (when (window-echo-input? win)
      (typecase retval
        (null nil)
        (string (add-message win retval))
        (list (dolist (line retval)
                (add-message win line)))
        (otherwise nil)))
    retval))


(defmethod send-to-window :around ((win <Terminal-Window>) (event <Key-Event>))
  (with-slots ((winx gui-event-winx) (winy gui-event-winy)
               (k gui-event-keypress)) event
    (cond
      ((window-input-active? win)
       (when (key-pressed k)
         (let ((pagelen (- (window-height win) 2))
               (num-items (length (window-items win))))
           (cond
             ((graphic-char-p (key-c k))
              (window-insert-character win (key-c k)))
             ((eql :backspace (key-vk k))
              (window-backspace-character win))
             ((eql :delete (key-vk k))
              (window-delete-character win))
             ((eql :enter (key-vk k))
              (send-string-to-terminal win)
              (setf (window-input-cursor win) 0)
              ;; (setf (nth (window-input-history-position win)
              ;;            (window-input-history win))
              ;;       (window-input-string win))
              (setf (window-input-history win)
                    (append
                     (remove ""
                             (append (window-input-history win)
                                     (list (window-input-string win)))
                             :test #'string=)
                     (list "")))
              (setf (window-input-history-position win)
                    (1- (length (window-input-history win))))
              (setf (window-input-string win) nil)
              (if (eql :transient (window-input-active? win))
                  (setf (window-input-active? win) nil)))
             ((eql :left (key-vk k))
              (when (plusp (window-input-cursor win))
                (decf (window-input-cursor win))
                (render-input-string win)))
             ((eql :right (key-vk k))
              (when (< (window-input-cursor win)
                       (length (window-input-string win)))
                (incf (window-input-cursor win))
                (render-input-string win)))
             ((eql :home (key-vk k))
              (setf (window-input-cursor win) 0)
              (render-input-string win))
             ((eql :end (key-vk k))
              (setf (window-input-cursor win)
                    (length (window-input-string win)))
              (render-input-string win))
             ((eql :up (key-vk k))
              (when (plusp (window-input-history-position win))
                ;; (setf (nth (window-input-history-position win)
                ;;            (window-input-history win))
                ;;       (window-input-string win))
                (decf (window-input-history-position win))
                (setf (window-input-string win)
                      (nth (window-input-history-position win)
                           (window-input-history win)))
                (setf (window-input-cursor win)
                      (length (window-input-string win)))
                (render-input-string win)))
             ((eql :down (key-vk k))
              (when (< (window-input-history-position win)
                       (1- (length (window-input-history win))))
                ;; (setf (nth (window-input-history-position win)
                ;;            (window-input-history win))
                ;;       (window-input-string win))
                (incf (window-input-history-position win))
                (setf (window-input-string win)
                      (nth (window-input-history-position win)
                           (window-input-history win)))
                (setf (window-input-cursor win)
                      (length (window-input-string win)))
                (render-input-string win)))
             (t
              (return-from send-to-window (call-next-method))))
           (constrain! (window-offset win) 0 (max 0 (- num-items pagelen)))
           (move-cursor-to win (window-offset win))
           ;;(constrain! (window-cursor win) 0 (max 0 (1- num-items)))
           ;; (warn "Now offset = ~D, cursor = ~D, num-items = ~D"
           ;;            (window-offset win) (window-cursor win) num-items)
           (prepare-window win)
           (redraw-window-area win))
         k))
      (t
       (call-next-method)))))


(defmethod window-insert-character ((win <Terminal-Window>) (ch character))
  (let ((instr (window-input-string win))
        (cur (window-input-cursor win)))
    (cond
      ((> (length instr) (* 3/4 (window-height win) (window-width win)))
       ;; input too long -- ignore request to input more characters.
       )
      ((null instr)
       (setf (window-input-string win) (string ch)))
      ((>= cur (length instr))
       (setf (window-input-string win) (concatenate 'string
                                                    instr (string ch))))
      ((zerop cur)
       (setf (window-input-string win) (concatenate 'string
                                                    (string ch) instr)))
      (t
       (setf (window-input-string win)
             (concatenate 'string (subseq instr 0 cur) (string ch)
                          (subseq instr cur)))))
    (incf (window-input-cursor win))
    (render-input-string win)))


(defmethod window-delete-character ((win <Terminal-Window>))
  (let ((instr (window-input-string win))
        (cur (window-input-cursor win)))
    (cond
      ((or (null instr)
           (zerop (length instr))
           (>= cur (length instr)))
       )
      ((zerop cur)
       (setf (window-input-string win) (subseq instr 1)))
      (t
       (setf (window-input-string win)
             (concatenate 'string (subseq instr 0 cur)
                          (subseq instr (1+ cur))))))))



(defmethod window-backspace-character ((win <Terminal-Window>))
  (let ((instr (window-input-string win))
        (cur (window-input-cursor win)))
    (cond
      ((or (null instr)
           (zerop (length instr))
           (zerop cur))
       (return-from window-backspace-character nil))
      ((>= cur (length instr))
       (setf (window-input-string win) (subseq instr 0 (1- cur))))
      (t
       (setf (window-input-string win)
             (concatenate 'string (subseq instr 0 (1- cur))
                          (subseq instr cur)))))
    (decf (window-input-cursor win))
    (render-input-string win)))




;;; <<Dialog Window>> =========================================================


(defvar *dialog->colour-table* nil
  "Hashtable DIALOG-BUTTON-VALUE -> COLOURNUM")
(defvar *colour->dialog-table* nil
  "Hashtable COLOURNUM -> DIALOG-BUTTON-VALUE")

(defvar *next-dialog-colour* 200
  "RGB value which maps to a unique dialog button.")
(defconstant +DIALOG-CONSOLE-DEFAULT-FOREGROUND+ :true-white
  "TODO document.")
(defconstant +DIALOG-CONSOLE-DEFAULT-BACKGROUND+ :true-black
  "TODO document.")

(defclass <Dialog-Window> (<Window>)
  ((dialog-console :accessor dialog-console :initform nil))
  (:documentation
     "Window where regions of text can be defined to produce particular effects
when clicked on."))


(defmethod initialize-instance :after ((win <Dialog-Window>) &key)
  (setf (dialog-console win)
        (console-new (window-width win) (window-height win)))
  (console-set-default-foreground (dialog-console win)
                                 (colour +DIALOG-CONSOLE-DEFAULT-FOREGROUND+))
  (console-set-default-background (dialog-console win)
                                 (colour +DIALOG-CONSOLE-DEFAULT-BACKGROUND+)))



(defmethod resize-window :after ((win <Dialog-Window>) width height)
  (setf (dialog-console win)
        (console-new width height))
  (console-set-default-foreground (dialog-console win)
                                 (colour (window-foreground win)))
  (console-set-default-background (dialog-console win)
                                 (colour (window-foreground win))))


(defun get-next-dialog-colour ()
  "TODO document."
  (iterate
    (while t)
    (multiple-value-bind
          (r g b) (decompose-colour (incf *next-dialog-colour*))
      (when (and (> r 0) (> g 0) (> b 0))
        (return-from get-next-dialog-colour *next-dialog-colour*)))))


(defun string->dialog-colour (str)
  "TODO document."
  (let ((col (gethash str *dialog->colour-table*)))
    (or col
        (progn
          (setf col (get-next-dialog-colour))
          (setf (gethash str *dialog->colour-table*) col)
          (setf (gethash col *colour->dialog-table*)
                str)
          col))))



(defmethod draw-string-at :after ((win <Dialog-Window>) str winx winy &key
                                  &allow-other-keys)
  (console-print (dialog-console win) winx winy
                 (make-coloured-string str :dialog? t :win win)))


(defmethod send-to-window ((win <Dialog-Window>) (event <Mouse-Hover-Event>))
  (with-slots ((winx gui-event-winx) (winy gui-event-winy)
               (data gui-event-focus)) event
    (unless data
      (setf data (gethash (console-get-char-foreground
                           (dialog-console win) winx winy)
                          *colour->dialog-table*)))
    (cond
      ((null data)
       (call-next-method)))))



(defmethod send-to-window :around ((win <Dialog-Window>) (event <Mouse-Event>))
  (with-slots ((winx gui-event-winx) (winy gui-event-winy)
               (mouse gui-event-mouse-state)) event
    (when (mouse-lbutton-pressed mouse)
      (let ((col (console-get-char-foreground (dialog-console win) winx winy)))
        (cond
          ((/= col (colour +DIALOG-CONSOLE-DEFAULT-FOREGROUND+))
           (if (gethash col *colour->dialog-table*)
               (send-to-window win (make-instance
                                    '<GUI-Dialog-Event>
                                    :string (gethash col *colour->dialog-table*)
                                    :winx winx :winy winy))))
          (t
           ;; Still here - no button clicked.
           (call-next-method)))))))




;;; <<Tooltip Window>> ========================================================


(defclass <Tooltip-Window> (<Dialog-Window>)
  ((window-raise-children-with-parent? :initform nil)
   (floating-window :accessor floating-window :initform nil))
  (:documentation
   "Window which displays floating 'tooltips' next to the mouse when hovering over
certain window regions."))


;;; <<Floating Window>>
(defclass <Floating-Window> (<Log-Window> <Ghost-Window>)
  ((window-framed? :initform nil)
  (window-can-resize? :initform nil)
  (window-can-drag? :initform nil)
  (last-mousex :accessor last-mousex :initform 0)
  (last-mousey :accessor last-mousey :initform 0)
  (last-text :accessor last-text :initform nil)
  (window-owner :initform nil))
  (:documentation
     "Window used to display the tooltips for a Tooltip-Window."))


(defmethod ok-to-show-tooltip? ((win <Tooltip-Window>))
  (or (not (typep win '<Window-With-Context-Menu>))
      (null (context-menu win))
      (window-hidden? (context-menu win))))


(defmethod process-window :before ((win <Floating-Window>))
  (unless (window-hidden? win)
    (when (or (/= *mouse-x* (last-mousex win))
              (/= *mouse-y* (last-mousey win)))
      ;;    (unless (eql (top-window-at *mouse-x* *mouse-y*) (window-owner win))
      (dirty-window win)
      (hide-window win))
    (setf (last-mousex win) *mouse-x*
          (last-mousey win) *mouse-y*)))


(defmethod raise-window :before ((win <Tooltip-Window>) &key &allow-other-keys)
  (unless (window-hidden? (floating-window win))
    (hide-window (floating-window win))))


(defmethod hide-window :before ((win <Tooltip-Window>) &key &allow-other-keys)
  (unless (window-hidden? (floating-window win))
    (hide-window (floating-window win))))



(defmethod initialize-instance :after ((win <Tooltip-Window>) &key)
  (setf (floating-window win)
        (make-instance '<Floating-Window>
                       :tlx 0 :tly 0
                       :width (calculate-floating-window-width win)
                       :height 3
                       :title "Tip" :foreground :white :background :dark-grey
                       :window-owner win
                       :hidden? t))
  (push (floating-window win) (window-children win)))


(defmethod calculate-floating-window-width ((win <Tooltip-Window>))
  (max 20 (floor (* 1/2 (window-width win)))))



(defmethod send-to-window :after ((win <Tooltip-Window>) (event <Mouse-Hover-Event>))
  (with-slots ((winx gui-event-winx) (winy gui-event-winy)
               (mouse gui-event-mouse-state)) event
    (let ((text (tooltip-text win (gui-event-focus event) winx winy)))
      (cond
        ((null text)
         ;; if focus is null or uninteresting, make sure the floating window
         ;; is NOT visible.
         )
        ((ok-to-show-tooltip? win)
         ;; Otherwise:
         ;; write the text into the floating-window
         (cond
           ((equal text (last-text (floating-window win)))
            (when (window-hidden? (floating-window win))
              (move-window-beside-mouse (floating-window win))
              (raise-window (floating-window win) :simple-redraw? nil)))
           (t
            (unless (window-hidden? (floating-window win))
              (hide-window (floating-window win)))
            (setf (last-text (floating-window win)) text)
            (clear-messages (floating-window win))
            (dolist (msg text)
              (add-message (floating-window win) msg))
            ;; resize the floating window
            (resize-window (floating-window win)
                           (calculate-floating-window-width win)
                           (min (floor (* 3/4 (screen-height)))
                                (+ 2 (length (window-items (floating-window win))))))
            (prepare-window (floating-window win))
            ;; move the floating window to an appropriate spot beside the mouse
            (move-window-beside-mouse (floating-window win))
            (raise-window (floating-window win) :simple-redraw? nil))))))))



(defun move-window-beside-mouse (win)
  "* Arguments:
- WIN: an instance of {defclass dormouse:<Window>}
* Returns: T if the new coordinates are different from the old.
* Description: Move the window WIN so that it is located next to the current
position of the mouse pointer. "
  (let ((width (window-width win))
        (height (window-height win))
        (oldx (window-tlx win))
        (oldy (window-tly win)))
    (move-window win
                 (constrain
                  (cond
                    ((> *mouse-x* (- (screen-width) width))
                     (- *mouse-x* width))
                    (t
                     (1+ *mouse-x*)))
                  0 (- (screen-width) width 1))
                 (constrain
                  (cond
                    ((> *mouse-y* (- (screen-height) height))
                     (- *mouse-y* height))
                    (t
                     (1+ *mouse-y*)))
                  0 (- (screen-height) height 1)))
    (not (and (= (window-tlx win) oldx)
              (= (window-tly win) oldy)))))



(defmethod tooltip-text ((win <Tooltip-Window>) datum winx winy)
  (declare (ignore winx winy))
  (cond
    ((null datum)
     nil)
    ((eql datum :unspecified)
     nil)
    (t
     (list "Tip is:"
           (make-coloured-string (format nil "~S" datum) :win win)))))


(defmethod tooltip-text :around ((win <Window>) datum winx winy)
  (declare (ignore datum winx winy))
  (let ((result (call-next-method)))
    (if (stringp result)
        (list result)
        ;; else
        result)))



;;; <<Viewport>> ==============================================================


(defclass <Viewport> (<Window>)
  ((window-transparency :initform +OPAQUE+)
   (map-console :accessor map-console :initform nil
                :documentation "The actual console which the viewport looks
onto.")
   (window-make-map? :accessor window-make-map? :initform t :initarg :make-map?)
   (window-map-shared :accessor window-map-shared :initform nil
                      :documentation "List of windows sharing this map")
   (map-xdim :accessor map-xdim :initarg :map-xdim :initform nil)
   (map-ydim :accessor map-ydim :initarg :map-ydim :initform nil)
   (view-tlx :accessor view-tlx :initform 0)   ; position of the viewport on
                                               ; the map
   (view-tly :accessor view-tly :initform 0))
  (:documentation
   "Window that looks onto a 'map' console, which is usually larger than the
viewport, but can be the same size or even smaller. "))



(defmethod initialize-instance :after ((win <Viewport>) &key)
  (unless (map-xdim win)
    (setf (map-xdim win) (+ 10 (window-width win))
          (map-ydim win) (+ 10 (window-height win))))
  (when (window-make-map? win)
    (setf (map-console win) (console-new (map-xdim win) (map-ydim win)))
    (console-set-default-foreground (map-console win)
                                   (colour (window-foreground win)))
    (console-set-default-background (map-console win)
                                   (colour (window-background win)))))


(defun viewport-width (win)
  "TODO document."
  (if (window-framed? win)
      (- (window-width win) 2)
      ;; else
      (window-width win)))


(defun viewport-height (win)
  "TODO document."
  (if (window-framed? win)
      (- (window-height win) 2)
      ;; else
      (window-height win)))


(defun view-brx (win)
  "TODO document."
  (+ (view-tlx win) (1- (viewport-width win))))


(defun view-bry (win)
  "TODO document."
  (+ (view-tly win) (1- (viewport-height win))))


(defun in-view? (win mapx mapy)
  "TODO document."
  (and (<= (view-tlx win) mapx (view-brx win))
       (<= (view-tly win) mapy (view-bry win))))


(defun in-viewport-map? (win mapx mapy)
  "TODO document."
  (and (< -1 mapx (map-xdim win))
       (< -1 mapy (map-ydim win))))


(defmethod prepare-window :after ((win <Viewport>))
  ;; Copy appropriate portion of map to viewport
  (when (map-console win)
    (copy-map-to-viewport win)))




(defmethod copy-map-to-viewport ((win <Viewport>))
  (let (;; Top left coordinates of MAP to copy
        (vtlx (view-tlx win))
        (vtly (view-tly win))
        ;; Top left coordinates to copy to, in WIN console
        (wintlx (+ (if (window-framed? win) 1 0)))
        (wintly (+ (if (window-framed? win) 1 0)))
        ;; Width and height of block of map to copy
        (width (- (window-width win) (if (window-framed? win) 2 0)))
        (height (- (window-height win) (if (window-framed? win) 2 0)))
        ;; Need to blank other areas of console?
        (edges-showing? nil))
    (cond
      ((< vtlx 0)
       (setf wintlx (+ wintlx (abs vtlx)))
       (setf width (+ width vtlx))
       (setf vtlx 0)
       (setf edges-showing? t))
      ((>= vtlx (- (map-xdim win) width))
       (setf width (- (map-xdim win) vtlx))
       (setf edges-showing? t)))
    (cond
      ((< vtly 0)
       (setf wintly (+ wintly (abs vtly)))
       (setf height (+ height vtly))
       (setf vtly 0)
       (setf edges-showing? t))
      ((>= vtly (- (map-ydim win) height))
       (setf height (- (map-ydim win) vtly))
       (setf edges-showing? t)))
    (when edges-showing?
      (console-rect (window-console win)
                    (if (window-framed? win) 1 0)
                    (if (window-framed? win) 1 0)
                    (- (window-width win) (if (window-framed? win) 2 0))
                    (- (window-height win) (if (window-framed? win) 2 0))
                    t :set))
    (console-blit (map-console win)
                  vtlx vtly
                  width height
                  (window-console win)
                  wintlx wintly
                  1.0 1.0)))



(defmethod clear-map ((win <Viewport>) &key (redraw *auto-redraw*))
  (console-clear (map-console win))
  (copy-map-to-viewport win)
  (when redraw
    (redraw-window-area win)))



(defmethod centre-viewport-on ((win <Viewport>) (mapx integer) (mapy integer))
  "Centre the viewport on map coordinates X, Y; does not refresh the screen"
  (setf (view-tlx win) (- mapx (floor (viewport-width win) 2)))
  (setf (view-tly win) (- mapy (floor (viewport-height win) 2))))



(defmethod share-map ((receiver <Viewport>) (giver <Viewport>) tlx tly)
  "Make RECEIVER look onto the same map console as GIVER. RECEIVER's viewport
is initially positioned with top left corner TLX,TLY on the map console."
  (when (map-console receiver)
      (dolist (w *window-stack*)
        (when (and (not (equal w receiver))
                   (typep w '<Viewport>)
                   (equal (map-console w) (map-console receiver)))
          (setf (window-map-shared w)
                (remove receiver (window-map-shared w))))))
  (setf (window-map-shared receiver) (copy-list (window-map-shared giver)))
  (dolist (w *window-stack*)
    (when (and (not (equal w receiver))
               (typep w '<Viewport>)
               (equal (map-console w) (map-console giver)))
      (pushnew receiver (window-map-shared w))))
  (setf (map-console receiver) (map-console giver))
  (setf (map-xdim receiver) (map-xdim giver))
  (setf (map-ydim receiver) (map-ydim giver))
  (setf (view-tlx receiver) tlx)
  (setf (view-tly receiver) tly)
  (prepare-window receiver))


(defmethod unshare-map ((receiver <Viewport>) (giver <Viewport>))
  (setf (window-map-shared receiver)
        (set-difference (window-map-shared receiver) (list giver receiver)))
  (setf (window-map-shared giver)
        (set-difference (window-map-shared giver) (list giver receiver))))



(defun all-windows (&key (except))
  "TODO document."
  (remove except (append *window-stack* *hidden-windows*)))



(defmethod destroy-window :before ((win <Viewport>))
  (cond
    ((and (window-map-shared win)
          (not (find win (window-map-shared win))))
     (dolist (sharer (window-map-shared win))
       (unshare-map win sharer))
     ;; do any other windows still use this console?
     (unless (find (map-console win) (all-windows :except win))
       (if (and (map-console win)
                (not (equal *root* (map-console win))))
           ;;(console-delete (map-console win))
           nil
           )))
    ;; else - only sharing with outselves
    ((if (and (map-console win)
              (not (equal *root* (map-console win))))
         nil
         ))))



(defun mapx->winx (vp x)
  "TODO document."
  (if (window-framed? vp)
      (1+ (- x (view-tlx vp)))
      ;; else
      (- x (view-tlx vp))))


(defun mapy->winy (vp y)
  "TODO document."
  (if (window-framed? vp)
      (1+ (- y (view-tly vp)))
      ;; else
      (- y (view-tly vp))))


(defun winx->mapx (vp x)
  "TODO document."
  (if (window-framed? vp)
      (1- (+ x (view-tlx vp)))
      ;; else
      (+ x (view-tlx vp))))


(defun winy->mapy (vp y)
  "TODO document."
  (if (window-framed? vp)
      (1- (+ y (view-tly vp)))
      ;; else
      (+ y (view-tly vp))))


(defmethod map-draw-char-at ((win <Viewport>) (ch integer) mapx mapy
                                &key (background-flag :set)
                                (fg nil) (bg nil) (redraw *auto-redraw*))
  (assert (map-console win))
  (console-draw-char-at (map-console win) ch mapx mapy
                        :background-flag background-flag
                        :fg fg :bg bg)
  (dolist (w (cons win (window-map-shared win)))
    (when (in-view? w mapx mapy)
      (console-blit (map-console win) mapx mapy
                    1 1
                    (window-console w)
                    (mapx->winx w mapx)
                    (mapy->winy w mapy)
                    1.0 1.0)
      (when redraw
        (redraw-all-at (winx->rootx w (mapx->winx w mapx))
                       (winy->rooty w (mapy->winy w mapy)))))))


(defmethod map-draw-char-at ((win <Viewport>) (ch character) mapx mapy
                             &key (background-flag :set)
                             (fg nil) (bg nil) (redraw *auto-redraw*))
  (map-draw-char-at win (char-code ch) mapx mapy
                        :background-flag background-flag
                        :fg fg :bg bg :redraw redraw))



(defmethod map-char-at ((win <Viewport>) mapx mapy)
  (code-char
   (console-get-char (map-console win) mapx mapy)))



(defmethod map-set-foreground-at ((win <Viewport>) colour mapx mapy
                                &key (redraw *auto-redraw*))
  (assert (map-console win))
  (console-set-char-foreground (map-console win) mapx mapy colour)
  (dolist (w (cons win (window-map-shared win)))
    (when (in-view? w mapx mapy)
      (console-blit (map-console win) mapx mapy
                    1 1
                    (window-console w)
                    (mapx->winx w mapx)
                    (mapy->winy w mapy)
                    1.0 1.0)
      (when redraw
        (redraw-all-at (winx->rootx w (mapx->winx w mapx))
                       (winy->rooty w (mapy->winy w mapy)))))))


(defmethod map-set-background-at ((win <Viewport>) colour mapx mapy
                                &key (redraw *auto-redraw*))
  (assert (map-console win))
  (console-set-char-background (map-console win) mapx mapy colour :set)
  (dolist (w (cons win (window-map-shared win)))
    (when (in-view? w mapx mapy)
      (console-blit (map-console win) mapx mapy
                    1 1
                    (window-console w)
                    (mapx->winx w mapx)
                    (mapy->winy w mapy)
                    1.0 1.0)
      (when redraw
        (redraw-all-at (winx->rootx w (mapx->winx w mapx))
                       (winy->rooty w (mapy->winy w mapy)))))))



(defmethod send-to-window ((win <Viewport>) (event <Mouse-Hover-Event>))
  nil)


(defmethod send-to-window ((win <Viewport>) (event <GUI-Mouse-Drag-Event>))
  (with-slots ((winx gui-event-winx) (winy gui-event-winy)
               (rodent gui-event-mouse-state)) event
    (let* ((start-cx (mouse-cx rodent))
           (start-cy (mouse-cy rodent))
           (started-dragging? nil)
           (prev-cx start-cx) (prev-cy start-cy)
           (deltax 0) (deltay 0))
      (iterate
        (while (mouse-lbutton (setf rodent (mouse-get-status))))
        ;; Update position of WIN based on mouse position
        ;; offsetx = cx - view-tlx
        ;; therefore cx - offsetx = current view-tlx
        ;;
        (setf deltax (- prev-cx
                        (constrain (mouse-cx rodent)
                                   (window-tlx win)
                                   (window-brx win))))
        (setf deltay (- prev-cy
                        (constrain (mouse-cy rodent)
                                   (window-tly win)
                                   (window-bry win))))
        (unless (and (= deltax deltay 0))
          (unless started-dragging?
            (setf started-dragging? t)
            (raise-window win)
            (prepare-window win)
            (redraw-window win)
            (console-flush))
          (incf (view-tlx win) deltax)
          (incf (view-tly win) deltay)
          (setf prev-cx (mouse-cx rodent))
          (setf prev-cy (mouse-cy rodent))
          (prepare-window win)
          (redraw-window win)
          (console-flush)))
      (cond
        (started-dragging?
         (redraw-window-area win)
         (console-flush))
        (t ;; never actually dragged - interpret as left-click
         (send-to-window win (make-instance '<Mouse-Event>
                                            :mouse-state
                                            (tcod:make-mouse :lbutton-pressed t)
                                            :winx winx :winy winy)))))))





;;; <<Top-level functions>> ===================================================


(defvar *screen-resolution-x* nil)
(defvar *screen-resolution-y* nil)


(defun start-gui (&key
                  (title "The Doryen Library")
                  (width nil)
                  (height nil)
                  (font-file *default-font-file*)
                  (fps 20)
                  ;;(font-file-chars-in-columns? t)
                  )
  "* Arguments:
- TITLE: String, used as the title displayed at the top of the TCOD
window.
- WIDTH: Desired width of the TCOD root console, in characters. Default 80.
  If NIL or not given, the root console width will be auto-calculated
  based on screen resolution and font size.
- HEIGHT: Desired height of the TCOD root console, in characters. Default 25.
  If NIL or not given, the root console height will be auto-calculated
  based on screen resolution and font size.
- FONT-FILE: Path to the font file to use. The default is
[[*default-font-file*]].
* Returns: None.
* Description: Initialise the TCOD library and prepare for running
the GUI.
* See Also: [[main-gui-loop]]"
;;;                +DEFAULT-FONT-FILE-CHARS-IN-COLUMNS?+))
;;;               (font-file-columns +DEFAULT-FONT-FILE-COLUMNS+)
;;;               (font-file-rows +DEFAULT-FONT-FILE-ROWS+)
;;;               (font-file-background-colour
;;;                +DEFAULT-FONT-FILE-BACKGROUND-COLOUR+))
  ;;(declare (type (integer 1) width height))
  (if *dialog->colour-table*
      (clrhash *dialog->colour-table*)
      ;; else
      (setf *dialog->colour-table* (make-hash-table :test #'equal)))
  (if *colour->dialog-table*
      (clrhash *colour->dialog-table*)
      ;; else
      (setf *colour->dialog-table* (make-hash-table :test #'eql)))
  (when font-file
    (assert (probe-file font-file))
    (console-set-custom-font font-file
                             (if (listp *default-font-layout*)
                                 *default-font-layout*
                                 (list *default-font-layout*))
;;;                        (if font-file-chars-in-columns?
;;;                            '(:font-layout-ascii-in-col)
;;;                            '(:font-layout-ascii-in-row))
                             0 0)) ;; TCOD automatically deduces WxH
;;;                        font-file-columns font-file-rows
;;;                        font-file-chars-in-columns?
;;;                        (colour font-file-background-colour))

  ;; Store screen resolution, then auto-calculate a sensible width and
  ;; height based on screen size and character size (unless a particular
  ;; width and height have been specified)

  (unless (numberp *screen-resolution-x*)
    (multiple-value-bind (scr-xdim scr-ydim) (sys-get-current-resolution)
      (setf *screen-resolution-x* scr-xdim
            *screen-resolution-y* scr-ydim)))

  (multiple-value-bind (ch-xdim ch-ydim) (sys-get-char-size)
    (unless width
      (setf width (floor (* 0.8 (/ *screen-resolution-x* ch-xdim)))))
    (unless height
      (setf height (floor (* 0.8 (/ *screen-resolution-y* ch-ydim))))))

  (console-init-root width height title nil :RENDERER-SDL)
  (start-colours)
  (setf *window-stack* nil)
  (sys-set-fps fps)                     ; limit FPS (and CPU load)
  (setf *temp-con*
        (console-new width height))     ; must be same size as ROOT
  (setf *scratch*
        (console-new width height))     ; must be same size as ROOT
  (console-clear *root*))


(defun legal-window-coordinates? (win x y)
  (tcod:legal-console-coordinates? (window-console win) x y))


(defun process-windows ()
  "* Arguments: None.
* Returns: None.
* Description: Called once for each iteration of {defun dormouse:main-gui-loop}.
Calls process-window for each non-hidden window.
* See Also: "
  (dolist (win (all-windows))
    (process-window win)))


(defvar *rodent* nil)
(defvar *last-rodent* nil)
(defvar *topwin* nil)
(defvar *last-topwin* nil)

(defun main-gui-loop-aux ()
  (let ((k nil)
        (mouse-hover-event (make-instance '<Mouse-Hover-Event>)))
    (iterate
     (setf k (console-check-for-keypress '(:key-pressed :key-released)))
     (if k (leave))
     (process-windows)
     (setf *rodent* (mouse-get-status))
     (unless (equal *rodent* *last-rodent*)
       ;; Deal with mouse events
       (setf *mouse-x* (mouse-cx *rodent*)
             *mouse-y* (mouse-cy *rodent*)
             *topwin* (window-with-focus))
       (setf *focus-changed?* (not (eql *topwin* *last-topwin*)))
       (cond
         ;; L button clicked
         ((mouse-lbutton-pressed *rodent*)
          (when *topwin*
            (cond
              ((and (=  *mouse-y* (window-tly *topwin*))
                    (=  *mouse-x* (window-brx *topwin*))
                    (window-can-close? *topwin*))
               ;; Clicked on 'X' in top right corner
               (hide-window *topwin*))
              (t
               ;; Clicked somewhere else on the window
               (raise-window *topwin*)
               (send-to-window
                *topwin*
                (make-instance '<Mouse-Event> :winx (- *mouse-x* (window-tlx *topwin*))
                                              :winy (- *mouse-y* (window-tly *topwin*))
                                              :mouse-state *rodent*))))))

         ;; R button clicked
         ((mouse-rbutton-pressed *rodent*)
          (when *topwin*
            (raise-window *topwin*)
            (send-to-window
             *topwin*
             (make-instance '<Mouse-Event> :winx (- *mouse-x* (window-tlx *topwin*))
                                              :winy (- *mouse-y* (window-tly *topwin*))
                                              :mouse-state *rodent*))))

         ;; Middle button clicked
         ((mouse-mbutton-pressed *rodent*)
          (when *topwin*
            (raise-window *topwin*)
               (send-to-window
                *topwin*
                (make-instance '<Mouse-Event> :winx (- *mouse-x* (window-tlx *topwin*))
                                              :winy (- *mouse-y* (window-tly *topwin*))
                                              :mouse-state *rodent*))))

         ;; L button held down
         ((mouse-lbutton *rodent*)
          (when *topwin*
            (raise-window *topwin*)
            (let ((start (get-internal-real-time))
                  (dragged? nil))
              (declare (ignorable dragged?))
              (iterate
               (while (mouse-lbutton (setf *rodent* (mouse-get-status))))
               (unless (and (< (get-internal-real-time)
                               (+ start (/ (* *drag-delay* 1000)
                                           internal-time-units-per-second)))
                            (zerop (mouse-dx *rodent*))
                            (zerop (mouse-dy *rodent*)))
                 (setf dragged? t)
                 (cond
                   ((and (window-can-drag? *topwin*)
                         (on-upper-window-border?
                          *topwin*
                          (- *mouse-x* (window-tlx *topwin*))
                          (- *mouse-y* (window-tly *topwin*))))
                    ;; Dragging on title bar - move the window
                    (mouse-drag-window *topwin* *rodent*))
                   ((and (window-can-resize? *topwin*)
                         (= *mouse-y* (+ (window-tly *topwin*)
                                         (1- (window-height *topwin*))))
                         (= *mouse-x* (+ (window-tlx *topwin*)
                                         (1- (window-width *topwin*)))))
                    ;; Dragging on bottom right corner
                    (mouse-resize-window *topwin* *rodent*))
                   (t
                    (send-to-window
                     *topwin*
                     (make-instance '<GUI-Mouse-Drag-Event>
                               :winx (- *mouse-x* (window-tlx *topwin*))
                               :winy (- *mouse-y* (window-tly *topwin*))
                               :mouse-state *rodent*))))
                 (return))))))

         (t                             ; mouse "just hovering"
                                        ;(break)
          (when *topwin*
            (setf (gui-event-winx mouse-hover-event)
                  (- *mouse-x* (window-tlx *topwin*)))
            (setf (gui-event-winy mouse-hover-event)
                  (- *mouse-y* (window-tly *topwin*)))
            (setf (gui-event-mouse-state mouse-hover-event) *rodent*)
            (send-to-window *topwin* mouse-hover-event))))
       ;; CONSOLE-FLUSH should not be done every iteration, which
       ;; is what was happening below.
       ;; This has been changed so now we only flush the console if the
       ;; mouse moves/mouse status changes, or a key is pressed.
       ;; It seems to work.
       ;;
       ;; Alternative plan could be to time since last flush, limit to
       ;; < 20 fps.
       (unless (equal *rodent* *last-rodent*)
         (setf *last-rodent* *rodent*)
         (console-flush))
       ))
    (when k
      (case (key-vk k)
        (:shift (setf *shift* (key-pressed k)))
        (:control (setf *ctrl* (key-pressed k)))
        (:alt (setf *alt* (key-pressed k)))
        (:printscreen (tcod:sys-save-screenshot))
        (otherwise
         ;; send (vk k) to window under mouse
         (when (setf *topwin* (window-with-focus))
           (send-key-to-window *topwin* k
                               (- *mouse-x* (window-tlx *topwin*))
                               (- *mouse-y* (window-tly *topwin*)))))))))



(defun main-gui-loop ()
  "* Arguments
None.
* Returns
None.
* Description

The function [[start-gui]] must be called to initialise libtcod prior to this
function being called.

When called, begins 'running' the currently defined window system. All existing
non-hidden windows are displayed, and a loop is entered where keyboard and
mouse events are intercepted and handled or sent to window objects for
handling. A special case is the PrintScreen key, which calls the TCOD library
function =sys-save-screenshot=.

The loop runs until the global variable [[*exit-gui?*]] is non-nil.
"
  (let ((*rodent* (make-mouse))
        (*last-rodent* (make-mouse))
        (*topwin* nil) (*last-topwin* nil))
    (redraw-all-windows)
    (setf *exit-gui?* nil)
    ;; Main loop
    (iterate
      (until (or *exit-gui?*
                 (console-is-window-closed?)))
      ;; The following snippet is taken from
      ;; http://code.google.com/p/lispbuilder/wiki/PortableInteractiveDevelopment
      ;; and allows swank to process commands from slime during the game loop.
      #+swank (let ((connection
                      (or swank::*emacs-connection* (swank::default-connection))))
                (when (and connection (not (eql swank:*communication-style* :spawn)))
                  (swank::handle-requests connection t)))
      (console-flush)
      ;; Inner part of loop moved into separate function, to allow redefinition
      ;; while within the loop.
      (main-gui-loop-aux))))



(defun resume-gui ()
  "* Arguments: None.
* Returns: None.
* Description: Resume running the currently defined window system."
  (console-flush)
  (main-gui-loop))


;;;; dormouse.lisp ends here =================================================
