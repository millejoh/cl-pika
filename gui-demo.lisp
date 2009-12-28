;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*- ;;;;;;;;;;;;;;;;;80

;;;; guidemo executable created in Clozure Common Lisp:
;;;; (ccl:save-application "guidemo" :toplevel-function #'gui-demo:gui-demo
;;;;    :prepend-kernel t)
;;;;
;;;; Application then compressed with gzexe:
;;;; $ gzexe guidemo

(in-package :cl-user)

(defpackage :tcod.gui-demo
  (:nicknames :gui-demo)
  (:use :cl :tcod :dormouse)
  (:export #:gui-demo))

(in-package :tcod.gui-demo)

;;; Global variables which will hold references to the various window objects,
;;; etc

(defvar *player* nil)
(defvar *tipwin* nil)
(defvar *dlgwin* nil)
(defvar *menuwin* nil)
(defvar *statwin* nil)
(defvar *msgwin* nil)
(defvar *viewport* nil)
(defvar *smallvp* nil)
(defvar *listwin* nil)
(defvar *alertwin* nil)
(defvar *custom-colours*
  `((:green 		68 158 53)
    (:red 		151 26 26)
    (:magenta		255 110 87)
    (:light-grey	185 192 162)
    (:grey		185 192 162)
    (:dark-grey		88 83 86)
    (:light-blue	145 202 255)
    (:light-green	131 212 82)
    (:light-cyan	176 223 215)
    (:light-red		255 34 34)
    (:light-magenta	255 167 246)
    (:yellow		255 218 90)
    (:brown	 	120 94 47))
  "* Description: List of lists. Each sublist has the form (COLOURNAME R G B).
COLOURNAME is a keyword that will be associated with the new colour.
R, G and B are byte values (0-255) that define the new colour.")

(defvar *maze-file* "maze.txt")
(defvar *font-file* "MDA9x14.png")

;;; Modify default behaviour of base window class and list windows, so
;;; that they print messages on the bottom of the screen giving information
;;; about mouse position and events.

(defmethod send-to-window ((win <Window>) data parm winx winy)
  (case data
    (:hover
     (bottom-message "[~C~C~C] ~40,'0B   "
                     (if *shift* #\S #\space)
                     (if *ctrl* #\C #\space)
                     (if *alt* #\A #\space)
                     parm))
    (otherwise
     (console-print-right *root* 0 (1- (console-get-height *root*))
                          :set "Window ~A received event ~S at (~D, ~D) "
                          win data winx winy))))


(defmethod send-to-window ((win <List-Window>) (data (eql :hover))
			   parm winx winy)
  (declare (ignore winx winy))
  (bottom-message "Current selected item: ~S" parm))


;;; Make our own subclass of `Viewport' so we can modify its behaviour.
;;; We also inherit from Background-Window - this will ensure MyViewport
;;; objects always stay at the bottom of the window stack (all other
;;; windows float above them).

(defclass <MyViewport> (<Viewport> <Background-Window>)
  ((window-framed? :initform nil)
   (cursorx :accessor cursorx :initform  20)
   (cursory :accessor cursory :initform  18)
   (cursor-char :accessor cursor-char :initform  #\@)))



(defmethod initialize-instance :after ((win <MyViewport>) &key)
  (let ((maze
         (with-open-file (s *maze-file* :direction :input)
           (loop while (listen s)
              collect (read-line s nil)))))
    (loop for x from 0 below (map-xdim win)
       do (loop for y from 0 below (map-ydim win) do
	       (let ((it (char (nth (1+ y) maze) (1+ x))))
		 (cond 
		   ((equal #\# it)
		    (map-draw-char-at win #\# x y :fg :light-blue
				      :redraw nil))))))
    (map-draw-char-at win (cursor-char win)
                      (cursorx win) (cursory win)
                      :fg :yellow)))


(defmethod send-key-to-window :around ((win <MyViewport>) (k key) winx winy)
  (declare (ignore winx winy))
  (let ((old-cx (cursorx win))
        (old-cy (cursory win)))
    (when (key-pressed k)
      (case (key-vk k)
        (:up (decf (cursory win))) 
        (:down (incf (cursory win))) 
        (:left (decf (cursorx win))) 
        (:right (incf (cursorx win)))
        (:char
         (setf (cursor-char win) (key-c k))
         (add-message *msgwin* "You pressed `~C'." (key-c k))
         (prepare-window *msgwin*)
         (redraw-window-area *msgwin*)
					;(console-flush)
         (if (eql #\q (key-c k))
             (setf *exit-gui?* t)))
        (otherwise
         (return-from send-key-to-window (call-next-method))))
      (setf (cursorx win) (max 0 (cursorx win)
                               (min (cursorx win) (1- (window-width win)))))
      (setf (cursory win) (max 0 (cursory win)
                               (min (cursory win) (1- (window-height win)))))
      (cond
        ((eql #\# (map-char-at win (cursorx win) (cursory win)))
         (setf (cursorx win) old-cx
               (cursory win) old-cy)
         (raise-window *alertwin*))
        (t
         (map-draw-char-at win #\space old-cx old-cy :fg :yellow)
         (map-draw-char-at win (cursor-char win) (cursorx win) (cursory win)
                           :fg :yellow :redraw t))))))


(defmethod send-to-window ((win <MyViewport>) (data (eql :left)) parm winx winy)
  (declare (ignore parm))
  (map-draw-char-at win (char-code #\#)
		    (winx->mapx win winx)
		    (winy->mapy win winy)
		    :fg :green :redraw t))


(defmethod send-to-window ((win <MyViewport>) (data (eql :hover)) parm winx winy)
  (declare (ignore parm))
  (bottom-message "[~C~C~C] MAP POSITION = ~3D ~3D"
		  (if *shift* #\S #\space)
		  (if *ctrl* #\C #\space)
		  (if *alt* #\A #\space)
		  (winx->mapx win winx)
		  (winy->mapy win winy)))


(defclass <MyDialog-Window> (<Dialog-Window>)
  ())


(defmethod prepare-window :after ((win <MyDialog-Window>))
  (draw-string-at
   win
   "Click me {fg:light-blue,click:1}here{/} and {fg:light-green,click:2}here{/}!"
   1 1))


(defmethod send-to-window ((win <MyDialog-Window>) (data (eql :dialog))
                                   parm winx winy)
  (add-message-and-redraw *msgwin* "You clicked button `~A'." parm))


(defclass <MyMenu-Window> (<Menu-Window>)
  ())


(defmethod send-to-window ((win <MyMenu-Window>) (data (eql :select))
                                   parm winx winy)
  (add-message-and-redraw *msgwin* "You chose `~A'." parm))


(defclass <MyTooltip-Window> (<Tooltip-Window>)
  ())


(defmethod prepare-window :after ((win <MyTooltip-Window>))
  (draw-string-at
   win
   "Hover over {fg:red,click:TIPDATA}me{/} for a tip!"
   1 1))


(defmethod tooltip-text :around ((win <MyTooltip-Window>) datum winx winy)
  (declare (ignore winx winy))
  (cond
    ((member datum (list nil :unspecified))
     (call-next-method))
    (t
     (list "Here is the tip:"
	   (format nil "{fg:red}~S{/}" datum)
           "Don't swim in shark-infested waters."))))


(defclass <Player> ()
  ((hit-points :initform 30 :accessor hit-points)
   (max-hit-points :initform 50 :accessor max-hit-points)
   (stamina :initform 100 :accessor stamina)
   (max-stamina :initform 120 :accessor max-stamina)))


(defclass <Statistics-Window> (<Tooltip-Window>)
  ((window-framed? :initform nil)
   (window-can-resize? :initform nil)
   (window-can-drag? :initform t)
   (window-auto-redraw? :initform t)
   (window-can-close? :initform nil)))


(defmethod prepare-window :after ((win <Statistics-Window>))
  (draw-string-at win (bar-chart (- (window-width win) 2)
				 (hit-points *player*) (max-hit-points *player*)
                                 :text :percent
				 :bar-colour :light-red
				 :empty-bar-colour :dark-red)
		  1 1 )
  (draw-string-at win (bar-chart (- (window-width win) 2)
				 (stamina *player*)
				 (max-stamina *player*)
                                 :text :fraction
				 :bar-colour :light-blue
				 :empty-bar-colour :dark-blue)
		  1 2 ))



(defmethod tooltip-text ((win <Statistics-Window>) datum winx winy)
  (declare (ignore datum))
  (flet ((percent (num denom)
           (floor (* num 100) denom)))
    (when (> winx 0)
      (case winy
        (1 (list (format nil "Hits: ~D/~D (~D%%)" (hit-points *player*)
                         (max-hit-points *player*)
                         (percent (hit-points *player*) (max-hit-points *player*)))))
        (2 (list (format nil "Stamina: ~D/~D (~D%%)" (stamina *player*)
                         (max-stamina *player*)
                         (percent (stamina *player*)
                                  (max-stamina *player*)))))
        (otherwise nil)))))



(defun gui-demo ()
  (let ((width 0)
	(height 0))
    (setf *player* (make-instance '<Player>))
    (start-gui :title "Dormouse demo" :width 100 :height 50
               :font-file *font-file*)
    (dolist (col *custom-colours*)
      (destructuring-bind (name r g b) col
	(tcod:make-colour name r g b)))
    
    (setf width (tcod:console-get-width tcod:*root*)) 
    (setf height (tcod:console-get-height tcod:*root*))
    ;; Make viewport. Make it slightly shorter than HEIGHT to
    ;; allow messages on the bottom line of the root console.
    (setf *viewport* (make-instance '<MyViewport>
				    :foreground :light-blue
				    :background :black
				    :width width :height (1- height)))
    (setf *smallvp* (make-instance '<Viewport> :tlx 58 :tly 1 :width 12
				 :height 12
				 :foreground :pink :background :green
				 ))
    (share-map *smallvp* *viewport* 15 13)
    (setf *tipwin* (make-instance '<MyTooltip-Window> :foreground :black
				:background :yellow
				:tlx 6 :tly 25 :width 30 :height 11
				:title "Tooltips"
                                :transparency +OPAQUE+))
    ;; Make windows
    (setf *dlgwin*
	  (make-instance '<MyDialog-Window> :tlx 8 :tly 3 :width 25 :height 8
			 :title "Dialog" :foreground :grey :background :red
                         :transparency +OPAQUE+))
    (make-instance '<Window> :tlx 50 :tly 30 :width 20 :height 12
		   :title "win2" :foreground :yellow
		   :background :dark-blue)
    (setf *alertwin*
	  (make-instance '<Alert-Window> :tlx 6 :tly 15 :width 20 :height 8
			 :title "Alert!" :foreground :yellow :background :red
			 :hidden? t :transparency +OPAQUE+ :text
                         "Your path is blocked by a wall! Click 'X' to close."))
    (setf *listwin*
	  (make-instance '<Filtered-Window> :tlx 21 :tly 38 :width 20 :height 9
			 :title "list" :foreground :green
			 :background :chocolate :transparency +OPAQUE+))
    (add-item *listwin* "a" "a. '{fg:green}apples{/}'" (make-simple-key #\a))
    (add-item *listwin* "b" "b. '{fg:red}plums{/}'" (make-simple-key #\b))
    (add-item *listwin* "c" "c. '{fg:yellow}bananas{/}'" (make-simple-key #\c))
    (add-item *listwin* "d" "d. '{fg:brown}potatoes{/}'" (make-simple-key #\d))
    (add-item *listwin* "e" "e. '{fg:pink}yams{/}'" (make-simple-key #\e))
    (setf *msgwin*
	  (make-instance '<Log-Window> :tlx 65 :tly 13 :width 33 :height 35
			 :title "Messages" :foreground :light-blue
			 :background :dark-blue :transparency +OPAQUE+))
    (setf *menuwin*
	  (make-instance '<MyMenu-Window> :tlx 40 :tly 10 :width 15 :height 7
			 :title "list" :foreground :light-blue
			 :background :dark-grey))
    (setf *statwin*
          (make-instance '<Statistics-Window> :tlx 3 :tly 43 :width 12 :height 4
                         :background :dark-grey))
    (add-item *menuwin* "item1" "Menu item 1" (make-simple-key #\1))
    (add-item *menuwin* "item2" "Menu item 2" (make-simple-key #\2))
    (add-item *menuwin* "item3" "Menu item 3" (make-simple-key #\3))
    (add-item *menuwin* "item4" "Menu item 4" (make-simple-key #\4))
    (add-message *msgwin* "Press 'Q' to quit.")
    (add-message *msgwin* "{yellow}The window where the mouse is hovering has focus.{/}")
    (add-message *msgwin* "Move windows by dragging on their titles.")
    (add-message *msgwin* "Resize by dragging in bottom right corner of window frame.")    
    (add-message *msgwin* "Close by clicking 'X' in top right corner.")    
    (add-message *msgwin* "Type in the list window to filter by a text string.")
    (add-message *msgwin* "Click and drag to move the map around in the background.")
    (add-message *msgwin* "Click the active text in the 'Dialog' window.")
    (add-message *msgwin* "Drag in the small green-and-pink window to view other parts of the background map.")
    (add-message *msgwin* "Press cursor keys to move the yellow '@' around the maze.")
    (main-gui-loop)))



