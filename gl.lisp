(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "cl-opengl")
  (ql:quickload "cl-glut")
  (ql:quickload "cxml"))

(defpackage :bla
  (:shadowing-import-from :cl close get special)
  (:use :cl :gl :glut))
(in-package :bla)


;; use amdcccle for configuration of ATI graphics card
;; http://www.stolk.org/debian/vblank-fglrx.html

;; Option "Capabilities" "0x00000800"
;; It turned out that bit 11 in this capabilities bitfield enables vblanking.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun build-var (classname var)
    (list var 
	  :initform nil
	  :accessor (intern (concatenate 'string (string classname) "-" 
					 (string var)))
	  :initarg (intern (string var) :keyword)))
  
  (defun build-varlist (classname varlist)
    (loop for var in varlist 
       collect (build-var classname var))))


(defmacro defobject (name ancestors &rest varlist)
  "Defines a class with a set of behavior. 
   Variables are accessed by name-varname.

   (defobject classname v1 v2 v3)
  "
  `(defclass ,name ,ancestors
     ,(build-varlist name varlist)))

(defobject fenster (window)
  draw-func)

(defun current-time ()
  (multiple-value-bind (sec usec)
      (sb-ext:get-time-of-day)
    (+ sec (/ usec 1000000))))

(let* ((start 0)
       (end 0)
       (count-max 12)
       (count count-max)
       (frame-rate 0))
  (defun measure-frame-rate ()
    (when (= 0 count)
      (setf end (current-time)
            frame-rate (/ (* 1s0 count-max)
                          (- end start))
            count count-max
            start (current-time)))
    (decf count))
  (defun get-frame-rate ()
    frame-rate))


(defmethod display ((w fenster))
  (clear :color-buffer-bit 
	 #+Nil :depth-buffer-bit)
  (load-identity)
  
  (funcall (fenster-draw-func w))
    
  (measure-frame-rate)
  
  (post-redisplay))





(defmethod keyboard ((w fenster) key x y)
  (case key
    (#\Esc (destroy-current-window))))

(defmacro with-gui ((w &optional (h w) (x 0) (y 0)) &body body)
  `(display-window 
    (make-instance 'bla::fenster
                   :mode '(:double :rgba :depth)
                   :width ,w :height ,h
                   :pos-x ,x :pos-y ,y 
                   :draw-func #'(lambda ()
                                  ,@body))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *numerals*
 (let ((doc (cxml:parse-file "numerals.svg" (cxml-dom:make-dom-builder))))
   (loop for i below 10 collect
	(list i 
	      (dom:get-attribute
	       (elt (dom:get-elements-by-tag-name doc "path") i) "d")))))

  (defun split (char str)
    (let ((start 0))
      (loop as pos = (position char str :start start)
	 while pos collect
	   (progn
	     (setf start (1+ pos))
	     pos))))

  (defun split-at-comma-space (str)
    (let ((pos (sort (append (split #\Space str)
			     (split #\, str))
		     #'<)))
      (append 
       (list (subseq str 0 (elt pos 0)))
       (loop for i below (1- (length pos)) collect
	    (subseq str (1+ (elt pos i)) (elt pos (1+ i))))
       (list (subseq str (1+ (first (last pos))))))))

  (defparameter *svg-commands*
    '((r-translate 2)
      (translate 2)
      (r-cubic-bezier 6)
      (cubic-bezier 6)
      (r-line-to 2)
      (line-to 2)
      (end-loop 0)))

  (declaim (optimize (debug 3)))


  (defun svg-path-d-to-lisp (cmds)
    (let ((last-cmd nil)
	  (res nil))
      (loop for i below (length cmds)
	 do
	   (let* ((current-cmd (cond ((string= "m" (elt cmds i)) 'r-translate)
				     #+nil ((string= "M" (elt cmds i)) 'translate)
				     ((string= "c" (elt cmds i)) 'r-cubic-bezier)
				     ((string= "C" (elt cmds i)) 'cubic-bezier)
				     ((string= "l" (elt cmds i)) 'r-line-to)
				     ((string= "L" (elt cmds i)) 'line-to)
				     ((string= "z" (elt cmds i)) 'end-loop)
				     (t  (decf i)
					 'repeat))))
	     (unless (eq current-cmd 'repeat)
	       (setf last-cmd current-cmd))
	     (let ((nr-args (first (cdr (assoc last-cmd *svg-commands*)))))
	       (push `(,last-cmd ,@(loop for j below nr-args collect 
					(let ((arg (read-from-string (elt cmds (incf i)))))
					  (unless (numberp arg)
						 (break "parse error, expected number got ~a." arg))
					  arg)))
		     res))))
      (reverse res)))


  (defun expand-relative-bezier (args &key (n 10))
    (destructuring-bind (x1 y1 x2 y2 x3 y3) args
      (let* ((ox 0s0)
	     (oy 0s0))
	(loop for i upto n collect
	     (let* ((u (/ (* 1s0 i) n))
		    (v (- 1s0 u))
		    (uu (* u u))
		    (vv (* v v))
		    (x (+ #+nil (* vv v x0) (* 3 vv u x1) (* 3 uu v x2) (* uu u x3)))
		    (y (+ #+nil (* vv v y0) (* 3 vv u y1) (* 3 uu v y2) (* uu u y3))))
	       (prog1
		   `(r-line-to ,(- x ox)
			       ,(- y oy))
		 (setf ox x
		       oy y)))))))

  #+nil
  (expand-relative-bezier '(-1.1 1.2 -2.5 1.3 -4 1.4))

  (defun expand-absolute-bezier (last-absolute-point args &key (n 10))
    (destructuring-bind (cmd x0 y0) last-absolute-point
      (unless (eq cmd 'line-to)
	(break "unexpected command ~a, at this stage there should be only line-to" cmd))
      (destructuring-bind (x1 y1 x2 y2 x3 y3) args
	(loop for i upto n collect
	     (let* ((u (/ (* 1s0 i) n))
		    (v (- 1s0 u))
		    (uu (* u u))
		    (vv (* v v))
		    (x (+ (* vv v x0) (* 3 vv u x1) (* 3 uu v x2) (* uu u x3)))
		    (y (+ (* vv v y0) (* 3 vv u y1) (* 3 uu v y2) (* uu u y3))))
	       `(line-to ,x ,y))))))

  (defun expand-all-relative-bezier-into-lines (ls &key (n 10))
    (let ((res nil))
      (dolist (e ls)
	(destructuring-bind (cmd &rest rest) e
	  (cond ((eq cmd 'r-cubic-bezier)
		 (let ((lines (expand-relative-bezier rest :n n)))
		   (dolist (f lines)
		     (push f res))))
		(t (push e res)))))
      (reverse res)))

  
  (defun accumulate-relative-coordinates (ls)
    (let ((res nil)
	  (x 0s0)
	  (y 0s0))
      (dolist (e ls)
	(destructuring-bind (cmd &rest rest) e
	  (cond ((eq cmd 'r-line-to)
		 (destructuring-bind (xx yy) rest
		   (incf x xx)
		   (incf y yy)
		   (push `(line-to ,x ,y) res)))
		((eq cmd 'line-to)
		 (destructuring-bind (xx yy) rest
		   (setf x xx)
		   (setf y yy)
		   (push `(line-to ,x ,y) res)))
		((eq cmd 'r-translate)
		 (destructuring-bind (xx yy) rest
		   (incf x xx)
		   (incf y yy)
		   (push `(translate ,x ,y) res)))
		((eq cmd 'cubic-bezier)
		 (dolist (e (expand-absolute-bezier (first res) rest))
		   (push e res))
		 (setf x (second (first res))
		       y (third (first res))))
		((eq cmd 'end-loop))
		(t (break "unexpected command ~a" cmd)))))
      (reverse res)))


  (defun svg-to-immediate-opengl (ls)
    (let ((res))
      (dolist (e ls)
	(destructuring-bind (cmd x y) e
	  (cond ((eq cmd 'line-to)
		 (push `(vertex ,x ,y) res))
		((or (eq cmd 'translate))
		 (push `(vertex ,x ,y) res))
		(t (break "unexpected command ~a" cmd)))))
      (reverse res))))

(defmacro draw-one ()
  (let ((one "m 0.125,.875 c -1.187999,1.231999 -2.592001,1.3935 -4,1.4375 l 0,1.28125 c 0.637999,-0.022 1.684751,-0.06925 2.71875,-0.53125 l 0,11.3125 -2.59375,0 0,1.28125 6.875,0 0,-1.28125 -2.59375,0 0,-13.5 -0.40625,0 z")
	(seven "m 0,0 0.40625,0 c 0.132,-0.4181 0.4605,-1.5508 0.8125,-1.9688 0.154,-0.176 1.78225,-0.1875 2.15625,-0.1875 l 4.84375,0 L 5.15625,1.5 c -2.023998,2.4419 -3.7035,5.7117 -4.1875,8.5937 -0.044,0.264 -0.166999,1.0311 0.625,1.0313 0.791999,0 0.92475,-0.7453 0.96875,-1.0313 L 2.75,9.0624 c 0.593999,-3.6077 1.599001,-5.9818 2.875,-7.5 l 3.78125,-4.5 0.09375,-0.4687 -5.1875,0 c -2.573997,0 -2.57175,-0.3008 -2.59375,-0.7188 l -0.375,0 z")
	(zero "m 9.2620001,0.8382 c 0.3079997,-1.76 0.4839995,-3.52 -0.022,-5.148 -0.6599994,-2.112 -2.398001,-2.464 -3.322,-2.464 -1.3199987,0 -3.0140013,0.572 -4.268,2.618 C 0.70400102,-2.6378 0.30799976,-0.9218 7.0571899e-8,0.8382 -0.26399967,2.4882 -0.50599931,4.4682 0.11000007,6.1402 c 0.65999934,1.782 2.17800103,2.222 3.25600003,2.222 1.1879988,0 2.9480013,-0.462 4.268,-2.552 0.945999,-1.518 1.3420003,-3.234 1.628,-4.972 m -1.782,-0.264 c -0.2639998,1.65 -0.5060005,3.146 -0.99,4.554 -0.6819993,2.09 -2.046001,2.75 -3.036,2.75 -0.8579992,0 -2.0680001,-0.55 -2.112,-2.662 -0.022,-1.32 0.3300002,-3.344 0.528,-4.642 0.2419997,-1.408 0.4840003,-2.86 0.858,-4.048 0.8579991,-2.618 2.5300005,-2.816 3.08,-2.816 0.7259992,0 2.112,0.396 2.178,2.574 0.022,1.232 -0.2640003,2.904 -0.506,4.29"))
    `(defun draw-one-fun ()
       ,@(svg-to-immediate-opengl (accumulate-relative-coordinates 
				   (expand-all-relative-bezier-into-lines 
				    (svg-path-d-to-lisp (split-at-comma-space zero))
				    :n 4))))))

(draw-one)


(defmacro def-number-fun ()
  `(progn
     ,@(loop for i from 0 below 10 collect
	    (let ((path-data (first (cdr (assoc i *numerals*)))))
	      `(defun ,(intern (format nil "DRAW-~a" i)) ()
		 ,@(svg-to-immediate-opengl
		    (accumulate-relative-coordinates 
		     (expand-all-relative-bezier-into-lines 
		      (svg-path-d-to-lisp (split-at-comma-space path-data))
		      :n 4))))))
     (defun draw-digit (c)
       (ecase c
	 ,@(loop for i from 0 below 10 collect
		`(,i (,(intern (format nil "DRAW-~a" i)))))))))
(def-number-fun)

(defparameter *sync* 3)
(defparameter *get-sync* 0)

(defun draw-centered-char (i)
 (with-pushed-matrix 
   (translate (* -11 i) 0 0)
   (with-primitive :line-loop
     (draw-digit i))))

(defun draw-number (val)
  (declare ((integer 0) val))
  (let ((s (format nil "~a" val)))
    (with-pushed-matrix
      (dotimes (i (length s))
	(translate 11 0 0)
	(draw-centered-char (- (char-code (char s i))
			       (char-code #\0)))))))


(sb-alien:define-alien-routine ("glXGetCurrentDisplay" glx-get-current-display)
    sb-alien:long)
(sb-alien:define-alien-routine ("glXGetCurrentDrawable" glx-get-current-drawable)
    sb-alien:int)
#+nil(sb-alien:define-alien-routine ("glXSwapIntervalEXT" glx-swap-interval-ext)
    sb-alien:int
  (dpy sb-alien:long)
  (drawable sb-alien:int)
  (interval sb-alien:int))

(sb-alien:define-alien-routine ("glXWaitVideoSyncSGI" glx-wait-video-sync-sgi)
    sb-alien:int
  (divisor sb-alien:int)
  (remainder sb-alien:int)
  (count sb-alien:unsigned-int :out))

(sb-alien:define-alien-routine ("glXGetVideoSyncSGI" glx-get-video-sync-sgi)
    sb-alien:int
  (count sb-alien:unsigned-int :out))

(sb-alien:define-alien-routine ("glXSwapIntervalSGI" glx-swap-interval-sgi)
    sb-alien:int
  (count sb-alien:int))

(sb-alien:load-shared-object "/usr/lib/xorg/modules/linux/libfglrxdrm.so")
(sb-alien:define-alien-routine ("firegl_WaitVBlank" firegl-wait-vblank)
    sb-alien:int)


(defparameter *blub* nil)
(let ((phi 0s0)
      (count 0))s

 (defun draw ()
   
   #+nil   (unless *blub*
	     (let ((dpy (glx-get-current-display))
		   (win (glx-get-current-drawable)))
	       (setf *blub* (list
			     dpy
			     win
			     (glx-swap-interval-ext dpy win 1)))))
   
   (line-width 2)
   (incf phi (/ (* 2 pi) 70))
 #+nil  (with-primitive :lines
     (color 1 0 0) (vertex 0 0 0) (vertex 1 0 0)
     (color 0 1 0) (vertex 0 0 0) (vertex 0 1 0)
     (color 0 0 1) (vertex 0 0 0) (vertex 0 0 1))
   (color 1 1 1)
   #+nil (with-pushed-matrix
     (let ((s .05))
       (scale s (- s) s))
     
     (with-primitives :line-loop
      (draw-one-fun)))
   (with-pushed-matrix
     (let ((x .1))
      (translate (* (- 1s0 x) (cos phi)) 0 0)
      (color 1 1 1)
      (rect (- x) -1 x 1)))
   (enable :line-smooth :blend)
   (hint :line-smooth-hint :nicest)
   (blend-func :src-alpha :one-minus-src-alpha )
   (with-pushed-matrix
     (let ((s .8))
       (scale s s s))
     (scale .02 -.02 .02)
     (translate -60 -1040 0)
     ;(enable :color-logic-op)
     ;(logic-op :xor)
     (draw-number (incf count))
     (translate 0 -24 0)
     (draw-number (floor (* 100 (get-frame-rate))))
     ;(disable :color-logic-op)
     )
  (sleep .014s0)
 #+nil (flush)
  #+nil (setf *blub* (firegl-wait-vblank))
 #+nil (let ((sync (glx-get-video-sync-sgi)))
    (glx-wait-video-sync-sgi 2 (mod (1+ sync) 2)))
   (swap-buffers)
  
  
#+nil  (finish)
#+nil
  (unless *blub*
    (setf *blub* (multiple-value-list 
		  #+nil (glx-get-video-sync-sgi)
		  (glx-wait-video-sync-sgi 2 0))))
 #+nil  (setf *get-sync*
    (glx-get-video-sync-sgi))
 #+nil  (unless (< *sync* 0)
     (let ((ret (glx-swap-interval-sgi *sync*)))
      (unless (= 0 ret)
	(break "error setting swap interval ~a." ret)))
     (setf *sync* -1))))
#+nil
(get-frame-rate)

#+nil
(with-gui (700 700) 30
  (draw))




