(progn
  (ql:quickload "cl-opengl")
  (ql:quickload "cl-glut"))
(defpackage :bla
  (:shadowing-import-from :cl close get special)
  (:use :cl :gl :glut))
(in-package :bla)

;; use amdcccle for configuration of ATI graphics card

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
       (count-max 30)
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
    
  (swap-buffers)
;  (flush)
  
;  (finish)
 ; (sleep (/ 1 68))
  (measure-frame-rate)
  
  (post-redisplay))


(sb-alien:define-alien-routine ("glXGetVideoSyncSGI" glx-get-video-sync-sgi)
    sb-alien:int
  (count sb-alien:int :out))

(sb-alien:define-alien-routine ("glXSwapIntervalSGI" glx-swap-interval-sgi)
    sb-alien:int
  (count sb-alien:int))



(defmethod keyboard ((w fenster) key x y)
  (case key
    (#\Esc (destroy-current-window))))

(defmacro with-gui ((w &optional (h w) (x 0) (y 0)) &body body)
  `(display-window 
    (make-instance 'bla::fenster
                   :mode '(:double :rgb :depth)
                   :width ,w :height ,h
                   :pos-x ,x :pos-y ,y 
                   :draw-func #'(lambda ()
                                  ,@body))))


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
    (r-cubic-bezier 6)
    (r-line-to 2)
    (end-loop 0)))

(declaim (optimize (debug 3)))


(defun svg-path-d-to-lisp (cmds)
  (let ((last-cmd nil)
	(res nil))
   (loop for i below (length cmds)
      do
	(let* ((current-cmd (cond ((string= "m" (elt cmds i)) 'r-translate)
				  ((string= "c" (elt cmds i)) 'r-cubic-bezier)
				  ((string= "l" (elt cmds i)) 'r-line-to)
				  ((string= "z" (elt cmds i)) 'end-loop)
				  (t  (decf i)
				      'repeat))))
	  (unless (eq current-cmd 'repeat)
	    (setf last-cmd current-cmd))
	  (let ((nr-args (first (cdr (assoc last-cmd *svg-commands*)))))
	    (push `(,last-cmd ,@(loop for j below nr-args collect 
				     (let ((arg (read-from-string (elt cmds (incf i)))))
				       #+NIL(unless (numberp arg)
					 (break "parse error, expected number got ~a." arg))
				       arg)))
		  res))))
   (reverse res)))


(defun expand-relative-bezier (args &key (n 10))
  (destructuring-bind (x1 y1 x2 y2 x3 y3) args
    (let* ((ox 0s0)
	   (oy 0s0))#+nil ((x2 (+ x1 xx2))
	   (y2 (+ y1 yy2))
	   (x3 (+ x2 xx3))
	   (y3 (+ y2 yy3))
	   
	   )
     (loop for i below n collect
	  (let* ((u (/ (* 1s0 i) n))
		 (v (- 1s0 u))
		 (uu (* u u))
		 (vv (* v v))
		 (x (+ #+nil (* uu u x0) (* 3 uu v x1) (* 3 u vv x2) (* uu u x3)))
		 (y (+ #+nil (* uu u y0) (* 3 uu v y1) (* 3 u vv y2) (* uu u y3))))
	    (prog1
	     `(r-line-to ,(- x ox)
			 ,(- y oy))
	      (setf ox x
		    oy y)))))))

#+nil
(expand-relative-bezier '(-1.1 1.2 -2.5 1.3 -4 1.4))

(defun expand-all-relative-bezier-into-lines (ls)
  (let ((res nil))
    (dolist (e ls)
     (destructuring-bind (cmd &rest rest) e
       (cond ((eq cmd 'r-cubic-bezier)
	      (let ((lines (expand-relative-bezier rest :n 13)))
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
	     ((eq cmd 'r-translate)
	      (destructuring-bind (xx yy) rest
		(incf x xx)
		(incf y yy)
		(push `(translate ,x ,y) res)))
	     ((eq cmd 'end-loop))
	     (t (break "unexpected command ~a" cmd)))))
    (reverse res)))

(defun svg-to-immediate-opengl (ls)
  (let ((res))
    (dolist (e ls)
      (destructuring-bind (cmd x y) e
	(cond ((eq cmd 'line-to)
	       (push `(vertex ,x ,y) res))
	      ((eq cmd 'translate))
	      (t (break "unexpected command ~a" cmd)))))
    (reverse res)))


(defmacro draw-one ()
  (let ((one "m 0.125,.875 c -1.187999,1.231999 -2.592001,1.3935 -4,1.4375 l 0,1.28125 c 0.637999,-0.022 1.684751,-0.06925 2.71875,-0.53125 l 0,11.3125 -2.59375,0 0,1.28125 6.875,0 0,-1.28125 -2.59375,0 0,-13.5 -0.40625,0 z"))
    `(progn ,@(svg-to-immediate-opengl (accumulate-relative-coordinates 
				(expand-all-relative-bezier-into-lines 
				 (svg-path-d-to-lisp (split-at-comma-space one))))))))

(defparameter *sync* 3)
(defparameter *get-sync* 0)
(let ((phi 0s0))
 (defun draw ()
   (incf phi (/ (* 2 pi) 60))
   (with-primitive :lines
     (color 1 0 0) (vertex 0 0 0) (vertex 1 0 0)
     (color 0 1 0) (vertex 0 0 0) (vertex 0 1 0)
     (color 0 0 1) (vertex 0 0 0) (vertex 0 0 1))
   (color 1 1 1)
   (with-pushed-matrix
     (scale .03 .03 .03)
     (with-primitive :line-loop
       (draw-one)))
   (translate (* .9 (cos phi)) 0 0)
   (color 1 1 1)
   (rect -.1 -1 .1 1)
   (setf *get-sync*
    (glx-get-video-sync-sgi))
   (unless (< *sync* 0)
     (let ((ret (glx-swap-interval-sgi *sync*)))
      (unless (= 0 ret)
	(break "error setting swap interval ~a." ret)))
     (setf *sync* -1))))

(get-frame-rate)

#+nil
(with-gui (600 630 600 30)
  (draw))




