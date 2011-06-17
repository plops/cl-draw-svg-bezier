(progn
  (ql:quickload "cl-opengl")
  (ql:quickload "cl-glut"))
(defpackage :bla
  (:shadowing-import-from :cl close get special)
  (:use :cl :gl :glut))
(in-package :bla)

(defun build-var (classname var)
  (list var 
        :initform nil
        :accessor (intern (concatenate 'string (string classname) "-" 
                                       (string var)))
        :initarg (intern (string var) :keyword)))

(defun build-varlist (classname varlist)
   (loop for var in varlist 
         collect (build-var classname var)))


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
  ;(flush)
  
  ;(finish)
  (measure-frame-rate)
  
  (post-redisplay))

#+nil
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

(defparameter *sync* 9)
(let ((phi 0s0))
 (defun draw ()
   (incf phi (/ (* 2 pi) 66))
   (with-primitive :lines
     (color 1 0 0) (vertex 0 0 0) (vertex 1 0 0)
     (color 0 1 0) (vertex 0 0 0) (vertex 0 1 0)
     (color 0 0 1) (vertex 0 0 0) (vertex 0 0 1))
   (translate (* .9 (cos phi)) 0 0)
   (color 1 1 1)
   (rect -.1 -1 .1 1)
   (unless (< *sync* 0)
     (unless (= 0
		(glx-swap-interval-sgi *sync*))
       (break "error setting swap interval."))
     (setf *sync* -1))))

(get-frame-rate)

(with-gui (600 630 600 30)
  (draw))