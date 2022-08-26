(defpackage #:point-origin%
  (:use :cl)
  (:shadow cl:+
           cl:-
           cl:*
           cl:/
           cl:random
           cl:min
           cl:max
           cl:type)
  (:export #:new
           #:dimensions
           #:copy
           #:element-type
           #:type

           #:==
           #:x
           #:y
           #:z
           #:w
                     
           #:->list
           #:->vector
           #:->values

           ;; Protocol parts that would be nice to have for new implementations

           #:+
           #:-
           #:*
           #:/

           #:magnitude
           #:dot
           #:cross
           #:normalize

           ;; Should work with anything that provides the basics.

           #:random
           #:transform
           #:ntransform
                     
           #:min
           #:max
           #:nmin
           #:nmax
                     
           #:distance
           #:euclidean-sqrt 
           #:euclidean
           #:manhattan
           #:minkowski      
           #:of-division

           #:angle
                     
           #:angle-cosine
           #:angle-sine
           #:angle-acos
           #:z-alignment-angles
           #:angle-2d
                     
           #:lerp
           #:negate
           #:jitter
                     
           #:in-polygon?
           #:bounds
           #:sphericize))

(in-package #:point-origin%)
(declaim (inline  #:new
                  #:dimensions
                  #:copy
                  #:element-type
                  #:type

                  #:==
                  #:x
                  #:y
                  #:z
                  #:w
                     
                  #:->list
                  #:->vector
                  #:->values

                  ;; Protocol parts that would be nice to have for new implementations

                  #:+
                  #:-
                  #:*
                  #:/

                  #:magnitude
                  #:dot
                  #:cross
                  #:normalize

                  ;; Should work with anything that provides the basics.

                  #:random
                  #:transform
                  #:ntransform
                     
                  #:min
                  #:max
                  #:nmin
                  #:nmax
                     
                  #:distance
                  #:euclidean-sqrt 
                  #:euclidean
                  #:manhattan
                  #:minkowski      
                  #:of-division

                  #:angle
                     
                  #:angle-cosine
                  #:angle-sine
                  #:angle-acos
                  #:z-alignment-angles
                  #:angle-2d
                     
                  #:lerp
                  #:negate
                  #:jitter
                     
                  #:in-polygon?
                  #:bounds
                  #:sphericize))
(eval-when (:compile-toplevel :load-toplevel :execute)
    (defun split-dot (symbol)
   (destructuring-bind (sym fn) (split-sequence:split-sequence #\. (symbol-name symbol))
     (list (intern fn) (intern sym))))

    (defun walk (tree test transform)
      (flet ((transform? (item test transform)
               (if (funcall test item)
                   (funcall transform item)
                   item)))
        (cond
          ((null tree) (values))
          ((atom tree) (case tree
                         (+ 'cl:+)
                         (- 'cl:-)
                         (/ 'cl:/)
                         (* 'cl:*)
                         (max 'cl:max)
                         (min 'cl:min)
                         (t (transform? tree test transform))))
          ((consp tree) (cons (walk (car tree) test transform) (walk (cdr tree) test transform))))))

    (defmacro with-dot (&body body)
      `(let (dummy)
         (declare (ignore dummy))
         ,@(walk body (lambda (x) (and (symbolp x) (position #\. (symbol-name x)) (not (eql x '/.)))) #'split-dot))))

(defun new (type x y &optional z w)
  (declare (ignorable z w))
  (case type  ;;why only 2d/3d single float constructors???
    (:ovec2 (origin.geometry.point2d:point (coerce x 'single-float) (coerce y 'single-float)))
    (:ovec3 (origin.geometry.point3d:point (coerce x 'single-float) (coerce y 'single-float) (coerce z 'single-float)))
    (:ovec4 (origin.vec4::%vec (coerce x 'single-float) (coerce y 'single-float)(coerce z 'single-float)(coerce w 'single-float)))
    (:odvec2 (origin.dvec2::%vec (coerce x 'double-float) (coerce y 'double-float)))
    (:odvec3 (origin.dvec3::%vec (coerce x 'double-float) (coerce y 'double-float) (coerce z 'double-float)))
    (:odvec4 (origin.dvec4::%vec (coerce x 'double-float) (coerce y 'double-float)(coerce z 'double-float)(coerce w 'double-float)))))



(defun dimensions (p)
  (typecase p
    ((simple-array single-float (2))2)
    ((simple-array single-float (3))3)
    ((simple-array single-float (4))4)
    ((simple-array double-float (2))2)
    ((simple-array double-float (3))3)
    ((simple-array double-float (4))4)))
(defun copy (p)
  (typecase p
    ((simple-array single-float (2)) (origin.vec2:copy p))
    ((simple-array single-float (3)) (origin.vec3:copy p))
    ((simple-array single-float (4)) (origin.vec4:copy p))
    ((simple-array double-float (2)) (origin.dvec2:copy p))
    ((simple-array double-float (3)) (origin.dvec3:copy p))
    ((simple-array double-float (4)) (origin.dvec4:copy p))))
(defun element-type (p)
  (typecase p
    ((simple-array single-float (2)) 'single-float)
    ((simple-array single-float (3)) 'single-float)
    ((simple-array single-float (4)) 'single-float)
    ((simple-array double-float (2)) 'double-float)
    ((simple-array double-float (3)) 'double-float)
    ((simple-array double-float (4)) 'double-float)))

(defun type (p)
  (typecase p
    ((simple-array single-float (2)) :ovec2)
    ((simple-array single-float (3)) :ovec3)
    ((simple-array single-float (4)) :ovec4)
    ((simple-array double-float (2)) :odvec2)
    ((simple-array double-float (3)) :odvec3)
    ((simple-array double-float (4)) :odvec4)))

(defun == (p1 p2 &optional epsilon)
  (typecase p1
    ((simple-array single-float (2)) (origin.vec2:= p1 p2))
    ((simple-array single-float (3)) (origin.vec3:= p1 p2))
    ((simple-array single-float (4)) (origin.vec4:= p1 p2))
    ((simple-array double-float (2)) (origin.dvec2:= p1 p2))
    ((simple-array double-float (3)) (origin.dvec3:= p1 p2))
    ((simple-array double-float (4)) (origin.dvec4:= p1 p2))))

(defun x (p)
  (typecase p
    ((simple-array single-float (2)) (origin.vec2:x p))
    ((simple-array single-float (3)) (origin.vec3:x p))
    ((simple-array single-float (4)) (origin.vec4:x p))
    ((simple-array double-float (2)) (origin.dvec2:x p))
    ((simple-array double-float (3)) (origin.dvec3:x p))
    ((simple-array double-float (4)) (origin.dvec4:x p))))
(defun y (p)
  (typecase p
    ((simple-array single-float (2)) (origin.vec2:y P))
    ((simple-array single-float (3)) (origin.vec3:y P))
    ((simple-array single-float (4)) (origin.vec4:y P))
    ((simple-array double-float (2)) (origin.dvec2:y P))
    ((simple-array double-float (3)) (origin.dvec3:y P))
    ((simple-array double-float (4)) (origin.dvec4:y P))))
(defun z (p)
  (declare (ignorable p))
  (typecase p
    ((simple-array single-float (2)) (coerce 0 'float))
    ((simple-array single-float (3)) (origin.vec3:z P))
    ((simple-array single-float (4)) (origin.vec4:z P))
    ((simple-array double-float (2)) (coerce 0 'float))
    ((simple-array double-float (3)) (origin.dvec3:z P))
    ((simple-array double-float (4)) (origin.dvec4:z P))))
(defun w (p)
  (declare (ignorable p))
  (typecase p
    ((simple-array single-float (2)) (coerce 0 'float))
    ((simple-array single-float (3)) (coerce 0 'float))
    ((simple-array single-float (4)) (origin.vec4:w p))
    ((simple-array double-float (2)) (coerce 0 'float))
    ((simple-array double-float (3)) (coerce 0 'float))
    ((simple-array double-float (4)) (origin.dvec4:w p))))

(defun (setf x) (value p)
  (typecase p
    ((simple-array single-float (2)) (setf (origin.vec2:x p) value))
    ((simple-array single-float (3)) (setf (origin.vec3:x p) value))
    ((simple-array single-float (4)) (setf (origin.vec4:x p) value))
    ((simple-array double-float (2)) (setf (origin.dvec2:x p) value))
    ((simple-array double-float (3)) (setf (origin.dvec3:x p) value))
    ((simple-array double-float (4)) (setf (origin.dvec4:x p) value))))
(defun (setf y) (value p)
  (typecase p
    ((simple-array single-float (2)) (setf (origin.vec2:y P) value))
    ((simple-array single-float (3)) (setf (origin.vec3:y P) value))
    ((simple-array single-float (4)) (setf (origin.vec4:y P) value))
    ((simple-array double-float (2)) (setf (origin.dvec2:y P) value))
    ((simple-array double-float (3)) (setf (origin.dvec3:y P) value))
    ((simple-array double-float (4)) (setf (origin.dvec4:y P) value))))
(defun (setf z) (value p)
  (declare (ignorable p))
  (typecase p
    ((simple-array single-float (2)) (coerce 0 'float))
    ((simple-array single-float (3)) (setf (origin.vec3:z P) value))
    ((simple-array single-float (4)) (setf (origin.vec4:z P) value))
    ((simple-array double-float (2)) (coerce 0 'float))
    ((simple-array double-float (3)) (setf (origin.dvec3:z P) value))
    ((simple-array double-float (4)) (setf (origin.dvec4:z P) value))))
(defun (setf w) (value p)
  (declare (ignorable p))
  (typecase p
    ((simple-array single-float (2)) (coerce 0 'float))
    ((simple-array single-float (3)) (coerce 0 'float))
    ((simple-array single-float (4)) (setf (origin.vec4:w p) value))
    ((simple-array double-float (2)) (coerce 0 'float))
    ((simple-array double-float (3)) (coerce 0 'float))
    ((simple-array double-float (4)) (setf (origin.dvec4:w p) value))))


(defun ->list (p)
  (coerce p 'list))
(defun ->vector (p) p)
(defun ->values (p)
  (typecase p
    ((simple-array single-float (2)) (origin.vec2:with-components ((p p)) (values px py)))
    ((simple-array single-float (3)) (origin.vec3:with-components ((p p)) (values px py pz)))
    ((simple-array single-float (4)) (origin.vec4:with-components ((p p)) (values px py pz pw)))
    ((simple-array double-float (2)) (origin.dvec2:with-components ((p p)) (values px py)))
    ((simple-array double-float (3)) (origin.dvec3:with-components ((p p)) (values px py pz)))
    ((simple-array double-float (4)) (origin.dvec4:with-components ((p p)) (values px py pz pw)))))


(defun + (p1 p2)
  (typecase p1
    ((simple-array single-float (2)) (origin.vec2:+ p1 p2))
    ((simple-array single-float (3)) (origin.vec3:+ p1 p2))
    ((simple-array single-float (4)) (origin.vec4:+ p1 p2))
    ((simple-array double-float (2)) (origin.dvec2:+ p1 p2))
    ((simple-array double-float (3)) (origin.dvec3:+ p1 p2))
    ((simple-array double-float (4)) (origin.dvec4:+ p1 p2))))
(defun - (p1 p2)
  (typecase p1
    ((simple-array single-float (2)) (origin.vec2:- p1 p2))
    ((simple-array single-float (3)) (origin.vec3:- p1 p2))
    ((simple-array single-float (4)) (origin.vec4:- p1 p2))
    ((simple-array double-float (2)) (origin.dvec2:- p1 p2))
    ((simple-array double-float (3)) (origin.dvec3:- p1 p2))
    ((simple-array double-float (4)) (origin.dvec4:- p1 p2))))
(defun * (p1 p2)
  (typecase p1
    ((simple-array single-float (2)) (origin.vec2:* p1 p2))
    ((simple-array single-float (3)) (origin.vec3:* p1 p2))
    ((simple-array single-float (4)) (origin.vec4:* p1 p2))
    ((simple-array double-float (2)) (origin.dvec2:* p1 p2))
    ((simple-array double-float (3)) (origin.dvec3:* p1 p2))
    ((simple-array double-float (4)) (origin.dvec4:* p1 p2))))
(defun / (p1 p2)
  (typecase p1
    ((simple-array single-float (2)) (origin.vec2:/ p1 p2))
    ((simple-array single-float (3)) (origin.vec3:/ p1 p2))
    ((simple-array single-float (4)) (origin.vec4:/ p1 p2))
    ((simple-array double-float (2)) (origin.dvec2:/ p1 p2))
    ((simple-array double-float (3)) (origin.dvec3:/ p1 p2))
    ((simple-array double-float (4)) (origin.dvec4:/ p1 p2))))


(defun dot (p1 p2)
  (typecase p1
    ((simple-array single-float (2)) (origin.vec2:dot p1 p2))
    ((simple-array single-float (3)) (origin.vec3:dot p1 p2))
    ((simple-array single-float (4)) (origin.vec4:dot p1 p2))
    ((simple-array double-float (2)) (origin.dvec2:dot p1 p2))
    ((simple-array double-float (3)) (origin.dvec3:dot p1 p2))
    ((simple-array double-float (4)) (origin.dvec4:dot p1 p2))))

(defun magnitude (p)
  (sqrt (dot p p)))
(defun cross (a b)
  (typecase a
    ((simple-array single-float (3)) (origin.vec3:cross a b))
    ((simple-array double-float (3)) (origin.dvec3:cross a b))
    (array (with-dot (origin.geometry.point3d:point  (- (* a.y b.z) (* a.z b.y))
                                                     (- (* a.z b.x) (* a.x b.z))
                                                     (- (* a.x b.y) (* a.y b.x)))))))

(defun normalize (p)
  (typecase p
    ((simple-array single-float (2)) (origin.vec2:normalize p))
    ((simple-array single-float (3)) (origin.vec3:normalize p))
    ((simple-array single-float (4)) (origin.vec4:normalize p))
    ((simple-array double-float (2)) (origin.dvec2:normalize p))
    ((simple-array double-float (3)) (origin.dvec3:normalize p))
    ((simple-array double-float (4)) (origin.dvec4:normalize p))))

;(defun random (p))
(defun transform (p transform)
  (let ((result (copy p)))
    (with-dot
      (setf result.x (funcall transform p.x)
            result.y (funcall transform p.y)
            result.z (funcall transform p.z)))))
(defun ntransform (p transform)
  (with-dot
    (setf p.x (funcall transform p.x)
          p.y (funcall transform p.y)
          p.z (funcall transform p.z))))

(defun min (p1 p2)
  (typecase p1
    ((simple-array single-float (2)) (origin.vec2:min p1 p2))
    ((simple-array single-float (3)) (origin.vec3:min p1 p2))
    ((simple-array single-float (4)) (origin.vec4:min p1 p2))
    ((simple-array double-float (2)) (origin.dvec2:min p1 p2))
    ((simple-array double-float (3)) (origin.dvec3:min p1 p2))
    ((simple-array double-float (4)) (origin.dvec4:min p1 p2))))
(defun max (p1 p2)
  (typecase p1
    ((simple-array single-float (2)) (origin.vec2:max p1 p2))
    ((simple-array single-float (3)) (origin.vec3:max p1 p2))
    ((simple-array single-float (4)) (origin.vec4:max p1 p2))
    ((simple-array double-float (2)) (origin.dvec2:max p1 p2))
    ((simple-array double-float (3)) (origin.dvec3:max p1 p2))
    ((simple-array double-float (4)) (origin.dvec4:max p1 p2))))
(defun nmin (p1 p2)
  (typecase p1
    ((simple-array single-float (2)) (origin.vec2:min! p1 p1 p2))
    ((simple-array single-float (3)) (origin.vec3:min! p1 p1 p2))
    ((simple-array single-float (4)) (origin.vec4:min! p1 p1 p2))
    ((simple-array double-float (2)) (origin.dvec2:min! p1 p1 p2))
    ((simple-array double-float (3)) (origin.dvec3:min! p1 p1 p2))
    ((simple-array double-float (4)) (origin.dvec4:min! p1 p1 p2))))
(defun nmax (p1 p2)
  (typecase p1
    ((simple-array single-float (2)) (origin.vec2:max! p1 p1 p2))
    ((simple-array single-float (3)) (origin.vec3:max! p1 p1 p2))
    ((simple-array single-float (4)) (origin.vec4:max! p1 p1 p2))
    ((simple-array double-float (2)) (origin.dvec2:max! p1 p1 p2))
    ((simple-array double-float (3)) (origin.dvec3:max! p1 p1 p2))
    ((simple-array double-float (4)) (origin.dvec4:max! p1 p1 p2))))
(defun average (&rest points)
  (let ((div (cl:/ 1.0 (length points))))
    (* (reduce #'+ points) (new (type (car points))div div div div))))

(defun distance (p1 p2 &optional h)
   (minkowski p1 p2 h))
(defun euclidean-sqrt(p1 p2)
  (with-dot (reduce #'+ (mapcar (lambda (i j) (expt (- j i)2)) (->list p1)(->list p2)))))
(defun euclidean(p1 p2)
  (sqrt (euclidean-sqrt p1 p2)))
(defun manhattan(p1 p2)
  (reduce #'cl:+ (map 'list #'abs (->list (- p2 p1)))))
(defun minkowski(p1 p2 h)
  (with-dot (expt (reduce #'+ (mapcar (lambda (i j) (expt (- j i) h)) (->list p1)(->list p2))) (/ h))))
(defun of-division (p1 p2 &optional (divisor 0.5))
  (* (+ p1 p2) divisor))

(defun angle (p1 p2 &optional degrees)
  (let* ((mags  (cl:* (magnitude p1)(magnitude p2)))
         (dot (dot p1 p2))
         (result 
           (if (zerop mags)
               (error "I'm too dumb to handle 0 magnitude points.")
               (acos (cl:/ dot mags)))))
    (if degrees
        (cl:* result (cl:/ 180 pi))
        result)))

(defun angle-cosine (p1 &optional p2)
  (if p2
      (dot (normalize p1) (normalize p2))
      (let ((norm (normalize p1)))
        (dot norm norm))))
(defun angle-sine (p1 &optional p2)
  (let ((c (angle-cosine p1 p2)))
    (sqrt (cl:- 1 (cl:* c c)))))
;; (defun angle-acos (p1 &optional (p2 p1))
;;   (acos (max -1.0 (min 1.0 (dot (normalize p1) (normalize p2))))))

(defun z-alignment-angles (p)
  (with-dot
    (let* ((x p.x)
           (z p.z)
           (x-angle (- (/ pi 2) (angle-acos p (new (type p) 0 0 0 0))))
           (y-angle (if (and (= x 0) (= z 0))
                        0.0
                        (acos (/ z (sqrt (+ (* x x) (* z z))))))))
      (values y-angle x-angle))))
(defun angle-2d (p1 &optional p2)
  (with-dot
    (let* ((theta1 (atan p1.y p1.x))
	   (theta2 (atan p2.y p2.x))
	   (dtheta (- theta2 theta1)))
      (loop while (> dtheta pi)
	    do (setf dtheta (- dtheta (* 2 pi))))
      (loop while (< dtheta (- pi))
	    do (setf dtheta (+ dtheta (* 2 pi))))
      dtheta)))

;(defun lerp (p1 p2 tee))
(defun negate(p)
  (* p -1))


;; (defun jitter(p amount)
;;   (if (= 2 (dimensions p))
;;       (+ p (random (type p) 1 amount amount))
;;       (+ p (random (type p) 1 amount amount amount))))

(defun in-polygon? (p points)
  (let ((angle 0.0))
    (loop :for (p1 p2) :on (append points (list (first points))) :by #'cdr :while p2
	  :do (setf angle (cl:+ angle (angle-2d (- p1 p) (- p2 p)))))
    (if (< (abs angle) pi)
	nil
	t)))
(defun bounds(points)
  (loop :with low := (copy (car points))
        :with high := (copy (car points))
        :for p :in points
        :do (nmin low p) (nmax high p)
        :finally (return (values low high))))
(defun sphericize (p radius &optional (factor 1.0) center )
  (lerp p (* (normalize (- p center)) radius) factor))


