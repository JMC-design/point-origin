(in-package #:point-)

(defmethod point:dimensions ((p vector)) (point-origin%:dimensions p))
(defmethod point:copy ((p vector)) (point-origin%:copy p))
(defmethod point:element-type ((p vector))(point-origin%:element-type p))
(defmethod point:type ((p vector))(point-origin%:type p))

(defmethod point:== ((p1 vector) (p2 vector)&optional epsilon) (point-origin%:== p1 p2 epsilon))

(defmethod point:x ((p vector))(point-origin%:x p))
(defmethod point:y ((p vector))(point-origin%:y p))
(defmethod point:z ((p vector))(point-origin%:z p))
(defmethod point:w ((p vector))(point-origin%:w p))

(defmethod (setf point:x) (value p) (setf (point-origin%:x p) value))
(defmethod (setf point:y) (value p) (setf (point-origin%:y p) value))
(defmethod (setf point:z) (value p) (setf (point-origin%:z p) value))
(defmethod (setf point:w) (value p) (setf (point-origin%:w p) value))


(defmethod point:->list   ((p vector))  (point-origin%:->list p))
(defmethod point:->values ((p vector))  (point-origin%:->values p))
(defmethod point:->vector ((p vector))   p)

(defmethod point:+ ((p1 vector) (p2 vector))(point-origin%:+ p1 p2))
(defmethod point:* ((p1 vector) (p2 vector))(point-origin%:* p1 p2))
(defmethod point:/ ((p1 vector) (p2 vector))(point-origin%:/ p1 p2))
(defmethod point:- ((p1 vector) (p2 vector))(point-origin%:- p1 p2))


(defmethod point:magnitude ((p vector)) (point-origin%:magnitude p))
(defmethod point:dot ((p1 vector) (p2 vector))   (point-origin%:dot p1 p2))
(defmethod point:cross ((p1 vector) (p2 vector)) (point-origin%:cross p1 p2))
(defmethod point:normalize ((p vector)) (point-origin%:normalize p))

;;(defmethod point:random ((p vector))        (point-origin%:))
;;(defmethod point:transform ((p vector) fn)  (point-origin%:transform p fn))
;;(defmethod point:ntransform ((p vector) fn) (point-origin%:ntransform p fn))

(defmethod point:min ((p1 vector) (p2 vector))  (point-origin%:min p1 p2))
(defmethod point:max ((p1 vector) (p2 vector))  (point-origin%:max p1 p2))
(defmethod point:nmin ((p1 vector) (p2 vector)) (point-origin%:nmin p1 p2))
(defmethod point:nmax ((p1 vector) (p2 vector)) (point-origin%:nmax p1 p2))

(defmethod point:distance ((p1 vector) (p2 vector) &optional (h 2))        (point-origin%:distance p1 p2 h))
(defmethod point:euclidean-sqrt ((p1 vector) (p2 vector))                  (point-origin%:euclidean-sqrt p1 p2))
(defmethod point:euclidean ((p1 vector) (p2 vector))                       (point-origin%:euclidean p1 p2))
(defmethod point:manhattan ((p1 vector) (p2 vector))                       (point-origin%:manhattan p1 p2))
(defmethod point:minkowski ((p1 vector) (p2 vector) h)                     (point-origin%:minkowski p1 p2 h))
(defmethod point:of-division ((p1 vector) (p2 vector) &optional (tee 0.5)) (point-origin%:of-division p1 p2 tee))

(defmethod point:angle ((p1 vector)&optional (p2 p1) degrees)(point-origin%:angle p1 p2 degrees))
(defmethod point:angle-cosine ((p1 vector) &optional (p2 p1)) (point-origin%:angle-cosine p1 p2))
(defmethod point:angle-sine ((p1 vector) &optional (p2 p1))   (point-origin%:angle-sine p1 p2))
(defmethod point:angle-acos ((p1 vector) &optional (p2 p1))   (point-origin%:angle-acos p1 p2))
(defmethod point:z-alignment-angles ((p vector))              (point-origin%:z-alignment-angles p))
(defmethod point:angle-2d ((p1 vector)&optional (p2 p1))           (point-origin%:angle-2d p1 p2))

;(defmethod point:lerp ((p1 vector) (p2 vector) tee &optional transform) (point-origin%:lerp p1 p2 tee transform))
(defmethod point:negate ((p vector))                (point-origin%:negate p))

;; make methods?
;;(defmethod point:jitter ((p vector)amount)          (point-origin%:jitter p amount))
;; (defmethod point:in-polygon? ((p vector) points) (point-origin%:in-polygon? p points))
;; (defmethod point:bounds (points)                 (point-origin%:bounds points))
;; (defmethod point:sphericize ((p vector)radius &optional (factor 1.0) center) (point-origin%:sphericize p radius factor center))

