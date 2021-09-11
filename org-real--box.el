;;; org-real--box.el --- Keep track of real things as org-mode links -*- lexical-binding: t -*-

;; Author: Tyler Grinn <tylergrinn@gmail.com>
;; Version: 0.1.0
;; File: org-real--box.el
;; Package-Requires: ((emacs "26.1"))
;; Keywords: tools
;; URL: https://gitlab.com/tygrdev/org-real

;;; Commentary:

;; Box class definition and related methods

;;; Code:

;;;; Patch! 0.0.1 -> 0.1.0+
;;;; Will be removed in version 1.0.0+

(and (fboundp 'org-real--merge) (fmakunbound 'org-real--merge))
(and (fboundp 'org-real--map-immediate) (fmakunbound 'org-real--map-immediate))
(and (fboundp 'org-real--next) (fmakunbound 'org-real--next))
(and (fboundp 'org-real--merge-into) (fmakunbound 'org-real--merge-into))
(and (fboundp 'org-real--add-matching) (fmakunbound 'org-real--add-matching))
(and (fboundp 'org-real--flex-add) (fmakunbound 'org-real--flex-add))
(and (fboundp 'org-real--expand) (fmakunbound 'org-real--expand))
(and (fboundp 'org-real--draw) (fmakunbound 'org-real--draw))
(and (fboundp 'org-real--get-width) (fmakunbound 'org-real--get-width))
(and (fboundp 'org-real--get-height) (fmakunbound 'org-real--get-height))
(and (fboundp 'org-real--get-top) (fmakunbound 'org-real--get-top))
(and (fboundp 'org-real--get-left) (fmakunbound 'org-real--get-left))

;;;; Requirements:

(require 'eieio)
(require 'cl-lib)

;;;; Variables from org-real.el

(eval-when-compile
  (defvar org-real-padding)
  (defvar org-real-margin))

;;;; Class definitions

(defclass org-real--box-collection ()
  ((box :initarg :box
        :type org-real--box)
   (next :initarg :next
         :type org-real--box-collection))
  "A collection of `org-real--box'es.")

(defclass org-real--box ()
  ((name :initarg :name
         :type string)
   (rel :initarg :rel
        :type string)
   (rel-box :initarg :rel-box
            :type org-real--box)
   (x-order :initarg :x-order
            :initform 0
            :type number)
   (y-order :initarg :y-order
            :initform 0
            :type number)
   (in-front :initarg :in-front
             :initform nil
             :type boolean)
   (behind :initarg :behind
           :initform nil
           :type boolean)
   (parent :initarg :parent
           :type org-real--box)
   (children :initarg :children
             :initform (org-real--box-collection)
             :type org-real--box-collection)
   (primary :initarg :primary
            :initform nil
            :type boolean))
  "A representation of a box in 3D space.")


;;;; Exports

(cl-defmethod org-real--make-instance ((_ (subclass org-real--box)) containers)
  "Create an instance of `org-real--box' from CONTAINERS.

CONTAINERS is a list of plists containing at least a :name
property and optionally a :rel property."
  (when-let* ((world (org-real--box))
              (base-container (pop containers))
              (base (org-real--box :name (plist-get base-container :name))))
    (oset base :parent world)
    (with-slots (children) world
      (setq children (org-real--add-to-list children base)))
    (if containers
        (org-real--make-instance-helper containers world base))
    world))

(cl-defmethod org-real--merge (boxes)
  "Merge BOXES into a single box."
  (if (< (length boxes) 2)
      (if (= 0 (length boxes))
          (org-real--box)
        (car boxes))
    (let ((world (org-real--box)))
      (while boxes
        (org-real--merge-into (pop boxes) world))
      world)))

;;;; Drawing

(cl-defmethod org-real--draw ((box org-real--box) offset)
  "Insert an ascii drawing of BOX into the current buffer.

OFFSET is the starting line to start insertion."
  (let ((children (with-slots (children) box (org-real--get-all children))))
    (if (slot-boundp box :name)
        (with-slots (name behind (align-bottom in-front) (dashed behind) primary) box
          (let* ((top (+ offset (org-real--get-top box)))
                 (left (org-real--get-left box))
                 (width (org-real--get-width box))
                 (height (org-real--get-height box)))
            (cl-flet ((draw (coords str &optional primary)
                            (forward-line (- (car coords) (line-number-at-pos)))
                            (move-to-column (cdr coords) t)
                            (if primary
                                (put-text-property 0 (length str) 'face 'org-real-primary
                                                   str))
                            (insert str)
                            (delete-char (length str))))
              (draw (cons top left)
                    (concat "┌" (make-string (- width 2) (if dashed #x254c #x2500)) "┐"))
              (if align-bottom
                  (draw (cons (+ top height) left)
                        (concat "┴" (make-string (- width 2) (if dashed #x254c #x2500)) "┴"))
                (draw (cons (+ top height -1) left)
                      (concat "└" (make-string (- width 2) (if dashed #x254c #x2500)) "┘")))
              (draw (cons (+ top 1 (cdr org-real-padding))
                          (+ left 1 (car org-real-padding)))
                    name
                    primary)
              (let ((r (+ top 1))
                    (c1 left)
                    (c2 (+ left width -1)))
                (dotimes (_ (- height (if align-bottom 1 2)))
                  (draw (cons r c1) (if dashed "╎" "│"))
                  (draw (cons r c2) (if dashed "╎" "│"))
                  (setq r (+ r 1))))))))
    (mapc
     (lambda (child) (org-real--draw child offset))
     children)))

(cl-defmethod org-real--get-width ((box org-real--box))
  "Get the width of BOX."
  (let* ((base-width (+ 2 ; box walls
                        (* 2 (car org-real-padding))))
         (width (+ base-width
                   (if (slot-boundp box :name)
                       (with-slots (name) box (length name))
                     0)))
         (children (with-slots (children) box (org-real--get-all children))))
    (if (not children)
        width
      (let* ((column-indices (cl-delete-duplicates
                              (mapcar (lambda (child) (with-slots (x-order) child x-order)) children)))
             (columns (mapcar
                       (lambda (c)
                         (seq-filter
                          (lambda (child)
                            (with-slots (x-order) child
                              (= c x-order)))
                          children))
                       column-indices))
             (column-widths (mapcar
                             (lambda (column)
                               (apply 'max (mapcar 'org-real--get-width column)))
                             columns))
             (children-width (seq-reduce
                              (lambda (total width)
                                (+ total (car org-real-margin) width))
                              column-widths
                              (* -1 (car org-real-margin)))))
        (if (> width (+ (* 2 (car org-real-margin)) children-width))
            width
          (+ base-width children-width))))))

(cl-defmethod org-real--get-height ((box org-real--box))
  "Get the height of BOX."
  (let* ((in-front (with-slots (in-front) box in-front))
         (height (+ (if in-front
                        (* -1 (cdr org-real-margin))
                      0)
                    3 ; box walls + text
                    (cdr org-real-padding)
                    (cdr org-real-margin)))
         (children (with-slots (children) box (org-real--get-all children))))
    (if (not children)
        height
      (let* ((row-indices (cl-delete-duplicates
                           (mapcar (lambda (child) (with-slots (y-order) child y-order)) children)))
             (rows (mapcar
                    (lambda (r)
                      (seq-filter
                       (lambda (child)
                         (with-slots (y-order) child
                           (= r y-order)))
                       children))
                    row-indices))
             (row-heights (mapcar
                           (lambda (row)
                             (apply 'max (mapcar 'org-real--get-height row)))
                           rows)))
        (+ height (seq-reduce '+ row-heights 0))))))

(cl-defmethod org-real--get-top ((box org-real--box))
  "Get the top row index of BOX."
  (if (not (slot-boundp box :parent))
      0
    (with-slots (parent x-order y-order) box
      (let* ((offset (+ 2 (cdr org-real-padding) (cdr org-real-margin)))
             (top (+ offset (org-real--get-top parent)))
             (above (seq-filter
                     (lambda (child)
                       (with-slots ((child-x x-order) (child-y y-order)) child
                         (and (= x-order child-x)
                              (< child-y y-order))))
                     (org-real--get-all (with-slots (children) parent children))))
             (directly-above (and above (seq-reduce
                                         (lambda (max child)
                                           (with-slots ((max-y y-order)) max
                                             (with-slots ((child-y y-order)) child
                                               (if (> child-y max-y)
                                                   child
                                                 max))))
                                         above
                                         (org-real--box :y-order -9999))))
             (above-height (and directly-above (apply 'max
                                                      (mapcar
                                                       'org-real--get-height
                                                       (seq-filter
                                                        (lambda (child)
                                                          (= (with-slots (y-order) directly-above y-order)
                                                             (with-slots (y-order) child y-order)))
                                                        (org-real--get-all
                                                         (with-slots (children) parent children))))))))
        (if directly-above
            (+ (org-real--get-top directly-above)
               above-height)
          (with-slots (rel rel-box) box
            (if (and (slot-boundp box :rel)
                     (or (string= "to the left of" rel)
                         (string= "to the right of" rel)))
                (org-real--get-top rel-box)
              top)))))))

(cl-defmethod org-real--get-left ((box org-real--box))
  "Get the left column index of BOX."
  (if (not (slot-boundp box :parent))
      0
    (with-slots (parent x-order y-order) box
      (let* ((left (+ 1
                      (car org-real-padding)
                      (org-real--get-left parent)))
             (to-the-left (seq-filter
                           (lambda (child)
                             (with-slots ((child-y y-order) (child-x x-order)) child
                                 (and (= y-order child-y)
                                      (< child-x x-order))))
                           (org-real--get-all (with-slots (children) parent children))))
             (directly-left (and to-the-left
                                 (seq-reduce
                                  (lambda (max child)
                                    (with-slots ((max-x x-order)) max
                                      (with-slots ((child-x x-order)) child
                                        (if (> child-x max-x)
                                            child
                                          max))))
                                  to-the-left
                                  (org-real--box :x-order -9999)))))
        (if directly-left
            (+ (org-real--get-left directly-left)
               (org-real--get-width directly-left)
               (car org-real-margin))
          (with-slots (rel rel-box) box
            (if (and (slot-boundp box :rel)
                     (or (string= "above" rel)
                         (string= "below" rel)))
                (org-real--get-left rel-box)
              left)))))))

;;;; Utility expressions

(cl-defmethod org-real--get-all ((collection org-real--box-collection))
  "Get all boxes in COLLECTION as a sequence."
  (with-slots (box next) collection
    (append (if (slot-boundp collection :box) (list box))
            (if (slot-boundp collection :next) (org-real--get-all next)))))

(cl-defmethod org-real--add-to-list ((collection org-real--box-collection)
                                     (box org-real--box))
  "Add BOX to COLLECTION and return new COLLECTION."
  (if (slot-boundp collection :box)
      (org-real--box-collection
       :box box
       :next collection)
    (oset collection :box box)
    collection))

(cl-defmethod org-real--make-instance-helper (containers parent (prev org-real--box))
  "Help create a 3D representation of CONTAINERS.

PREV must already existing in PARENT."
  (let* ((container (pop containers))
         (rel (plist-get container :rel))
         (box (org-real--box :name (plist-get container :name))))
    (when prev
      (oset box :rel (plist-get container :rel))
      (oset box :rel-box prev)
      (with-slots
          ((cur-x x-order)
           (cur-y y-order)
           (cur-behind behind)
           (cur-in-front in-front))
          box
        (with-slots
            ((prev-x x-order)
             (prev-y y-order)
             (prev-behind behind)
             (prev-in-front in-front))
            prev
          (cond ((or (string= rel "in") (string= rel "on"))
                 (setq cur-x prev-x)
                 (setq cur-y prev-y)
                 (setq cur-behind prev-behind))
                ((string= rel "behind")
                 (setq cur-x prev-x)
                 (setq cur-y prev-y)
                 (setq cur-behind t))
                ((string= rel "in front of")
                 (setq cur-x prev-x)
                 (setq cur-y 9999)
                 (setq cur-behind prev-behind)
                 (setq cur-in-front t))
                ((string= rel "above")
                 (setq cur-x prev-x)
                 (setq cur-y (- prev-y 1))
                 (setq cur-behind prev-behind))
                ((string= rel "below")
                 (setq cur-x prev-x)
                 (setq cur-y (+ 1 prev-y))
                 (setq cur-behind prev-behind)
                 (setq cur-in-front prev-in-front))
                ((string= rel "to the left of")
                 (setq cur-x (- prev-x 1))
                 (setq cur-y prev-y)
                 (setq cur-behind prev-behind)
                 (setq cur-in-front prev-in-front))
                ((string= rel "to the right of")
                 (setq cur-x (+ 1 prev-x))
                 (setq cur-y prev-y)
                 (setq cur-behind prev-behind)
                 (setq cur-in-front prev-in-front))))))
    
    (if (and prev (member rel '("in" "on" "behind" "in front of")))
        (progn
          (oset box :parent prev)
          (oset prev :children (org-real--add-to-list (with-slots (children) prev children) box))
          (if containers
              (org-real--make-instance-helper containers prev box)
            (oset box :primary t)))
      (oset box :parent parent)
      (oset parent :children (org-real--add-to-list (with-slots (children) parent children) box))
      (if containers
          (org-real--make-instance-helper containers parent box)
        (oset box :primary t)))))

(cl-defmethod org-real--map-immediate (fn (box org-real--box))
  "Map a function FN across all immediate relatives of BOX, including BOX.

Any box with a :rel-box slot equivalent to BOX will be passed to
FN."
  (progn
    (funcall fn box)
    (mapc
     (lambda (box) (org-real--map-immediate fn box))
     (org-real--next box t))))

(cl-defmethod org-real--next ((box org-real--box) &optional exclude-children)
  "Retrieve any boxes for which the :rel-box slot is BOX.

If EXCLUDE-CHILDREN, only retrieve sibling boxes."
  (let ((relatives (append (if exclude-children '() (org-real--get-all
                                                     (with-slots (children) box children)))
                           (if (slot-boundp box :parent)
                               (org-real--get-all
                                (with-slots
                                    (children)
                                    (with-slots (parent) box parent)
                                  children))
                             '()))))
    (seq-filter
     (lambda (relative)
       (and (slot-boundp relative :rel-box)
            (string= (with-slots
                         (name)
                         (with-slots (rel-box) relative rel-box)
                       name)
                     (with-slots (name) box name))))
     relatives)))

(cl-defmethod org-real--expand ((box org-real--box))
  "Get a list of all boxes, including BOX, that are children of BOX."
  (with-slots (children) box
    (apply 'append (list box) (mapcar 'org-real--expand (org-real--get-all children)))))

(cl-defmethod org-real--merge-into ((from org-real--box) (to org-real--box))
  "Merge FROM box into TO box."
  (let ((from-boxes (reverse (org-real--expand from)))
        (to-boxes (reverse (org-real--expand to))))
    (unless (seq-some
             (lambda (from-box)
               (seq-some
                (lambda (to-box)
                  (when (and (slot-boundp from-box :name)
                             (slot-boundp to-box :name)
                             (string= (with-slots (name) from-box name)
                                      (with-slots (name) to-box name)))
                    (org-real--add-matching from-box to-box to)
                    t))
                to-boxes))
             from-boxes)
      (org-real--flex-add from to to))))

(cl-defmethod org-real--add-matching ((box org-real--box)
                                      (match org-real--box)
                                      (world org-real--box))
  "Add BOX to WORLD after finding a matching box MATCH already in WORLD.

MATCH is used to set the :rel-box and :parent slots on children
of BOX."
  (with-slots
      (parent
       (match-y y-order)
       (match-x x-order)
       (match-behind behind)
       (match-in-front in-front))
      match
    (let ((next-boxes (org-real--next box)))
      (mapc
       (lambda (next)
         (with-slots
             (rel
              (next-y y-order)
              (next-x x-order)
              (next-behind behind)
              (next-in-front in-front))
             next
           (cond
            ((string= rel "above")
             (setq next-y match-y)
             (org-real--map-immediate
              (lambda (child)
                (with-slots ((child-y y-order)) child
                  (when (>= child-y match-y)
                    (setq child-y (+ 1 child-y)))))
              match)
             (setq next-x match-x)
             (setq next-behind match-behind))
            ((string= rel "below")
             (setq next-y (+ 1 match-y))
             (org-real--map-immediate
              (lambda (child)
                (with-slots ((child-y y-order)) child
                  (when (> child-y match-y)
                    (setq child-y (+ 1 child-y)))))
              match)
             (setq next-x match-x)
             (setq next-behind match-behind))
            ((string= rel "to the right of")
             (setq next-x (+ 1 match-x))
             (org-real--map-immediate
              (lambda (child)
                (with-slots ((child-x x-order)) child
                  (when (> child-x match-x)
                    (setq child-x (+ 1 child-x)))))
              match)
             (setq next-y match-y)
             (setq next-behind match-behind)
             (setq next-in-front match-in-front))
            ((string= rel "to the left of")
             (setq next-x match-x)
             (org-real--map-immediate
              (lambda (child)
                (with-slots ((child-x x-order)) child
                  (when (>= child-x match-x)
                    (setq child-x (+ 1 child-x)))))
              match)
             (setq next-y match-y)
             (setq next-behind match-behind)
             (setq next-in-front match-in-front)))
             
           (oset next :rel-box match)
           (if (member rel '("in" "on" "behind" "in front of"))
               (org-real--flex-add next match world)
             (oset next :parent parent)
             (oset parent :children (org-real--add-to-list
                                     (with-slots (children) parent children)
                                     next)))
           (org-real--add-matching next next world)))
      next-boxes))))

(cl-defmethod org-real--flex-add ((box org-real--box)
                                  (parent org-real--box)
                                  (world org-real--box))
  "Add BOX to a PARENT box already existing in WORLD.

This function ignores the :rel slot and adds BOX in such a way
that the width of WORLD is kept below 80 characters if possible."
  (with-slots ((siblings children)) parent
    (let* ((cur-width (org-real--get-width world))
           (siblings (org-real--get-all siblings))
           (last-sibling (and siblings (seq-reduce
                                        (lambda (max sibling)
                                          (with-slots
                                              ((max-x x-order)
                                               (max-y y-order))
                                              max
                                            (with-slots
                                                ((sibling-x x-order)
                                                 (sibling-y y-order))
                                                sibling
                                              (if (> sibling-y max-y)
                                                  sibling
                                                (if (and (= max-y sibling-y) (> sibling-x max-x))
                                                    sibling
                                                  max)))))
                                        (seq-filter
                                         (lambda (sibling)
                                           (not (with-slots (in-front) sibling in-front)))
                                         siblings)
                                        (org-real--box :y-order -9999)))))
      (oset box :parent parent)
      (oset parent :children (org-real--add-to-list (with-slots (children) parent children) box))
      (when (and last-sibling (not (with-slots (in-front) box in-front)))
        (with-slots
            ((last-sibling-y y-order)
             (last-sibling-x x-order))
            last-sibling
          (oset box :y-order last-sibling-y)
          (oset box :x-order (+ 1 last-sibling-x))
          (let ((new-width (org-real--get-width world)))
            (when (and (> new-width cur-width) (> new-width 80))
              (oset box :y-order (+ 1 last-sibling-y))
              (oset box :x-order 0))))))))



(provide 'org-real--box)

;;; org-real--box.el ends here
