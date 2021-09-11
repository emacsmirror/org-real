;;; org-real.el --- Create org-mode links to real things -*- lexical-binding: t -*-

;;; Commentary:

;; Box class definition and related methods

;;;; Patch! 0.0.1 -> 0.1.0

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

;;;; Class definition

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
             :initform '()
             :type list)
   (primary :initarg :primary
            :initform nil
            :type boolean)))

;;;; Exports

(defun org-real--create-box (containers)
  "Create an `org-real--box' from CONTAINERS.

CONTAINERS is a list of plists containing at least a :name
property and optionally a :rel property.  PARENT and PREV
parameters are used internally and should not be supplied."
  (let ((world (org-real--box)))
    (org-real--create-box-helper containers world)
    world))

(defun org-real--merge (boxes)
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
  (let ((children (oref box :children)))
    (if (slot-boundp box :name)
        (let* ((top (+ offset (org-real--get-top box)))
               (left (org-real--get-left box))
               (width (org-real--get-width box))
               (height (org-real--get-height box))
               (name (oref box :name))
               (dashed (oref box :behind))
               (align-bottom (oref box :in-front))
               (primary (oref box :primary)))
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
                (draw (cons (+ top height -1 (cdr org-real--margin)) left)
                      (concat "┴" (make-string (- width 2) (if dashed #x254c #x2500)) "┴"))
              (draw (cons (+ top height -1) left)
                    (concat "└" (make-string (- width 2) (if dashed #x254c #x2500)) "┘")))
            (draw (cons (+ top 1 (cdr org-real--padding))
                        (+ left 1 (car org-real--padding)))
                  name
                  primary)
            (let ((r (+ top 1))
                  (c1 left)
                  (c2 (+ left width -1)))
              (dotimes (_ (- height (if align-bottom 1 2)))
                (draw (cons r c1) (if dashed "╎" "│"))
                (draw (cons r c2) (if dashed "╎" "│"))
                (setq r (+ r 1)))))))
    (mapc
     (lambda (child) (org-real--draw child offset))
     children)))

(cl-defmethod org-real--get-width ((box org-real--box))
  "Get the width of BOX."
  (let* ((base-width (+ 2 ; box walls
                        (* 2 (car org-real--padding))))
         (width (+ base-width (if (slot-boundp box :name)
                                  (length (oref box :name))
                                0)))
         (children (oref box :children)))
    (if (not children)
        width
      (let* ((column-indices (delete-duplicates
                              (mapcar (lambda (child) (oref child :x-order)) children)))
             (columns (mapcar
                       (lambda (c)
                         (seq-filter
                          (lambda (child)
                            (= c (oref child :x-order)))
                          children))
                       column-indices))
             (column-widths (mapcar
                             (lambda (column)
                               (apply 'max (mapcar 'org-real--get-width column)))
                             columns))
             (children-width (seq-reduce
                              (lambda (total width)
                                (+ total (car org-real--margin) width))
                              column-widths
                              (* -1 (car org-real--margin)))))
        (if (> width (+ (* 2 (car org-real--margin)) children-width))
            width
          (+ base-width children-width))))))

(cl-defmethod org-real--get-height ((box org-real--box))
  "Get the height of BOX."
  (let* ((in-front (oref box :in-front))
         (height (+ (if in-front
                        (* -1 (cdr org-real--margin))
                      0)
                    2 ; box walls
                    (* 2 (cdr org-real--padding))
                    (cdr org-real--margin)))
         (children (oref box :children)))
    (if (not children)
        height
      (let* ((row-indices (delete-duplicates
                           (mapcar (lambda (child) (oref child :y-order)) children)))
             (rows (mapcar
                    (lambda (r)
                      (seq-filter
                       (lambda (child)
                         (= r (oref child :y-order)))
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
    (let* ((offset (+ 1 (* 2 (cdr org-real--padding)) (cdr org-real--margin)))
           (parent (oref box :parent))
           (top (+ offset (org-real--get-top parent))))
      (let* ((x-order (oref box :x-order))
             (y-order (oref box :y-order))
             (above (seq-filter
                     (lambda (child) (and (= x-order (oref child :x-order))
                                          (< (oref child :y-order) y-order)))
                     (oref parent :children)))
             (directly-above (and above (seq-reduce
                                         (lambda (max child)
                                           (if (> (oref child :y-order) (oref max :y-order))
                                               child
                                             max))
                                         above
                                         (org-real--box :y-order -9999))))
             (above-height (and directly-above (apply 'max
                                                      (mapcar
                                                       'org-real--get-height
                                                       (seq-filter
                                                        (lambda (child)
                                                          (= (oref directly-above :y-order)
                                                             (oref child :y-order)))
                                                        (oref parent :children)))))))
        (if directly-above
            (+ (org-real--get-top directly-above)
               above-height)
          (if (and (slot-boundp box :rel)
                   (or (string= "to the left of" (oref box :rel))
                       (string= "to the right of" (oref box :rel))))
              (org-real--get-top (oref box :rel-box))
            top))))))

(cl-defmethod org-real--get-left ((box org-real--box))
  "Get the left column index of BOX."
  (if (not (slot-boundp box :parent))
      0
    (let* ((parent (oref box :parent))
           (left (+ 1
                    (car org-real--padding)
                    (org-real--get-left parent)))
           (to-the-left (seq-filter
                         (lambda (child) (and (= (oref box :y-order) (oref child :y-order))
                                              (< (oref child :x-order) (oref box :x-order))))
                         (oref parent :children)))
           (directly-left (and to-the-left
                               (seq-reduce
                                (lambda (max child)
                                  (if (> (oref child :x-order) (oref max :x-order))
                                      child
                                    max))
                                to-the-left
                                (org-real--box :x-order -9999)))))
      (if directly-left
          (+ (org-real--get-left directly-left)
             (org-real--get-width directly-left)
             (car org-real--margin))
        (if (and (slot-boundp box :rel)
                 (or (string= "above" (oref box :rel))
                     (string= "below" (oref box :rel))))
            (org-real--get-left (oref box :rel-box))
          left)))))

;;;; Utility expressions
  
(defun org-real--create-box-helper (containers parent &optional prev)
  (let* ((container (pop containers))
         (rel (plist-get container :rel))
         (box (org-real--box :name (plist-get container :name))))
    (when prev
      (oset box :rel (plist-get container :rel))
      (oset box :rel-box prev)
      (cond ((or (string= rel "in") (string= rel "on"))
             (oset box :x-order (oref prev :x-order))
             (oset box :y-order (oref prev :y-order))
             (oset box :behind (oref prev :behind)))
            ((string= rel "behind")
             (oset box :x-order (oref prev :x-order))
             (oset box :y-order (oref prev :y-order))
             (oset box :behind t))
            ((string= rel "in front of")
             (oset box :x-order (oref prev :x-order))
             (oset box :y-order 9999)
             (oset box :behind (oref prev :behind))
             (oset box :in-front t))
            ((string= rel "above")
             (oset box :x-order (oref prev :x-order))
             (oset box :y-order (- (oref prev :y-order) 1))
             (oset box :behind (oref prev :behind)))
            ((string= rel "below")
             (oset box :x-order (oref prev :x-order))
             (oset box :y-order (+ 1 (oref prev :y-order)))
             (oset box :behind (oref prev :behind))
             (oset box :in-front (oref prev :in-front)))
            ((string= rel "to the left of")
             (oset box :x-order (- (oref prev :x-order) 1))
             (oset box :y-order (oref prev :y-order))
             (oset box :behind (oref prev :behind))
             (oset box :in-front (oref prev :in-front)))
            ((string= rel "to the right of")
             (oset box :x-order (+ 1 (oref prev :x-order)))
             (oset box :y-order (oref prev :y-order))
             (oset box :behind (oref prev :behind))
             (oset box :in-front (oref prev :in-front)))))
    
    (if (and prev (member (oref box :rel)
                          '("in" "on" "behind" "in front of")))
        (progn
          (oset box :parent prev)
          (object-add-to-list prev :children box)
          (if containers
              (org-real--create-box-helper containers prev box)
            (oset box :primary t)))
      (oset box :parent parent)
      (object-add-to-list parent :children box)
      (if containers
          (org-real--create-box-helper containers parent box)
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
  (let ((relatives (append (if exclude-children '() (oref box :children))
                           (if (slot-boundp box :parent) (oref (oref box :parent) :children) '()))))
    (seq-filter
     (lambda (relative)
       (and (slot-boundp relative :rel-box)
            (string= (oref (oref relative :rel-box) :name)
                     (oref box :name))))
     relatives)))

(cl-defmethod org-real--expand ((box org-real--box))
  "Get a list of all boxes, including BOX, that are children of BOX."
  (apply 'append (list box) (mapcar 'org-real--expand (oref box :children))))

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
                             (string= (oref from-box :name) (oref to-box :name)))
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
  (let ((next-boxes (org-real--next box))
        (parent (oref match :parent)))
    (mapc
     (lambda (next)
       (let ((rel (oref next :rel)))
         (cond
          ((string= rel "above")
           (let ((y-order (oref match :y-order)))
             (oset next :y-order y-order)
             (org-real--map-immediate
              (lambda (box) (when (>= (oref box :y-order) y-order)
                              (oset box :y-order (+ 1 (oref box :y-order)))))
              match))
           (oset next :x-order (oref match :x-order))
           (oset next :behind (oref match :behind)))
          ((string= rel "below")
           (let ((y-order (oref match :y-order)))
             (oset next :y-order (+ 1 y-order))
             (org-real--map-immediate
              (lambda (box) (when (> (oref box :y-order) y-order)
                              (oset box :y-order (+ 1 (oref box :y-order)))))
              match))
           (oset next :x-order (oref match :x-order))
           (oset next :behind (oref match :behind)))
          ((string= rel "to the right of")
           (let ((x-order (oref match :x-order)))
             (oset next :x-order (+ 1 x-order))
             (org-real--map-immediate
              (lambda (box) (when (> (oref box :x-order) x-order)
                              (oset box :x-order (+ 1 (oref box :x-order)))))
              match))
           (oset next :y-order (oref match :y-order))
           (oset next :behind (oref match :behind))
           (oset next :in-front (oref match :in-front)))
          ((string= rel "to the left of")
           (let ((x-order (oref match :x-order)))
             (oset next :x-order x-order)
             (org-real--map-immediate
              (lambda (box) (when (>= (oref box :x-order) x-order)
                              (oset box :x-order (+ 1 (oref box :x-order)))))
              match))
           (oset next :y-order (oref match :y-order))
           (oset next :behind (oref match :behind))
           (oset next :in-front (oref match :in-front))))

         (oset next :rel-box match)
         (if (member rel '("in" "on" "behind" "in front of"))
             (org-real--flex-add next match world)
           (oset next :parent parent)
           (object-add-to-list parent :children next))
         (org-real--add-matching next next world)))
     next-boxes)))

(cl-defmethod org-real--flex-add ((box org-real--box)
                                  (parent org-real--box)
                                  (world org-real--box))
  "Add BOX to a PARENT box already existing in WORLD.

This function ignores the :rel slot and adds BOX in such a way
that the width of WORLD is kept below 80 characters if possible."
  (let* ((cur-width (org-real--get-width world))
         (siblings (oref parent :children))
         (last-sibling (and siblings (seq-reduce
                                    (lambda (max sibling)
                                      (let ((max-x (oref max :x-order))
                                            (max-y (oref max :y-order))
                                            (sibling-x (oref sibling :x-order))
                                            (sibling-y (oref sibling :y-order)))
                                        (if (> sibling-y max-y)
                                            sibling
                                          (if (and (= max-y sibling-y) (> sibling-x max-x))
                                              sibling
                                            max))))
                                    (seq-filter
                                     (lambda (sibling) (not (oref sibling :in-front)))
                                     siblings)
                                    (org-real--box :y-order -9999)))))
    (oset box :parent parent)
    (object-add-to-list parent :children box)
    (when (and last-sibling (not (oref box :in-front)))
      (oset box :y-order (oref last-sibling :y-order))
      (oset box :x-order (+ 1 (oref last-sibling :x-order)))
      (let ((new-width (org-real--get-width world)))
        (when (and (> new-width cur-width) (> new-width 80))
          (oset box :y-order (+ 1 (oref last-sibling :y-order)))
          (oset box :x-order 0))))))



(provide 'org-real--box)
