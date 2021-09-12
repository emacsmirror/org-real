;;; org-real.el --- Keep track of real things as org-mode links -*- lexical-binding: t -*-

;; Author: Tyler Grinn <tylergrinn@gmail.com>
;; Version: 0.1.0
;; File: org-real.el
;; Package-Requires: ((emacs "26.1"))
;; Keywords: tools
;; URL: https://gitlab.com/tygrdev/org-real

;;; Commentary:

;; This package adds a 'real' type link to org mode to create links to
;; real things.
;;
;; The function `org-real-world' will display all real links in the
;; current buffer.

;;; Code:

;;;; Requirements

(require 'eieio)
(require 'org-element)
(require 'cl-lib)

(require 'org-real-box)

;;;; Customization variables

(defcustom org-real-margin '(2 . 1)
  "Margin to be used when displaying boxes.

The first number is the horizontal margin, second is the vertical
margin"
  :type 'cons
  :group 'org-real)

(defcustom org-real-padding '(2 . 1)
  "Padding to be used when displaying boxes.

The first number is the horizontal padding, second is the
vertical padding"
  :type 'cons
  :group 'org-real)

;;;; Faces

(defface org-real-primary
  '((t :background "aquamarine"
       :foreground "black"))
  "Face for the last thing in a real link."
  :group 'org-real)

;;;; Constants

(defconst org-real-prepositions
  '("in" "on" "behind" "in front of" "above" "below" "to the left of" "to the right of")
  "List of available prepositions for things.")

;;;; Class definitions

(defclass org-real-box-collection ()
  ((box :initarg :box
        :type org-real-box)
   (next :initarg :next
         :type org-real-box-collection))
  "A collection of `org-real-box'es.")

(defclass org-real-box ()
  ((name :initarg :name
         :type string)
   (rel :initarg :rel
        :type string)
   (rel-box :initarg :rel-box
            :type org-real-box)
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
           :type org-real-box)
   (children :initarg :children
             :initform (org-real-box-collection)
             :type org-real-box-collection)
   (primary :initarg :primary
            :initform nil
            :type boolean))
  "A representation of a box in 3D space.")


(cl-defmethod org-real--make-instance ((_ (subclass org-real-box)) containers)
  "Create an instance of `org-real-box' from CONTAINERS.

CONTAINERS is a list of plists containing at least a :name
property and optionally a :rel property."
  (when-let* ((world (org-real-box))
              (base-container (pop containers))
              (base (org-real-box :name (plist-get base-container :name))))
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
          (org-real-box)
        (car boxes))
    (let ((world (org-real-box)))
      (while boxes
        (org-real--merge-into (pop boxes) world))
      world)))

;;;; Drawing

(cl-defmethod org-real--draw ((box org-real-box) offset)
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

(cl-defmethod org-real--get-width ((box org-real-box))
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

(cl-defmethod org-real--get-height ((box org-real-box))
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

(cl-defmethod org-real--get-top ((box org-real-box))
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
                                         (org-real-box :y-order -9999))))
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

(cl-defmethod org-real--get-left ((box org-real-box))
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
                                  (org-real-box :x-order -9999)))))
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

;;;; `org-real-box' utility expressions

(cl-defmethod org-real--get-all ((collection org-real-box-collection))
  "Get all boxes in COLLECTION as a sequence."
  (with-slots (box next) collection
    (append (if (slot-boundp collection :box) (list box))
            (if (slot-boundp collection :next) (org-real--get-all next)))))

(cl-defmethod org-real--add-to-list ((collection org-real-box-collection)
                                     (box org-real-box))
  "Add BOX to COLLECTION and return new COLLECTION."
  (if (slot-boundp collection :box)
      (org-real-box-collection
       :box box
       :next collection)
    (oset collection :box box)
    collection))

(cl-defmethod org-real--make-instance-helper (containers parent (prev org-real-box))
  "Help create a 3D representation of CONTAINERS.

PREV must already existing in PARENT."
  (let* ((container (pop containers))
         (rel (plist-get container :rel))
         (box (org-real-box :name (plist-get container :name))))
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

(cl-defmethod org-real--map-immediate (fn (box org-real-box))
  "Map a function FN across all immediate relatives of BOX, including BOX.

Any box with a :rel-box slot equivalent to BOX will be passed to
FN."
  (progn
    (funcall fn box)
    (mapc
     (lambda (box) (org-real--map-immediate fn box))
     (org-real--next box t))))

(cl-defmethod org-real--next ((box org-real-box) &optional exclude-children)
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

(cl-defmethod org-real--expand ((box org-real-box))
  "Get a list of all boxes, including BOX, that are children of BOX."
  (with-slots (children) box
    (apply 'append (list box) (mapcar 'org-real--expand (org-real--get-all children)))))

(cl-defmethod org-real--merge-into ((from org-real-box) (to org-real-box))
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

(cl-defmethod org-real--add-matching ((box org-real-box)
                                      (match org-real-box)
                                      (world org-real-box))
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

(cl-defmethod org-real--flex-add ((box org-real-box)
                                  (parent org-real-box)
                                  (world org-real-box))
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
                                        (org-real-box :y-order -9999)))))
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


;;;; General utility expressions

(defun org-real--find-last-index (pred sequence)
  "Return the index of the last element for which (PRED element) is non-nil in SEQUENCE."
  (let ((i (- (length sequence) 1)))
    (catch 'match
      (mapc
       (lambda (elt)
         (if (funcall pred elt) (throw 'match i))
         (setq i (- i 1)))
       (reverse sequence))
      nil)))

(defun org-real--link-make-string (link &optional description)
  "Make a bracket link, consisting of LINK and DESCRIPTION.
LINK is escaped with backslashes for inclusion in buffer."
  (let* ((zero-width-space (string ?\x200B))
	 (description
	  (and (org-string-nw-p description)
	       ;; Description cannot contain two consecutive square
	       ;; brackets, or end with a square bracket.  To prevent
	       ;; this, insert a zero width space character between
	       ;; the brackets, or at the end of the description.
	       (replace-regexp-in-string
		"\\(]\\)\\(]\\)"
		(concat "\\1" zero-width-space "\\2")
		(replace-regexp-in-string "]\\'"
					  (concat "\\&" zero-width-space)
					  (org-trim description))))))
    (if (not (org-string-nw-p link)) description
      (format "[[%s]%s]"
	      (org-link-escape link)
	      (if description (format "[%s]" description) "")))))


(defun org-real--parse-url (str)
  "Parse STR into a list of plists.

Returns a list of plists with a :name property and optionally a
:rel property."
  (let* ((url (url-generic-parse-url str))
         (host (url-host url))
         (path-and-query (url-path-and-query url))
         (tokens (cdr
                     (split-string (concat (car path-and-query) "?"
                                           (cdr path-and-query))
                                   "/")))
         (containers (mapcar
                      (lambda (token)
                        (let* ((location (split-string token "\\?"))
                               (container (list :name (car location)))
                               (rel (and (string-match "&?rel=\\([^&]*\\)" (cadr location))
                                         (match-string 1 (cadr location)))))
                          (if rel
                              (plist-put container :rel rel)
                            container)))
                      tokens)))
    (push (list :name host) containers)))

(defun org-real--parse-buffer ()
  "Parse all real links in the current buffer."
  (let ((container-matrix '()))
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
        (if (string= (org-element-property :type link) "real")
            (add-to-list 'container-matrix
                          (org-real--parse-url
                           (org-element-property :raw-link link))
                          t))))
    container-matrix))

(defun org-real--to-link (containers)
  "Create a link string from CONTAINERS."
  (concat "real://"
          (mapconcat
           (lambda (container)
             (concat (plist-get container :name)
                     (when (plist-member container :rel)
                       (concat "?rel=" (plist-get container :rel)))))
           containers
           "/")))

;;;; Interactive functions

(defun org-real-world ()
  "View all real links in the current buffer."
  (interactive)
  (org-real--pp
   (org-real--merge
    (mapcar
     (lambda (containers)
       (org-real--make-instance 'org-real-box containers))
     (org-real--parse-buffer)))))

;;;; `org-insert-link' configuration

(org-link-set-parameters "real"
                         :follow #'org-real-follow
                         :complete #'org-real-complete)

(defun org-real-follow (url &rest _)
  "Open a real link URL in a popup buffer."
  (let* ((containers (org-real--parse-url url))
         (box (org-real--make-instance 'org-real-box (copy-tree containers))))
    (org-real--pp box (copy-tree containers))))

(defun org-real-complete (&optional existing)
  "Complete a real link or edit EXISTING link."
  (let* ((container-matrix (org-real--parse-buffer))
         (containers (if existing
                         (org-real--parse-url existing)
                       (org-real--complete-thing "Thing: " container-matrix))))
    (catch 'confirm
      (while t
        (org-real--pp (org-real--make-instance 'org-real-box containers) containers)
        (let ((response (read-event "RETURN    - Confirm\nBACKSPACE - Remove context\n+         - Add context")))
          (cond
           ((eq response 'return)
            (throw 'confirm containers))
           ((eq response 'backspace)
            (pop containers)
            (if (= 0 (length containers))
                (setq containers (org-real--complete-thing "Thing: " container-matrix))))
           ((eq response ?+)
            (let* ((top (plist-get (car containers) :name))
                   (preposition
                    (completing-read (concat "The " top " is: ") org-real-prepositions nil t))
                   (additional-containers
                    (org-real--complete-thing (concat "The " top " is " preposition " the: ") container-matrix)))
              (setcar containers (plist-put (car containers) :rel preposition))
              (setq containers (append additional-containers containers))))))))
    (org-real--to-link containers)))

(defun org-real--complete-thing (prompt container-matrix)
  "Use `completing-read' with PROMPT to get a list of containers.

CONTAINER-MATRIX is used to generate possible completions.  The
return value is the longest list of containers from the matrix
that contains, as the last element, a container with a name
matching the one returned from `completing-read'."
  (let* ((completions (mapcar
                       (lambda (container) (plist-get container :name))
                       (apply 'append container-matrix)))
         (result (completing-read prompt completions nil 'confirm))
         (existing-containers (car (seq-sort
                                    (lambda (a b) (> (length a) (length b)))
                                    (mapcar
                                     (lambda (containers)
                                       (cl-subseq containers 0
                                                  (+ 1 (org-real--find-last-index
                                                        (lambda (container)
                                                          (string= (plist-get container :name) result))
                                                        containers))))
                                     (seq-filter
                                      (lambda (containers)
                                        (seq-some
                                         (lambda (container)
                                           (string= (plist-get container :name) result))
                                         containers))
                                      container-matrix))))))
    (if existing-containers
        existing-containers
      `((:name ,result)))))

;;; Hooks

(defun org-real--read-string-advice (orig prompt link &rest args)
  "Advise `read-string' during `org-insert-link' to use custom completion.

ORIG is `read-string', PROMPT and LINK and ARGS are the arguments
passed to it."
  (if (string= "real" (ignore-errors (url-type (url-generic-parse-url link))))
      (org-real-complete link)
    (apply orig prompt link args)))

(defun org-real--maybe-edit-link (orig &rest args)
  "Advise `org-insert-link' to advise `read-string' during editing of a link.

ORIG is `org-insert-link', ARGS are the arguments passed to it."
  (advice-add 'read-string :around #'org-real--read-string-advice)
  (unwind-protect
      (if (called-interactively-p 'any)
          (call-interactively orig)
        (apply orig args))
    (advice-remove 'read-string #'org-real--read-string-advice)))

(advice-add 'org-insert-link :around #'org-real--maybe-edit-link)

(defun org-real--apply (&rest _)
  "Apply any change to the current buffer if last inserted link is real."
  (let (new-link replace-all)
    (cond
     ((org-in-regexp org-link-bracket-re 1)
      (setq new-link (match-string-no-properties 1)))
     ((org-in-regexp org-link-plain-re)
      (setq new-link (org-unbracket-string "<" ">" (match-string 0)))))
    (when (and new-link
               (string= "real" (ignore-errors (url-type (url-generic-parse-url new-link)))))
      (let ((new-containers (reverse (org-real--parse-url new-link))))
        (while new-containers
          (let ((primary (plist-get (car new-containers) :name))
                (changes '())
                old-containers)
            (org-element-map (org-element-parse-buffer) 'link
              (lambda (old-link)
                (when (string= (org-element-property :type old-link) "real")
                  (setq old-containers (reverse (org-real--parse-url
                                                 (org-element-property :raw-link old-link))))
                  
                  (when-let* ((new-index 0)
                              (old-index (seq-position
                                          old-containers
                                          primary
                                          (lambda (a b) (string= (plist-get a :name) b))))
                              (begin (org-element-property :begin old-link))
                              (end (org-element-property :end old-link))
                              (replace-link (org-real--to-link
                                             (reverse
                                              (append (cl-subseq old-containers 0 old-index)
                                                      new-containers))))
                              (old-desc ""))
                    (when (catch 'conflict
                            (if (not (= (length new-containers) (- (length old-containers) old-index)))
                                (throw 'conflict t))
                            (while (< new-index (length new-containers))
                              (if (or (not (string= (plist-get (nth new-index new-containers) :name)
                                                    (plist-get (nth old-index old-containers) :name)))
                                      (not (string= (plist-get (nth new-index new-containers) :rel)
                                                    (plist-get (nth old-index old-containers) :rel))))
                                  (throw 'conflict t))
                              (setq new-index (+ 1 new-index))
                              (setq old-index (+ 1 old-index)))
                            nil)
                      (goto-char begin)
                      (if (org-in-regexp org-link-bracket-re 1)
                          (setq old-desc (when (match-end 2) (match-string-no-properties 2))))
                      (push
                       `(lambda ()
                          (delete-region ,begin ,end)
                          (goto-char ,begin)
                          (insert (org-real--link-make-string ,replace-link ,old-desc)))
                       changes))))))
            (when (and changes
                       (or replace-all (let ((response
                                              (read-char-choice
                                               (concat
                                                "Replace all occurrences of "
                                                primary
                                                " in current buffer? y/n/a ")
                                               '(?y ?Y ?n ?N ?a ?A)
                                               t)))
                                         (cond
                                          ((or (= response ?y) (= response ?Y)) t)
                                          ((or (= response ?n) (= response ?N)) nil)
                                          ((or (= response ?a) (= response ?A))
                                           (setq replace-all t))))))
              (mapc 'funcall changes)))
          (pop new-containers)))))
  (message nil))

(advice-add 'org-insert-link :after #'org-real--apply)

;;;; Pretty printing

(defun org-real--pp (box &optional containers)
  "Pretty print BOX in a popup buffer.

If CONTAINERS is passed in, also pretty print a sentence
describing where BOX is."
  (let ((top (org-real--get-top box))
        (width (org-real--get-width box))
        (height (org-real--get-height box))
        (inhibit-read-only t)
        (buffer (get-buffer-create "Org Real")))
    (with-current-buffer buffer
      (erase-buffer)
      (toggle-truncate-lines t)
      (if containers (org-real--pp-text containers))
      (let ((offset (- (line-number-at-pos)
                       (cdr org-real-margin)
                       (* 2 (cdr org-real-padding)))))
        (dotimes (_ (+ top height)) (insert (concat (make-string width ?\s) "\n")))
        (org-real--draw box offset)
        (special-mode)))
    (display-buffer buffer `(display-buffer-pop-up-window
                             (window-width . 80)
                             (window-height . ,height)))))
(defun org-real--pp-text (containers)
  "Insert a textual representation of CONTAINERS into the current buffer."
  (let* ((reversed (reverse containers))
         (container (pop reversed))
         (primary-name (plist-get container :name)))
    (dotimes (_ (cdr org-real-padding)) (insert "\n"))
    (insert (make-string (car org-real-padding) ?\s))
    (insert "The ")
    (put-text-property 0 (length primary-name) 'face 'org-real-primary
                       primary-name)
    (insert primary-name)
    (if reversed (insert " is"))
    (while reversed
      (insert " ")
      (insert (plist-get container :rel))
      (setq container (pop reversed))
      (insert " the ")
      (insert (plist-get container :name)))
    (insert ".")
    (fill-paragraph)
    (insert "\n")))

(provide 'org-real)

;;; org-real.el ends here
