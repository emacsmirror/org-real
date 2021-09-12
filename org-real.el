;;; org-real.el --- Keep track of real things as org-mode links -*- lexical-binding: t -*-

;; Author: Tyler Grinn <tylergrinn@gmail.com>
;; Version: 0.2.0
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

;;;; Patch! 0.1.1 > 0.2.0+
;;;; Will be removed in version 1.0.0+

(let ((customizations (get 'org-real 'custom-group)))
  (setf customizations (cl-delete "org-real-margin" customizations :key #'car :test #'string=))
  (setf customizations (cl-delete "org-real-padding" customizations :key #'car :test #'string=))
  (put 'org-real 'custom-group customizations))

;;;; Requirements

(require 'eieio)
(require 'org-element)
(require 'cl-lib)

;;;; Customization variables

(defgroup org-real nil
  "Customization options for org-real"
  :group 'applications)

(defcustom org-real-margin-x 2
  "Horizontal margin to be used when displaying boxes."
  :type 'number
  :group 'org-real)

(defcustom org-real-margin-y 1
  "Vertical margin to be used when displaying boxes."
  :type 'number
  :group 'org-real)

(defcustom org-real-padding-x 2
  "Horizontal padding to be used when displaying boxes."
  :type 'number
  :group 'org-real)

(defcustom org-real-padding-y 1
  "Vertical padding to be used when displaying boxes."
  :type 'number
  :group 'org-real)

;;;; Faces

(defface org-real-primary
  '((t :background "aquamarine"
       :foreground "black"))
  "Face for the last thing in a real link."
  :group 'org-real)

;;;; Constants

(defconst org-real-prepositions
  '("in" "on" "behind" "in front of" "above" "below" "to the left of" "to the right of" "on top of")
  "List of available prepositions for things.")

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
                       org-real-margin-y
                       (* 2 org-real-padding-y))))
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
    (dotimes (_ org-real-padding-y) (insert "\n"))
    (insert (make-string org-real-padding-x ?\s))
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

;;;; Class definitions and public methods

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
   (on-top :initarg :on-top
           :initform nil
           :type boolean)
   (parent :initarg :parent
           :type org-real-box)
   (children :initarg :children
             :initform (org-real-box-collection)
             :type org-real-box-collection)
   (top :initarg :top
        :type number)
   (left :initarg :left
         :type number)
   (width :initarg :width
          :type number)
   (height :initarg :height
           :type number)
   (primary :initarg :primary
            :initform nil
            :type boolean))
  "A representation of a box in 3D space.")


(cl-defmethod org-real--get-all ((collection org-real-box-collection))
  "Get all boxes in COLLECTION as a sequence."
  (with-slots (box next) collection
    (append (if (slot-boundp collection :box) (list box))
            (if (slot-boundp collection :next) (org-real--get-all next)))))

(cl-defmethod org-real--push ((collection org-real-box-collection)
                                     (box org-real-box))
  "Add BOX to COLLECTION and return new COLLECTION."
  (if (slot-boundp collection :box)
      (org-real-box-collection
       :box box
       :next collection)
    (oset collection :box box)
    collection))

(cl-defmethod org-real--make-instance ((_ (subclass org-real-box)) containers)
  "Create an instance of `org-real-box' from CONTAINERS.

CONTAINERS is a list of plists containing at least a :name
property and optionally a :rel property."
  (when-let* ((world (org-real-box))
              (base-container (pop containers))
              (base (org-real-box :name (plist-get base-container :name))))
    (oset base :parent world)
    (with-slots (children) world
      (setq children (org-real--push children base)))
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
    (with-slots (name behind in-front on-top (dashed behind) primary) box
      (when (slot-boundp box :name)
        (let* ((top (+ offset (org-real--get-top box)))
               (left (org-real--get-left box))
               (width (org-real--get-width box))
               (height (org-real--get-height box))
               (align-bottom (or in-front on-top)))
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
            (draw (cons (+ top 1 org-real-padding-y)
                        (+ left 1 org-real-padding-x))
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
  (with-slots ((stored-width width)) box
    (if (slot-boundp box :width)
        stored-width
      (let* ((base-width (+ 2 ; box walls
                            (* 2 org-real-padding-x)))
             (width (+ base-width
                       (if (slot-boundp box :name)
                           (with-slots (name) box (length name))
                         0)))
             (children (with-slots (children) box (org-real--get-all children))))
        (if (not children)
            (setq stored-width width)
          (let* ((row-indices (cl-delete-duplicates
                               (mapcar
                                (lambda (child) (with-slots (y-order) child y-order))
                                children)))
                 (rows (mapcar
                        (lambda (r)
                          (cl-delete-duplicates
                           (seq-filter
                            (lambda (child) (with-slots (y-order) child (= r y-order)))
                            children)
                           :test #'(lambda (a b) (string= (with-slots (name) a name)
                                                          (with-slots (name) b name)))))
                        row-indices))
                 (children-width (apply 'max
                                        (mapcar
                                         (lambda (row)
                                           (seq-reduce
                                            (lambda (sum width)
                                              (+ sum width org-real-margin-x))
                                            (mapcar 'org-real--get-width row)
                                            (* -1 org-real-margin-x)))
                                         rows))))
            (if (> width (+ (* 2 org-real-margin-x) children-width))
                (setq stored-width width)
              (setq stored-width (+ base-width children-width)))))))))

(cl-defmethod org-real--get-on-top-height ((box org-real-box))
  "Get the height of any boxes on top of the parent of BOX."
  (with-slots (children rel) box
    (+
     (if (and (slot-boundp box :rel)
              (string= "on top of" rel))
         (org-real--get-height box)
       0)
     (apply 'max 0
            (mapcar
             'org-real--get-on-top-height
             (seq-filter
              (lambda (child)
                (with-slots ((child-rel rel)) child
                  (and (slot-boundp child :rel)
                       (string= "on top of" child-rel))))
              (org-real--get-all children)))))))

(cl-defmethod org-real--get-height ((box org-real-box) &optional include-on-top)
  "Get the height of BOX.

If INCLUDE-ON-TOP is non-nil, also include height on top of box"
  (let ((on-top-height (if include-on-top (org-real--get-on-top-height box) 0)))
    (with-slots ((stored-height height) in-front on-top) box
      (if (slot-boundp box :height)
          (+ stored-height on-top-height)
        (let ((height (+ (if (or in-front on-top) -1 0)
                         3 ; box walls + text
                         (* 2 org-real-padding-y)))
              (children (seq-filter
                         (lambda (child) (with-slots (on-top) child (not on-top)))
                         (with-slots (children) box (org-real--get-all children)))))
          (if (not children)
              (progn
                (setq stored-height height)
                (+ height on-top-height))
            (let* ((last-row (seq-reduce
                              (lambda (last-row child)
                                (with-slots ((last-y y-order)) (car last-row)
                                  (with-slots ((child-y y-order)) child
                                    (cond ((= last-y child-y)
                                           (push child last-row)
                                           last-row)
                                          ((> child-y last-y) (list child))
                                          (t last-row)))))
                              children
                              (list (pop children))))
                   (last-row-top (org-real--get-top (car last-row)))
                   (last-row-height (apply 'max (mapcar
                                                 (lambda (child)
                                                   (org-real--get-height child include-on-top))
                                                 last-row))))
              (setq stored-height (-
                                   (+ (if in-front 0 org-real-padding-y)
                                      last-row-top
                                      last-row-height)
                                   (org-real--get-top box)))
              (+ stored-height on-top-height))))))))

(cl-defmethod org-real--get-top ((box org-real-box))
  "Get the top row index of BOX."
  (with-slots ((stored-top top) on-top parent x-order y-order rel rel-box) box
    (cond ((slot-boundp box :top) stored-top)
          (on-top (- (org-real--get-top parent) (org-real--get-height box)))
          (t
           (let ((on-top-height (org-real--get-on-top-height box)))
             (if (not (slot-boundp box :parent))
                 (setq stored-top on-top-height)
               (let* ((siblings (with-slots (children) parent
                                  (seq-filter
                                   (lambda (sibling)
                                     (with-slots (on-top in-front) sibling
                                       (not (or on-top in-front))))
                                   (org-real--get-all children))))
                      (offset (+ 2 org-real-padding-y org-real-margin-y))
                      (top (+ on-top-height offset (org-real--get-top parent)))
                      (above (seq-filter
                              (lambda (sibling)
                                (with-slots ((sibling-x x-order) (sibling-y y-order)) sibling
                                  (and (= x-order sibling-x)
                                       (< sibling-y y-order))))
                              siblings))
                      (directly-above (and above (seq-reduce
                                                  (lambda (max child)
                                                    (with-slots ((max-y y-order)) max
                                                      (with-slots ((child-y y-order)) child
                                                        (if (> child-y max-y)
                                                            child
                                                          max))))
                                                  above
                                                  (org-real-box :y-order -9999))))
                      (above-height (and directly-above (+ org-real-margin-y
                                                           (apply 'max
                                                                  (mapcar
                                                                   'org-real--get-height
                                                                   (seq-filter
                                                                    (lambda (sibling)
                                                                      (= (with-slots (y-order) directly-above y-order)
                                                                         (with-slots (y-order) sibling y-order)))
                                                                    siblings)))))))
                 (if directly-above
                     (setq stored-top (+ on-top-height
                                         (org-real--get-top directly-above)
                                         above-height))
                   (if (and (slot-boundp box :rel)
                            (or (string= "to the left of" rel)
                                (string= "to the right of" rel)))
                       (setq stored-top (org-real--get-top rel-box))
                     (setq stored-top top))))))))))

(cl-defmethod org-real--get-left ((box org-real-box))
  "Get the left column index of BOX."
  (with-slots ((stored-left left)) box
    (if (slot-boundp box :left)
        stored-left
      (if (not (slot-boundp box :parent))
          (setq stored-left 0)
        (with-slots (parent x-order y-order) box
          (let* ((left (+ 1
                          org-real-padding-x
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
                (setq stored-left (+ (org-real--get-left directly-left)
                                     (org-real--get-width directly-left)
                                     org-real-margin-x))
              (with-slots (rel rel-box) box
                (if (and (slot-boundp box :rel)
                         (or (string= "above" rel)
                             (string= "below" rel)))
                    (setq stored-left (org-real--get-left rel-box))
                  (setq stored-left left))))))))))

;;;; Private class methods

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
           (cur-on-top on-top)
           (cur-in-front in-front))
          box
        (with-slots
            ((prev-x x-order)
             (prev-y y-order)
             (prev-behind behind)
             (prev-on-top on-top)
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
                ((string= rel "on top of")
                 (setq cur-x prev-x)
                 (setq cur-y -9999)
                 (setq cur-behind prev-behind)
                 (setq cur-on-top t))
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
                 (setq cur-on-top prev-on-top)
                 (setq cur-in-front prev-in-front))
                ((string= rel "to the right of")
                 (setq cur-x (+ 1 prev-x))
                 (setq cur-y prev-y)
                 (setq cur-behind prev-behind)
                 (setq cur-on-top prev-on-top)
                 (setq cur-in-front prev-in-front))))))

    (if (and prev (member rel '("in" "on" "behind" "in front of" "on top of")))
        (progn
          (oset box :parent prev)
          (with-slots (children) prev
            (setq children (org-real--push children box)))
          (if containers
              (org-real--make-instance-helper containers prev box)
            (oset box :primary t)))
      (oset box :parent parent)
      (with-slots (children) parent
        (setq children (org-real--push children box)))
      (if containers
          (org-real--make-instance-helper containers parent box)
        (oset box :primary t)))))

(cl-defmethod org-real--make-dirty (box)
  "Clear all TOP LEFT WIDTH and HEIGHT coordinates from BOX and its children."
  (if (slot-boundp box :top) (slot-makeunbound box :top))
  (if (slot-boundp box :left) (slot-makeunbound box :left))
  (if (slot-boundp box :width) (slot-makeunbound box :width))
  (if (slot-boundp box :height) (slot-makeunbound box :height))
  (with-slots (children) box
    (mapc 'org-real--make-dirty (org-real--get-all children))))

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
            (string= (with-slots (name) (with-slots (rel-box) relative rel-box) name)
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
  (with-slots (primary) box
    (oset match :primary primary))
  (with-slots
      (children
       parent
       (match-y y-order)
       (match-x x-order)
       (match-behind behind)
       (match-in-front in-front)
       (match-on-top on-top))
      match
    (with-slots ((siblings children)) parent
      (let ((next-boxes (org-real--next box)))
        (mapc
         (lambda (next)
           (with-slots
               (rel
                (next-y y-order)
                (next-x x-order)
                (next-behind behind)
                (next-in-front in-front)
                (next-on-top on-top))
               next
             (cond
              ((string= rel "above")
               (setq next-y match-y)
               (mapc
                (lambda (sibling)
                  (with-slots ((sibling-y y-order)) sibling
                    (when (>= sibling-y match-y)
                      (setq sibling-y (+ 1 sibling-y)))))
                (org-real--get-all siblings))
               (setq next-x match-x)
               (setq next-behind match-behind))
              ((string= rel "below")
               (setq next-y (+ 1 match-y))
               (mapc
                (lambda (sibling)
                  (with-slots ((sibling-y y-order)) sibling
                    (when (> sibling-y match-y)
                      (setq sibling-y (+ 1 sibling-y)))))
                (org-real--get-all siblings))
               (setq next-x match-x)
               (setq next-behind match-behind))
              ((string= rel "on top of")
               (setq next-x (+ 1
                               (apply 'max 0
                                      (mapcar
                                       (lambda (child) (with-slots (x-order) child x-order))
                                       (seq-filter
                                        (lambda (child) (with-slots (on-top) child on-top))
                                        (org-real--get-all children))))))
               (setq next-behind match-behind))
              ((string= rel "to the right of")
               (setq next-x (+ 1 match-x))
               (mapc
                (lambda (sibling)
                  (with-slots ((sibling-y y-order) (sibling-x x-order)) sibling
                    (when (and (= sibling-y match-y)
                               (> sibling-x match-x))
                      (setq sibling-x (+ 1 sibling-x)))))
                (org-real--get-all siblings))
               (setq next-y match-y)
               (setq next-behind match-behind)
               (setq next-in-front match-in-front)
               (setq next-on-top match-on-top))
              ((string= rel "to the left of")
               (setq next-x match-x)
               (setq next-y match-y)
               (mapc
                (lambda (sibling)
                  (with-slots ((sibling-y y-order) (sibling-x x-order)) sibling
                    (when (and (= sibling-y match-y)
                               (>= sibling-x match-x))
                      (setq sibling-x (+ 1 sibling-x)))))
                (org-real--get-all siblings))
               (setq next-behind match-behind)
               (setq next-in-front match-in-front)
               (setq next-on-top match-on-top)))
             
             (oset next :rel-box match)
             (cond
              ((member rel '("in front of" "on top of"))
               (oset next :parent match)
               (setq children (org-real--push children next)))
              ((member rel '("in" "on" "behind"))
               (org-real--flex-add next match world))
              (t
               (oset next :parent parent)
               (setq siblings (org-real--push siblings next))))
             (org-real--add-matching next next world)))
         next-boxes)))))
  
(cl-defmethod org-real--flex-add ((box org-real-box)
                                  (parent org-real-box)
                                  (world org-real-box))
  "Add BOX to a PARENT box already existing in WORLD.

This function ignores the :rel slot and adds BOX in such a way
that the width of WORLD is kept below 80 characters if possible."
  (with-slots ((siblings children)) parent
    (let* ((all-siblings (seq-filter
                          (lambda (sibling)
                            (with-slots (in-front on-top) sibling
                              (not (or in-front on-top))))
                          (org-real--get-all siblings)))
           (last-sibling (and all-siblings
                              (seq-reduce
                               (lambda (max sibling)
                                 (with-slots ((max-x x-order) (max-y y-order)) max
                                   (with-slots ((sibling-x x-order) (sibling-y y-order)) sibling
                                     (if (> sibling-y max-y)
                                         sibling
                                       (if (and (= max-y sibling-y) (> sibling-x max-x))
                                           sibling
                                         max)))))
                               all-siblings
                               (org-real-box :y-order -9999))))
           (cur-width (org-real--get-width world)))
      (org-real--make-dirty world)
      (oset box :parent parent)
      (setq siblings (org-real--push siblings box))
      (when last-sibling
        (with-slots
            ((last-sibling-y y-order)
             (last-sibling-x x-order))
            last-sibling
          (oset box :y-order last-sibling-y)
          (oset box :x-order (+ 1 last-sibling-x))
          (let ((new-width (org-real--get-width world)))
            (org-real--make-dirty world)
            (when (and (> new-width cur-width) (> new-width 80))
              (oset box :y-order (+ 1 last-sibling-y))
              (oset box :x-order 0))))))))

;;;; Utility expressions

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
    (seq-sort (lambda (a b) (>= (length a) (length b))) container-matrix)))

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


(provide 'org-real)

;;; org-real.el ends here
