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
;;
;; The function `org-real-headlines' will display all headlines in the
;; current org file as an org-real diagram.
;;
;; When in an Org Real mode diagram, the standard movement keys will
;; move by boxes rather than characters.  Each button has the
;; following keys:
;;
;;   RET   - Jump to first occurrence of link.
;;   o     - Open next occurrence of link in other window.
;;             Pressed multiple times, cycle through occurrences.
;;   M-RET - Open all occurrences as separate buffers.
;;             This will split the current window as needed.
;;

;;; Code:

;;;; Requirements

(require 'eieio)
(require 'org-element)
(require 'cl-lib)

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

(defcustom org-real-include-context t
  "Whether to show context when opening a real link."
  :type 'boolean
  :group 'org-real)

(defcustom org-real-flex-width 80
  "When merging links, try to keep width below this."
  :type 'number
  :group 'org-real)

;;;; Faces

(defface org-real-primary
  '((t :background "aquamarine"
       :foreground "black"))
  "Face for the last thing in a real link."
  :group 'org-real)

;;;; Constants & variables

(defconst org-real-prepositions
  '("in" "on" "behind" "in front of" "above" "below" "to the left of" "to the right of" "on top of")
  "List of available prepositions for things.")

(defvar org-real--tab-ring '()
  "List of buffer positions of buttons in an Org Real diagram.")
(make-variable-buffer-local 'org-real--tab-ring)

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

(defun org-real-headlines (max-level)
  "View all org headlines as an org real diagram.

MAX-LEVEL is the maximum level to show headlines for."
  (interactive "P")
  (org-real--pp (org-real--parse-headlines (or max-level 2))))

;;;; Org Real mode

(defun org-real-tab-cycle ()
  "Cycle through buttons in the current Org Real buffer."
  (interactive)
  (if-let ((pos (seq-find (lambda (pos) (> pos (point))) org-real--tab-ring)))
      (goto-char pos)))

(defun org-real-tab-uncycle ()
  "Cycle through buttons in the current Org Real buffer in reverse."
  (interactive)
  (if-let ((pos (seq-find (lambda (pos) (< pos (point))) (reverse org-real--tab-ring))))
      (goto-char pos)))

(defun org-real-tab-cycle-down ()
  "Cycle to the next button on the row below."
  (interactive)
  (let ((col (current-column)))
    (forward-line 1)
    (org-real-tab-cycle)
    (move-to-column col t)
    (let ((pos (point)))
      (goto-char (seq-reduce
                  (lambda (closest p)
                    (if (< (abs (- pos p))
                           (abs (- pos closest)))
                        p
                      closest))
                  org-real--tab-ring
                  1.0e+INF)))))

(defun org-real-tab-cycle-up ()
  "Cycle to the next button on the row above."
  (interactive)
  (let ((col (current-column)))
    (forward-line -1)
    (org-real-tab-uncycle)
    (move-to-column col t)
    (let ((pos (point)))
      (goto-char (seq-reduce
                  (lambda (closest p)
                    (if (< (abs (- pos p))
                           (abs (- pos closest)))
                        p
                      closest))
                  org-real--tab-ring
                  1.0e+INF)))))

(define-derived-mode org-real-mode special-mode
  "Org Real"
  "Mode for viewing an org-real diagram.

The following commands are available:

\\{org-real-mode-map}"
  :group 'org-mode
  (toggle-truncate-lines t))

(mapc
 (lambda (key) (define-key org-real-mode-map (kbd (car key)) (cdr key)))
 '(("TAB"       . org-real-tab-cycle)
   ("<right>"   . org-real-tab-cycle)
   ("C-f"       . org-real-tab-cycle)
   ("M-f"       . org-real-tab-cycle)
   ("f"         . org-real-tab-cycle)
   ("<backtab>" . org-real-tab-uncycle)
   ("<left>"    . org-real-tab-uncycle)
   ("C-b"       . org-real-tab-uncycle)
   ("M-b"       . org-real-tab-uncycle)
   ("b"         . org-real-tab-uncycle)
   ("<up>"      . org-real-tab-cycle-up)
   ("C-p"       . org-real-tab-cycle-up)
   ("p"         . org-real-tab-cycle-up)
   ("<down>"    . org-real-tab-cycle-down)
   ("C-n"       . org-real-tab-cycle-down)
   ("n"         . org-real-tab-cycle-down)))

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
      (org-real-mode)
      (erase-buffer)
      (setq org-real--tab-ring '())
      (if containers (org-real--pp-text containers))
      (let ((offset (- (line-number-at-pos)
                       org-real-margin-y
                       (* 2 org-real-padding-y))))
        (dotimes (_ (+ top height)) (insert (concat (make-string width ?\s) "\n")))
        (org-real--draw box offset)
        (goto-char 0)
        (setq org-real--tab-ring
              (seq-sort '< org-real--tab-ring))))
    (display-buffer buffer `(display-buffer-pop-up-window
                             (window-width . ,width)
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
  (let* ((containers (org-real--parse-url url (point-marker)))
         (box (org-real--make-instance 'org-real-box (copy-tree containers))))
    (if org-real-include-context
        (let* ((primary-name (plist-get (car (reverse containers)) :name))
               (children (mapcar
                          (lambda (containers)
                            (org-real--make-instance 'org-real-box containers t))
                          (seq-filter
                           (lambda (containers)
                             (setq containers (reverse containers))
                             (pop containers)
                             (seq-some
                              (lambda (container)
                                (string= primary-name (plist-get container :name)))
                              containers))
                           (org-real--parse-buffer)))))
          (setq box (org-real--merge (push box children)))))
    (org-real--pp box (copy-tree containers))))

(defun org-real-complete (&optional existing)
  "Complete a real link or edit EXISTING link."
  (let* ((container-matrix (org-real--parse-buffer))
         (containers (if existing
                         (org-real--parse-url existing (point-marker))
                       (org-real--complete-thing "Thing: " container-matrix '()))))
    (catch 'confirm
      (while t
        (org-real--pp (org-real--make-instance 'org-real-box containers) containers)
        (let ((response (read-event "RETURN    - Confirm\nBACKSPACE - Remove context\n+         - Add context")))
          (cond
           ((or (eq response 'return) (eq response 13))
            (throw 'confirm containers))
           ((or (eq response 'backspace) (eq response 127))
            (pop containers)
            (if (= 0 (length containers))
                (setq containers (org-real--complete-thing "Thing: " container-matrix containers))))
           ((eq response ?+)
            (let* ((top (plist-get (car containers) :name))
                   (preposition
                    (completing-read (concat "The " top " is: ") org-real-prepositions nil t))
                   (additional-containers
                    (org-real--complete-thing (concat "The " top " is " preposition " the: ")
                                              container-matrix
                                              containers)))
              (setcar containers (plist-put (car containers) :rel preposition))
              (setq containers (append additional-containers containers))))))))
    (org-real--to-link containers)))

(defun org-real--complete-thing (prompt container-matrix existing)
  "Use `completing-read' with PROMPT to get a list of containers.

CONTAINER-MATRIX is used to generate possible completions.  The
return value is the longest list of containers from the matrix
that contains, as the last element, a container with a name
matching the one returned from `completing-read'.

EXISTING containers will be excluded from the completion."
  (let* ((existing-names (mapcar (lambda (container) (plist-get container :name)) existing))
         (completions (seq-filter
                       (lambda (name) (not (member name existing-names)))
                       (cl-delete-duplicates
                        (mapcar
                         (lambda (container) (plist-get container :name))
                         (apply 'append container-matrix)))))
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
      (let ((new-containers (reverse (org-real--parse-url new-link (point-marker)))))
        (while new-containers
          (let ((primary (plist-get (car new-containers) :name))
                (changes '())
                old-containers)
            (org-element-map (org-element-parse-buffer) 'link
              (lambda (old-link)
                (when (string= (org-element-property :type old-link) "real")
                  (setq old-containers (reverse (org-real--parse-url
                                                 (org-element-property :raw-link old-link)
                                                 (set-marker (point-marker) (org-element-property :begin old-link)))))
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
                                                      new-containers)))))
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
                      (let* ((old-desc (save-excursion
                                         (and (goto-char begin)
                                              (org-in-regexp org-link-bracket-re 1)
                                              (match-end 2)
                                              (match-string-no-properties 2))))
                             (new-link (org-real--link-make-string replace-link old-desc)))
                        (push
                         `(lambda ()
                            (save-excursion
                              (delete-region ,begin ,end)
                              (goto-char ,begin)
                              (insert ,new-link)))
                         changes)))))))
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
            :type boolean)
   (locations :initarg :locations
              :initform '()
              :type list))
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

(cl-defmethod org-real--make-instance ((_ (subclass org-real-box))
                                       containers
                                       &optional skip-primary)
  "Create an instance of `org-real-box' from CONTAINERS.

CONTAINERS is a list of plists containing at least a :name
property and optionally a :rel property.  If SKIP-PRIMARY is
non-nil, skip setting :primary slot on the last box."
  (when-let* ((world (org-real-box))
              (base-container (pop containers))
              (base (org-real-box :name (plist-get base-container :name)
                                  :locations (list (plist-get base-container :loc)))))
    (oset base :parent world)
    (with-slots (children) world
      (setq children (org-real--push children base)))
    (if containers
        (org-real--make-instance-helper containers world base skip-primary)
      (unless skip-primary (oset base :primary t)))
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

OFFSET is the starting line to start insertion.

Adds to list `org-real--tab-ring' the buffer position of each
button drawn."
  (let ((children (with-slots (children) box (org-real--get-all children))))
    (with-slots (name behind in-front on-top (dashed behind) primary locations) box
      (when (slot-boundp box :name)
        (let* ((top (+ offset (org-real--get-top box)))
               (left (org-real--get-left box))
               (width (org-real--get-width box))
               (height (org-real--get-height box))
               (align-bottom (or in-front on-top)))
          (cl-flet* ((draw (coords str)
                           (forward-line (- (car coords) (line-number-at-pos)))
                           (move-to-column (cdr coords) t)
                           (insert str)
                           (delete-char (length str)))
                     (button (coords str &optional primary)
                               (if (not locations) (draw coords str)
                                 (forward-line (- (car coords) (line-number-at-pos)))
                                 (move-to-column (cdr coords) t)
                                 (add-to-list 'org-real--tab-ring (point))
                                 (if primary (put-text-property 0 (length str)
                                                                'face 'org-real-primary str))
                                 (insert-button str
                                                'help-echo "Jump to first occurence"
                                                'keymap (org-real--create-button-keymap box))
                                 (delete-char (length str)))))
            (draw (cons top left)
                  (concat "┌" (make-string (- width 2) (if dashed #x254c #x2500)) "┐"))
            (if align-bottom
                (draw (cons (+ top height) left)
                      (concat "┴" (make-string (- width 2) (if dashed #x254c #x2500)) "┴"))
              (draw (cons (+ top height -1) left)
                    (concat "└" (make-string (- width 2) (if dashed #x254c #x2500)) "┘")))
            (button (cons (+ top 1 org-real-padding-y)
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
                           :test #'(lambda (a b)
                                     (and (slot-boundp a :name)
                                          (slot-boundp b :name)
                                          (string= (with-slots (name) a name)
                                                   (with-slots (name) b name))))))
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

If INCLUDE-ON-TOP is non-nil, also include height on top of box."
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
                      (top (+ on-top-height offset (org-real--get-top parent))))
                 (if-let* ((directly-above (seq-reduce
                                            (lambda (above sibling)
                                              (with-slots ((sibling-y y-order)) sibling
                                                (if (< sibling-y y-order)
                                                    (if above
                                                        (with-slots ((max-y y-order)) (car above)
                                                          (if (> sibling-y max-y)
                                                              (list sibling)
                                                            (if (= sibling-y max-y)
                                                                (push sibling above)
                                                              above)))
                                                      (list sibling))
                                                  above)))
                                            siblings
                                            '()))
                           (above-height (+ org-real-margin-y
                                            (apply 'max
                                                   (mapcar
                                                    'org-real--get-height
                                                    directly-above)))))
                     (setq stored-top (+ on-top-height
                                         (org-real--get-top (car directly-above))
                                         above-height))
                   (setq stored-top top)))))))))

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
                                      (org-real-box :x-order -1.0e+INF)))))
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

(cl-defmethod org-real--make-instance-helper (containers
                                              (parent org-real-box)
                                              (prev org-real-box)
                                              &optional skip-primary)
  "Help create a 3D representation of CONTAINERS.

PREV must already exist in PARENT."
  (let* ((container (pop containers))
         (rel (plist-get container :rel))
         (box (org-real-box
               :name (plist-get container :name)
               :rel (plist-get container :rel)
               :rel-box prev
               :locations (list (plist-get container :loc)))))
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
          (with-slots ((siblings children)) parent
            (let ((row-siblings (seq-filter
                                 (lambda (sibling)
                                   (with-slots (y-order) sibling
                                     (= prev-y y-order)))
                                 (org-real--get-all siblings)))
                  (sibling-y-orders (mapcar
                                     (lambda (sibling) (with-slots (y-order) sibling y-order))
                                     (seq-filter
                                      (lambda (sibling)
                                        (with-slots (in-front on-top) sibling
                                          (not (or in-front on-top))))
                                      (org-real--get-all siblings)))))

              (cond ((or (string= rel "in") (string= rel "on"))
                     (setq cur-behind prev-behind))
                    ((string= rel "behind")
                     (setq cur-behind t))
                    ((string= rel "in front of")
                     (setq cur-y 1.0e+INF)
                     (setq cur-behind prev-behind)
                     (setq cur-in-front t))
                    ((string= rel "on top of")
                     (setq cur-y -1.0e+INF)
                     (setq cur-behind prev-behind)
                     (setq cur-on-top t))
                    ((string= rel "above")
                     (setq cur-x prev-x)
                     (setq cur-y (- (apply 'min 0 sibling-y-orders) 1))
                     (setq cur-behind prev-behind))
                    ((string= rel "below")
                     (setq cur-x prev-x)
                     (setq cur-y (+ 1 (apply 'max 0 sibling-y-orders)))
                     (setq cur-behind prev-behind)
                     (setq cur-in-front prev-in-front))
                    ((string= rel "to the left of")
                     (setq cur-x prev-x)
                     (mapc
                      (lambda (sibling)
                        (with-slots (x-order) sibling
                          (if (>= x-order cur-x)
                              (setq x-order (+ 1 x-order)))))
                      row-siblings)
                     (setq cur-y prev-y)
                     (setq cur-behind prev-behind)
                     (setq cur-on-top prev-on-top)
                     (setq cur-in-front prev-in-front))
                    ((string= rel "to the right of")
                     (setq cur-x (+ 1 prev-x))
                     (mapc
                      (lambda (sibling)
                      (with-slots (x-order) sibling
                        (if (>= x-order cur-x)
                            (setq x-order (+ 1 x-order)))))
                      row-siblings)
                     (setq cur-y prev-y)
                     (setq cur-behind prev-behind)
                     (setq cur-on-top prev-on-top)
                     (setq cur-in-front prev-in-front)))

              (if (and prev (member rel '("in" "on" "behind" "in front of" "on top of")))
                  (progn
                    (oset box :parent prev)
                    (with-slots (children) prev
                      (setq children (org-real--push children box)))
                    (if containers
                        (org-real--make-instance-helper containers prev box skip-primary)
                      (unless skip-primary (oset box :primary t))))
                (oset box :parent parent)
                (with-slots (children) parent
                  (setq children (org-real--push children box)))
                (if containers
                    (org-real--make-instance-helper containers parent box skip-primary)
                  (unless skip-primary (oset box :primary t))))))))))

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
  (if (slot-boundp box :name)
      (apply 'append (list box) (mapcar 'org-real--expand (org-real--next box)))
    (with-slots (children) box
      (apply 'append (mapcar 'org-real--expand (org-real--get-all children))))))

(cl-defmethod org-real--merge-into ((from org-real-box) (to org-real-box))
  "Merge FROM box into TO box."
  (let ((from-boxes (reverse (org-real--expand from)))
        (to-boxes (org-real--expand to)))
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

MATCH is used to set the :rel-box and :parent slots on relatives
of BOX."
  (oset match :primary (or (with-slots (primary) match primary)
                           (with-slots (primary) box primary)))
  (oset match :locations (append (with-slots (locations) match locations)
                                 (with-slots (locations) box locations)))
  (mapc
   (lambda (next)
     (org-real--add-matching-helper next match world))
   (org-real--next box)))

(cl-defmethod org-real--add-matching-helper ((next org-real-box)
                                             (match org-real-box)
                                             (world org-real-box))
  "Helper for `org-real--add-matching'.

When MATCH is found, add relative NEXT into WORLD according to
its relationship to MATCH."
  (with-slots
      (children
       parent
       (match-primary primary)
       (match-y y-order)
       (match-x x-order)
       (match-behind behind)
       (match-in-front in-front)
       (match-on-top on-top))
      match
    (with-slots ((siblings children)) parent
      (with-slots
          (rel
           rel-box
           (next-y y-order)
           (next-x x-order)
           (next-behind behind)
           (next-in-front in-front)
           (next-on-top on-top))
          next
        (let ((next-boxes (org-real--next next))
              (row-siblings (seq-filter
                             (lambda (sibling)
                               (with-slots (y-order) sibling
                                 (= y-order match-y)))
                             (org-real--get-all siblings)))
              (sibling-y-orders (mapcar
                                 (lambda (sibling) (with-slots (y-order) sibling y-order))
                                 (seq-filter
                                  (lambda (sibling)
                                    (with-slots (in-front on-top) sibling
                                      (not (or in-front on-top))))
                                  (org-real--get-all siblings)))))
          (cond
           ((string= rel "to the left of")
            (setq next-x match-x)
            (setq next-y match-y)
            (setq next-behind match-behind)
            (mapc
             (lambda (sibling)
               (with-slots (x-order) sibling
                 (if (>= x-order next-x)
                     (setq x-order (+ 1 x-order)))))
             row-siblings))
           ((string= rel "to the right of")
            (setq next-x (+ 1 match-x))
            (setq next-y match-y)
            (setq next-behind match-behind)
            (mapc
             (lambda (sibling)
               (with-slots (x-order) sibling
                 (if (>= x-order next-x)
                     (setq x-order (+ 1 x-order)))))
             row-siblings))
           ((string= rel "above")
            (setq next-y (- (apply 'min 0 sibling-y-orders) 1))
            (setq next-x match-x)
            (setq next-behind match-behind))
           ((string= rel "below")
            (setq next-y (+ 1 (apply 'max 0 sibling-y-orders)))
            (setq next-x match-x)
            (setq next-behind match-behind))
           ((or next-on-top next-in-front)
            (setq next-x (+ 1 (apply 'max 0
                                     (mapcar
                                      (lambda (child) (with-slots (x-order) child x-order))
                                      (seq-filter
                                       (lambda (child)
                                         (with-slots (in-front on-top) child
                                           (and (eq next-in-front in-front)
                                                (eq next-on-top on-top))))
                                       (org-real--get-all children))))))
            (setq next-behind match-behind)))
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
          (mapc
           (lambda (next-next)
             (org-real--add-matching-helper next-next next world))
           next-boxes))))))

(cl-defmethod org-real--flex-add ((box org-real-box)
                                  (parent org-real-box)
                                  (world org-real-box))
  "Add BOX to a PARENT box already existing in WORLD.

This function ignores the :rel slot and adds BOX in such a way
that the width of WORLD is kept below `org-real-flex-width'
characters if possible."
  (let ((cur-width (org-real--get-width world)))
    (org-real--make-dirty world)
    (with-slots ((siblings children)) parent
      (if-let* ((all-siblings (seq-filter
                               (lambda (sibling)
                                 (with-slots (in-front on-top) sibling
                                   (not (or in-front on-top))))
                               (org-real--get-all siblings)))
                (last-sibling (seq-reduce
                               (lambda (max sibling)
                                 (with-slots ((max-x x-order) (max-y y-order)) max
                                   (with-slots ((sibling-x x-order) (sibling-y y-order)) sibling
                                     (if (> sibling-y max-y)
                                         sibling
                                       (if (and (= max-y sibling-y) (> sibling-x max-x))
                                           sibling
                                         max)))))
                               all-siblings
                               (org-real-box :y-order -1.0e+INF))))
          (with-slots
              ((last-sibling-y y-order)
               (last-sibling-x x-order))
              last-sibling
            (oset box :y-order last-sibling-y)
            (oset box :x-order (+ 1 last-sibling-x))
            (oset box :parent parent)
            (setq siblings (org-real--push siblings box))

            (let ((new-width (org-real--get-width world)))
              (org-real--make-dirty world)
              (when (and (> new-width cur-width) (> new-width org-real-flex-width))
                (oset box :y-order (+ 1 last-sibling-y))
                (oset box :x-order 0))))
        (oset box :parent parent)
        (setq siblings (org-real--push siblings box))))))

(cl-defmethod org-real--add-headline (headline
                                      (parent org-real-box)
                                      (world org-real-box)
                                      max-level)
  "Add HEADLINE to WORLD as a child of PARENT.

If HEADLINE is greater than MAX-LEVEL, exclude it and its
children."
  (let* ((pos (org-element-property :begin headline))
         (level (org-element-property :level headline))
         (rel (or (org-entry-get pos "REL") "in"))
         (box (org-real-box :name (org-element-property :title headline)
                            :rel rel
                            :rel-box parent
                            :parent parent
                            :locations (list (set-marker (point-marker) pos))
                            :in-front (string= rel "in front of")
                            :on-top (string= rel "on top of")
                            :y-order (cond
                                      ((string= rel "in front of") 1.0e+INF)
                                      ((string= rel "on top of") -1.0e+INF)
                                      (t 0))
                            :primary t)))
    (when (<= level max-level)
      (if (= 1 level)
          (org-real--flex-add box parent world)
        (org-real--add-matching-helper box parent world))
      (mapc
       (lambda (h)
         (org-real--add-headline h box world max-level))
       (cddr headline)))))

;;;; Org real mode buttons

(defun org-real--jump-other-window (markers)
  "Jump to location of link in other window.

MARKERS is a list of locations of each button in the buffer."
  (let ((i 0))
    (lambda ()
      (interactive)
      (let* ((marker (nth i markers))
             (buffer (marker-buffer marker))
             (pos (marker-position marker)))
        (save-selected-window
          (switch-to-buffer-other-window buffer)
          (goto-char pos))
        (setq i (mod (+ 1 i) (length markers)))))))

(defun org-real--jump-to (marker)
  "Jump to the first occurrence of a link in the same window.

MARKER is the position of the first occurrence of the link."
  (lambda ()
    (interactive)
    (switch-to-buffer (marker-buffer marker))
    (goto-char (marker-position marker))))

(defun org-real--jump-all (markers)
  "View all occurrences of a link in the same window.

MARKERS is the list of positions of the link."
  (lambda ()
    (interactive)
    (let ((size (/ (window-height) (length markers))))
      (or (<= window-min-height size)
          (error "To many buffers to visit simultaneously"))
      (switch-to-buffer (marker-buffer (car markers)))
      (goto-char (marker-position (car markers)))
      (dolist (marker (cdr markers))
        (select-window (split-window nil size))
        (switch-to-buffer (marker-buffer marker))
        (goto-char (marker-position marker))))))

(cl-defmethod org-real--create-button-keymap ((box org-real-box))
  "Create a keymap for a button in Org Real mode.

BOX is the box the button is being made for."
  (with-slots (locations) box
    (easy-mmode-define-keymap
     (mapcar
      (lambda (key) (cons (kbd (car key)) (cdr key)))
      `(("o"         . ,(org-real--jump-other-window locations))
        ("<mouse-1>" . ,(org-real--jump-to (car locations)))
        ("RET"       . ,(org-real--jump-to (car locations)))
        ("M-RET"     . ,(org-real--jump-all locations)))))))

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

(defun org-real--parse-url (str marker)
  "Parse STR into a list of plists.

Returns a list of plists with a :name property and optionally a
:rel property.  MARKER is the location of the link and will be
set to the :loc slot of each box."
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
                               (container (list :name (car location) :loc marker))
                               (rel (and (string-match "&?rel=\\([^&]*\\)" (cadr location))
                                         (match-string 1 (cadr location)))))
                          (if rel
                              (plist-put container :rel rel)
                            container)))
                      tokens)))
    (push (list :name host :loc marker) containers)))

(defun org-real--parse-buffer ()
  "Parse all real links in the current buffer."
  (let ((container-matrix '()))
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
        (if (string= (org-element-property :type link) "real")
            (add-to-list 'container-matrix
                          (org-real--parse-url
                           (org-element-property :raw-link link)
                           (set-marker (point-marker) (org-element-property :begin link)))
                          t))))
    container-matrix))


(defun org-real--parse-headlines (max-level)
  "Create an org-real-box from the current buffer's headlines.

MAX-LEVEL is the maximum depth of headlines to display."
  (let ((headlines (cddr (org-element-parse-buffer 'headline)))
        (world (org-real-box)))
    (mapc
     (lambda (headline)
        (org-real--add-headline headline world world max-level))
     headlines)
    world))
    

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
