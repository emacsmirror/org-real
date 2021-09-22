;;; org-real.el --- Keep track of real things as org-mode links -*- lexical-binding: t -*-

;; Author: Tyler Grinn <tylergrinn@gmail.com>
;; Version: 0.3.0
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
;; current org file as an org-real diagram.  The relationship between
;; a headline and its parent can be set by using a REL property on the
;; child headline.  Valid values for REL are:
;;
;;   - on top of
;;   - in front of
;;   - behind
;;   - above
;;   - below
;;   - to the right of
;;   - to the left of
;;
;; When in an Org Real mode diagram, the standard movement keys will
;; move by boxes rather than characters.  S-TAB will cycle the
;; visibility of all children.  Each box has the following keys:
;;
;;   TAB   - Cycle visibility of box's children
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

;;;; Patch! 0.2.0 > 0.3.0+
;;;; Will be removed in version 1.0.0+

(unintern 'org-real--add-matching nil)
(unintern 'org-real--flex-add nil)

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

(defcustom org-real-default-visibility 2
  "Default level to display boxes."
  :type 'number
  :group 'org-real)

;;;; Faces

(defface org-real-primary nil
  "Face for the last thing in a real link."
  :group 'org-real)

(face-spec-set
 'org-real-primary
 '((t :foreground "light slate blue"))
 'face-defface-spec)

;;;; Constants & variables

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
     (org-real--parse-buffer)))
   nil nil t))

(defun org-real-headlines ()
  "View all org headlines as an org real diagram.

MAX-LEVEL is the maximum level to show headlines for."
  (interactive)
  (org-real--pp
   (org-real--parse-headlines)
   nil
   'display-buffer-same-window
   t 1 2))

;;;; Org Real mode

(defvar org-real--box-ring '()
  "List of buffer positions of buttons in an Org Real diagram.")
(make-variable-buffer-local 'org-real--box-ring)
(defvar org-real--current-box nil
  "Current box the buffer is displaying.")
(make-variable-buffer-local 'org-real--current-box)
(defvar org-real--current-containers '()
  "Current containers the buffer is displaying.")
(make-variable-buffer-local 'org-real--current-containers)
(defvar org-real--current-offset 0
  "Current offset for the box diagram.")
(make-variable-buffer-local 'org-real--current-offset)
(defvar org-real--visibility org-real-default-visibility
  "Visibility of children in the current org real diagram.")
(make-variable-buffer-local 'org-real--visibility)
(defvar org-real--max-visibility 3
  "Maximum visibility setting allowed when cycling all children.")
(make-variable-buffer-local 'org-real--max-visibility)

(defun org-real-mode-cycle ()
  "Cycle through buttons in the current Org Real buffer."
  (interactive)
  (if-let ((pos (seq-find (lambda (pos) (> pos (point))) org-real--box-ring)))
      (goto-char pos)))

(defun org-real-mode-uncycle ()
  "Cycle through buttons in the current Org Real buffer in reverse."
  (interactive)
  (if-let ((pos (seq-find (lambda (pos) (< pos (point))) (reverse org-real--box-ring))))
      (goto-char pos)))

(defun org-real-mode-cycle-down ()
  "Cycle to the next button on the row below."
  (interactive)
  (let ((col (current-column)))
    (forward-line 1)
    (org-real-mode-cycle)
    (move-to-column col t)
    (let ((pos (point)))
      (goto-char (seq-reduce
                  (lambda (closest p)
                    (if (< (abs (- pos p))
                           (abs (- pos closest)))
                        p
                      closest))
                  org-real--box-ring
                  1.0e+INF)))))

(defun org-real-mode-cycle-up ()
  "Cycle to the next button on the row above."
  (interactive)
  (let ((col (current-column)))
    (forward-line -1)
    (org-real-mode-uncycle)
    (move-to-column col t)
    (let ((pos (point)))
      (goto-char (seq-reduce
                  (lambda (closest p)
                    (if (< (abs (- pos p))
                           (abs (- pos closest)))
                        p
                      closest))
                  org-real--box-ring
                  1.0e+INF)))))

(defun org-real-mode-cycle-visibility ()
  "Cycle visibility on all children in the current buffer."
  (interactive)
  (setq org-real--visibility (mod (+ 1 org-real--visibility)
                                  (+ 1 org-real--max-visibility)))
  (if (= 0 org-real--visibility)
      (setq org-real--visibility 1))
  (cond
   ((= 1 org-real--visibility) (message "OVERVIEW"))
   ((= 2 org-real--visibility) (message "CONTENTS"))
   ((= 3 org-real--visibility) (message "MORE CONTENTS")))
  (org-real--update-visibility org-real--current-box)
  (org-real-mode-redraw))

(defun org-real-mode-redraw ()
  "Redraw `org-real--current-box' in the current buffer."
  (org-real--make-dirty org-real--current-box)
  (org-real--flex-adjust org-real--current-box)
  (let ((width (org-real--get-width org-real--current-box))
        (height (org-real--get-height org-real--current-box t))
        (inhibit-read-only t))
    (erase-buffer)
    (setq org-real--box-ring '())
    (if org-real--current-containers
        (org-real--pp-text org-real--current-containers))
    (setq org-real--current-offset (- (line-number-at-pos)
                                      org-real-margin-y
                                      (* 2 org-real-padding-y)))
    (dotimes (_ height) (insert (concat (make-string width ?\s) "\n")))
    (org-real--draw org-real--current-box)
    (goto-char 0)
    (setq org-real--box-ring
          (seq-sort '< org-real--box-ring))))

(define-derived-mode org-real-mode special-mode
  "Org Real"
  "Mode for viewing an org-real diagram.

The following commands are available:

\\{org-real-mode-map}"
  :group 'org-mode
  (let ((inhibit-message t))
    (toggle-truncate-lines t)))

(mapc
 (lambda (key) (define-key org-real-mode-map (kbd (car key)) (cdr key)))
 '(("TAB"       . org-real-mode-cycle)
   ("<right>"   . org-real-mode-cycle)
   ("C-f"       . org-real-mode-cycle)
   ("M-f"       . org-real-mode-cycle)
   ("f"         . org-real-mode-cycle)
   ("<left>"    . org-real-mode-uncycle)
   ("C-b"       . org-real-mode-uncycle)
   ("M-b"       . org-real-mode-uncycle)
   ("b"         . org-real-mode-uncycle)
   ("<up>"      . org-real-mode-cycle-up)
   ("C-p"       . org-real-mode-cycle-up)
   ("p"         . org-real-mode-cycle-up)
   ("<down>"    . org-real-mode-cycle-down)
   ("C-n"       . org-real-mode-cycle-down)
   ("n"         . org-real-mode-cycle-down)
   ("<backtab>" . org-real-mode-cycle-visibility)))

;;;; Pretty printing

(defun org-real--pp (box
                     &optional
                     containers
                     display-buffer-fn
                     select
                     visibility
                     max-visibility)
  "Pretty print BOX in a popup buffer.

If CONTAINERS is passed in, also pretty print a sentence
describing where BOX is.

DISPLAY-BUFFER-FN is used to display the diagram, by
default `display-buffer-pop-up-window'.

If SELECT is non-nil, select the Org Real window after displaying
it.

VISIBILITY is the initial visibility of children and
MAX-VISIBILITY is the maximum depth to display when cycling
visibility."
  (let ((buffer (get-buffer-create "Org Real")))
    (with-current-buffer buffer
      (org-real-mode)
      (setq org-real--current-box box)
      (setq org-real--current-containers containers)
      (setq org-real--visibility (or visibility org-real-default-visibility))
      (setq org-real--max-visibility (or max-visibility 3))
      (org-real--update-visibility box)
      (org-real-mode-redraw)
      (let* ((width (apply 'max (mapcar 'length (split-string (buffer-string) "\n"))))
             (height (count-lines (point-min) (point-max)))
             (buffer (get-buffer-create "Org Real"))
             (window (or (get-buffer-window buffer)
                         (display-buffer buffer
                                         `(,(or display-buffer-fn
                                                'display-buffer-pop-up-window)
                                           (window-width . ,width)
                                           (window-height . ,height))))))
        (if select (select-window window))))))

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
                             (let ((rel-containers (reverse containers)))
                               (pop rel-containers) ;; Exclude copies of the same thing
                               (seq-some
                                (lambda (rel-container)
                                  (string= primary-name (plist-get rel-container :name)))
                                rel-containers)))
                           (org-real--parse-buffer)))))
          (setq box (org-real--merge (push box children)))))
    (org-real--pp box (copy-tree containers) nil nil 0)))

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
      `((:name ,result :loc ,(point-marker))))))

;;; Hooks and advice

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
   (hidden-children :initarg :hidden-children
                    :initform (org-real-box-collection)
                    :type org-real-box-collection)
   (level :initarg :level
          :initform 0
          :type number)
   (top :initarg :top
        :type number)
   (left :initarg :left
         :type number)
   (width :initarg :width
          :type number)
   (height :initarg :height
           :type number)
   (flex :initarg :flex
         :initform nil
         :type boolean)
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
  "Create an instance of an org real box from CONTAINERS.

CONTAINERS is a list of plists containing at least a :name
property and optionally a :rel property.  If SKIP-PRIMARY is
non-nil, skip setting :primary slot on the last box."
  (when-let* ((world (org-real-box))
              (base-container (pop containers))
              (base (org-real-box :name (plist-get base-container :name)
                                  :level 1
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

(cl-defmethod org-real--update-visibility ((box org-real-box))
  "Update visibility of BOX and all of its children."
  (with-slots (level children hidden-children) box
    (let ((hidden (org-real--get-all hidden-children)))
      (if (or (= 0 org-real--visibility)
              (<= level org-real--visibility))
          (if hidden (cl-rotatef children hidden-children))
        (if (not hidden) (cl-rotatef children hidden-children))))
    (mapc 'org-real--update-visibility (append (org-real--get-all children)
                                               (org-real--get-all hidden-children)))))

;;;; Drawing

(cl-defmethod org-real--draw ((box org-real-box))
  "Insert an ascii drawing of BOX into the current buffer.

OFFSET is the starting line to start insertion.

Adds to list `org-real--box-ring' the buffer position of each
button drawn."
  (let ((children (with-slots (children) box (org-real--get-all children))))
    (with-slots
        (name
         behind
         in-front
         on-top
         (dashed behind)
         primary
         locations
         hidden-children)
        box
      (when (slot-boundp box :name)
        (let* ((top (+ org-real--current-offset (org-real--get-top box)))
               (left (org-real--get-left box))
               (width (org-real--get-width box))
               (height (org-real--get-height box))
               (double (org-real--get-all hidden-children))
               (align-bottom (or in-front on-top)))
          (cl-flet* ((draw (coords str &optional primary)
                           (forward-line (- (car coords) (line-number-at-pos)))
                           (move-to-column (cdr coords) t)
                           (if primary (put-text-property 0 (length str)
                                                          'face 'org-real-primary str))
                           (insert str)
                           (delete-char (length str)))
                     (draw-name (coords str &optional primary)
                                (if (not locations) (draw coords str)
                                  (forward-line (- (car coords) (line-number-at-pos)))
                                  (move-to-column (cdr coords) t)
                                  (add-to-list 'org-real--box-ring (point))
                                  (if primary (put-text-property 0 (length str)
                                                                 'face 'org-real-primary str))
                                  (insert-button str
                                                 'help-echo "Jump to first occurence"
                                                 'keymap (org-real--create-button-keymap box))
                                  (delete-char (length str)))))
            (draw (cons top left)
                  (concat (if double "╔" "┌")
                          (make-string (- width 2) (cond (dashed #x254c)
                                                         (double #x2550)
                                                         (t #x2500)))
                          (if double "╗" "┐")))
            (if align-bottom
                (draw (cons (+ top height) left)
                      (concat (if double "╨" "┴")
                              (make-string (- width 2) (cond (dashed #x254c)
                                                             (t #x2500)))
                              (if double "╨" "┴")))
              (draw (cons (+ top height -1) left)
                    (concat (if double "╚" "└")
                            (make-string (- width 2) (cond (dashed #x254c)
                                                           (double #x2550)
                                                           (t #x2500)))
                            (if double "╝" "┘"))))
            (draw-name (cons (+ top 1 org-real-padding-y)
                             (+ left 1 org-real-padding-x))
                       name
                       primary)
            (let ((r (+ top 1))
                  (c1 left)
                  (c2 (+ left width -1)))
              (dotimes (_ (- height (if align-bottom 1 2)))
                (draw (cons r c1) (cond (dashed "╎")
                                        (double "║")
                                        (t "│")))
                (draw (cons r c2) (cond (dashed "╎")
                                        (double "║")
                                        (t "│")))
                (setq r (+ r 1))))))))
    (mapc 'org-real--draw children)))

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
  "Get the height of any boxes on top of BOX."
  (apply 'max 0
         (mapcar
          'org-real--get-on-top-height-helper
          (seq-filter
           (lambda (child) (with-slots (rel) child (and (slot-boundp child :rel)
                                                        (string= rel "on top of"))))
           (with-slots (children) box (org-real--get-all children))))))

(cl-defmethod org-real--get-on-top-height-helper ((child org-real-box))
  "Get the height of any boxes on top of CHILD, including child."
  (with-slots (children rel) child
    (+
     (org-real--get-height child)
     (apply 'max 0
            (mapcar
             'org-real--get-on-top-height-helper
             (seq-filter
              (lambda (grandchild)
                (with-slots ((grandchild-rel rel)) grandchild
                  (and (slot-boundp grandchild :rel)
                       (string= "on top of" grandchild-rel))))
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
            (let* ((row-indices (cl-delete-duplicates
                                 (mapcar
                                  (lambda (child) (with-slots (y-order) child y-order))
                                  children)))
                   (children-height (seq-reduce
                                     (lambda (sum row)
                                       (+ sum org-real-padding-y row))
                                     (mapcar
                                      (lambda (r)
                                        (apply 'max 0
                                               (mapcar
                                                (lambda (child) (org-real--get-height child t))
                                                (seq-filter
                                                 (lambda (child)
                                                   (with-slots (y-order) child (= r y-order)))
                                                 children))))
                                      row-indices)
                                     (* -1 org-real-padding-y))))

              (setq stored-height (+ height children-height))
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
                           (above-bottom (+ org-real-margin-y
                                             (apply 'max
                                                    (mapcar
                                                     (lambda (sibling)
                                                       (+ (org-real--get-top sibling)
                                                          (org-real--get-height sibling)))
                                                     directly-above)))))
                     (setq stored-top (+ on-top-height above-bottom))
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
               :locations (list (plist-get container :loc)))))
    (with-slots
        ((cur-x x-order)
         (cur-y y-order)
         (cur-level level)
         (cur-behind behind)
         (cur-on-top on-top)
         (cur-in-front in-front))
        box
        (with-slots
            ((prev-x x-order)
             (prev-y y-order)
             (prev-level level)
             (prev-behind behind)
             (prev-on-top on-top)
             (prev-in-front in-front))
            prev
          (with-slots ((siblings children) (hidden-siblings hidden-children)) parent
            (let (sibling-y-orders row-siblings)
              (cond
               ((or (string= rel "in") (string= rel "on"))
                (setq cur-level (+ 1 prev-level))
                (setq cur-behind prev-behind))
               ((string= rel "behind")
                (setq cur-level (+ 1 prev-level))
                (setq cur-behind t))
               ((string= rel "in front of")
                (setq cur-level (+ 1 prev-level))
                (setq cur-y 1.0e+INF)
                (setq cur-behind prev-behind)
                (setq cur-in-front t))
               ((string= rel "on top of")
                (setq cur-level (+ 1 prev-level))
                (setq cur-y -1.0e+INF)
                (setq cur-behind prev-behind)
                (setq cur-on-top t))
               ((member rel '("above" "below"))
                (setq cur-behind prev-behind)
                (setq cur-x prev-x)
                (cond
                 ((and prev-in-front (string= rel "below"))
                  (while (with-slots (in-front) prev in-front)
                    (setq prev (with-slots (parent) prev parent)))
                  (setq parent (with-slots (parent) prev parent)))
                 ((and prev-on-top (string= rel "above"))
                  (while (with-slots (on-top) prev on-top)
                      (setq prev (with-slots (parent) prev parent)))
                  (setq parent (with-slots (parent) prev parent)))
                 ((and prev-on-top (string= rel "below"))
                  (setq rel "in")
                  (setq prev parent)))
                (setq cur-level (+ 1 (with-slots (level) parent level)))
                (setq sibling-y-orders
                      (with-slots ((siblings children) (hidden-siblings hidden-children)) parent
                        (mapcar
                         (lambda (sibling) (with-slots (y-order) sibling y-order))
                         (seq-filter
                          (lambda (sibling)
                            (with-slots (in-front on-top) sibling
                              (not (or in-front on-top))))
                          (append (org-real--get-all siblings)
                                  (org-real--get-all hidden-siblings))))))
                (if (or prev-on-top (string= rel "above"))
                    (setq cur-y (- (apply 'min 0 sibling-y-orders) 1))
                  (setq cur-y (+ 1 (apply 'max 0 sibling-y-orders)))))
               ((member rel '("to the left of" "to the right of"))
                (setq row-siblings (seq-filter
                                    (lambda (sibling)
                                      (with-slots (y-order) sibling
                                        (= prev-y y-order)))
                                    (append (org-real--get-all siblings)
                                            (org-real--get-all hidden-siblings))))
                (setq cur-level prev-level)
                (setq cur-y prev-y)
                (setq cur-behind prev-behind)
                (setq cur-on-top prev-on-top)
                (setq cur-in-front prev-in-front)
                (if (string= rel "to the left of")
                    (setq cur-x prev-x)
                  (setq cur-x (+ 1 prev-x)))
                (mapc
                 (lambda (sibling)
                   (with-slots (x-order) sibling
                     (if (>= x-order cur-x)
                         (setq x-order (+ 1 x-order)))))
                 row-siblings)))
              (oset box :rel-box prev)
              (oset box :rel rel)
              (if (not (slot-boundp box :name)) (setq cur-level 0))
              (let ((visible (or (= 0 org-real--visibility) (<= cur-level org-real--visibility))))
                (if (and prev (member rel '("in" "on" "behind" "in front of" "on top of")))
                    (progn
                      (oset box :parent prev)
                      (if visible
                          (with-slots (children) prev
                            (setq children (org-real--push children box)))
                        (with-slots (hidden-children) prev
                          (setq hidden-children (org-real--push hidden-children box))))
                    (if containers
                        (org-real--make-instance-helper containers prev box skip-primary)
                      (unless skip-primary (oset box :primary t))))
                  (oset box :parent parent)
                  (if visible
                      (with-slots (children) parent
                        (setq children (org-real--push children box)))
                    (with-slots (hidden-children) parent
                      (setq hidden-children (org-real--push hidden-children box))))
                  (if containers
                      (org-real--make-instance-helper containers parent box skip-primary)
                    (unless skip-primary (oset box :primary t)))))))))))

(cl-defmethod org-real--get-world ((box org-real-box))
  "Get the top most box related to BOX."
  (with-slots (parent) box
    (if (slot-boundp box :parent)
        (org-real--get-world parent)
      box)))

(cl-defmethod org-real--make-dirty (box)
  "Clear all TOP LEFT WIDTH and HEIGHT coordinates from BOX and its children."
  (if (slot-boundp box :top) (slot-makeunbound box :top))
  (if (slot-boundp box :left) (slot-makeunbound box :left))
  (if (slot-boundp box :width) (slot-makeunbound box :width))
  (if (slot-boundp box :height) (slot-makeunbound box :height))
  (with-slots (children hidden-children) box
    (mapc 'org-real--make-dirty (append (org-real--get-all children)
                                        (org-real--get-all hidden-children)))))

(cl-defmethod org-real--next ((box org-real-box) &optional exclude-children)
  "Retrieve any boxes for which the :rel-box slot is BOX.

If EXCLUDE-CHILDREN, only retrieve sibling boxes."
  (let ((relatives (append (if exclude-children '() (with-slots (children hidden-children) box
                                                      (append (org-real--get-all children)
                                                              (org-real--get-all hidden-children))))
                           (if (slot-boundp box :parent)
                                (with-slots
                                    (children hidden-children)
                                    (with-slots (parent) box parent)
                                  (append (org-real--get-all children)
                                          (org-real--get-all hidden-children)))
                             '()))))
    (seq-filter
     (lambda (relative)
       (with-slots (rel-box) relative
         (and (slot-boundp relative :rel-box)
              (string= (with-slots (name) rel-box name)
                       (with-slots (name) box name)))))
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
                    (org-real--add-matching from-box to-box)
                    t))
                  to-boxes))
             from-boxes)
      (let ((all-from-children (with-slots (children hidden-children) from
                                 (append (org-real--get-all children)
                                         (org-real--get-all hidden-children)))))
        (with-slots ((to-children children) (to-behind behind)) to
          (if (= 1 (length all-from-children))
              (org-real--flex-add (car all-from-children) to)
            (org-real--flex-add from to)))))))

(cl-defmethod org-real--add-matching ((box org-real-box)
                                      (match org-real-box))
  "Add relatives to BOX to MATCH.

MATCH is used to set the :rel-box and :parent slots on relatives
of BOX."
  (oset match :primary (or (with-slots (primary) match primary)
                           (with-slots (primary) box primary)))
  (oset match :locations (append (with-slots (locations) match locations)
                                 (with-slots (locations) box locations)))
  (mapc
   (lambda (next)
     (org-real--add-next next match))
   (org-real--next box)))

(cl-defmethod org-real--add-next ((next org-real-box)
                                  (prev org-real-box))
  "Add NEXT to world according to its relationship to PREV."
  (with-slots
      (children
       hidden-children
       parent
       (prev-level level)
       (prev-primary primary)
       (prev-y y-order)
       (prev-x x-order)
       (prev-behind behind)
       (prev-in-front in-front)
       (prev-on-top on-top))
      prev
    (with-slots ((siblings children) (hidden-siblings hidden-children)) parent
      (with-slots
          (rel
           rel-box
           (next-level level)
           (next-y y-order)
           (next-x x-order)
           (next-behind behind)
           (next-in-front in-front)
           (next-on-top on-top))
          next
        (let* ((next-boxes (org-real--next next))
               (all-siblings (append (org-real--get-all siblings)
                                     (org-real--get-all hidden-siblings)))
               (row-siblings (seq-filter
                              (lambda (sibling)
                                (with-slots (y-order) sibling
                                  (= y-order prev-y)))
                              all-siblings))
               (sibling-y-orders (mapcar
                                  (lambda (sibling) (with-slots (y-order) sibling y-order))
                                  (seq-filter
                                   (lambda (sibling)
                                     (with-slots (in-front on-top) sibling
                                       (not (or in-front on-top))))
                                   all-siblings))))
          (cond
           ((string= rel "to the left of")
            (setq next-level prev-level)
            (setq next-x prev-x)
            (setq next-y prev-y)
            (setq next-behind prev-behind)
            (mapc
             (lambda (sibling)
               (with-slots (x-order) sibling
                 (if (>= x-order next-x)
                     (setq x-order (+ 1 x-order)))))
             row-siblings))
           ((string= rel "to the right of")
            (setq next-level prev-level)
            (setq next-x (+ 1 prev-x))
            (setq next-y prev-y)
            (setq next-behind prev-behind)
            (mapc
             (lambda (sibling)
               (with-slots (x-order) sibling
                 (if (>= x-order next-x)
                     (setq x-order (+ 1 x-order)))))
             row-siblings))
           ((string= rel "above")
            (setq next-level prev-level)
            (setq next-y (- (apply 'min 0 sibling-y-orders) 1))
            (setq next-x prev-x)
            (setq next-behind prev-behind))
           ((string= rel "below")
            (setq next-level prev-level)
            (setq next-y (+ 1 (apply 'max 0 sibling-y-orders)))
            (setq next-x prev-x)
            (setq next-behind prev-behind))
           ((or next-on-top next-in-front)
            (setq next-level (+ 1 prev-level))
            (setq next-x (+ 1 (apply 'max 0
                                     (mapcar
                                      (lambda (child) (with-slots (x-order) child x-order))
                                      (seq-filter
                                       (lambda (child)
                                         (with-slots (in-front on-top) child
                                           (and (eq next-in-front in-front)
                                                (eq next-on-top on-top))))
                                       (append (org-real--get-all children)
                                               (org-real--get-all hidden-children)))))))
            (setq next-behind prev-behind))
           ((member rel '("in" "on" "behind"))
            (setq next-level (+ 1 prev-level))
            (setq next-behind prev-behind)))
          (if (not (slot-boundp next :name)) (setq next-level 0))
          (oset next :rel-box prev)
          (let ((visible (or (= 0 org-real--visibility) (<= next-level org-real--visibility))))
            (cond
             ((member rel '("in front of" "on top of"))
              (oset next :parent prev)
              (if visible
                  (setq children (org-real--push children next))
                (setq hidden-children (org-real--push hidden-children next))))
              ((member rel '("in" "on" "behind"))
               (org-real--flex-add next prev))
              (t
               (oset next :parent parent)
               (if visible
                   (setq siblings (org-real--push siblings next))
                 (setq hidden-siblings (org-real--push hidden-siblings next))))))
          (mapc
           (lambda (next-next)
             (org-real--add-next next-next next))
           next-boxes))))))

(cl-defmethod org-real--flex-add ((box org-real-box)
                                  (parent org-real-box))
  "Add BOX to a PARENT box flexibly.

This function ignores the :rel slot and adds BOX in such a way
that the width of the world is kept below `org-real-flex-width'
characters if possible."
  (let* ((world (org-real--get-world parent))
         (cur-width (org-real--get-width world)))
    (org-real--make-dirty world)
    (with-slots
        ((siblings children)
         (hidden-siblings hidden-children)
         (parent-level level)
         (parent-behind behind))
        parent
      (let* ((level (+ 1 parent-level))
             (visible (or (= 0 org-real--visibility) (<= level org-real--visibility)))
             (all-siblings (seq-filter
                            (lambda (sibling)
                              (with-slots (in-front on-top) sibling
                                (not (or in-front on-top))))
                            (append (org-real--get-all siblings)
                                    (org-real--get-all hidden-siblings))))
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
                                 (org-real-box :y-order -1.0e+INF)))))
        (oset box :flex t)
        (oset box :parent parent)
        (oset box :behind parent-behind)
        (org-real--apply-level box level)
        (if visible
            (setq siblings (org-real--push siblings box))
          (setq hidden-siblings (org-real--push hidden-siblings box)))
        (when last-sibling
          (with-slots
              ((last-sibling-y y-order)
               (last-sibling-x x-order))
              last-sibling
            (oset box :y-order last-sibling-y)
            (oset box :x-order (+ 1 last-sibling-x))
            (let ((new-width (org-real--get-width world)))
              (org-real--make-dirty world)
              (when (and (> new-width cur-width) (> new-width org-real-flex-width))
                (oset box :y-order (+ 1 last-sibling-y))
                (oset box :x-order 0)))))))))

(cl-defmethod org-real--flex-adjust ((box org-real-box))
  "Adjust BOX x and y orders to try to fit world within `org-real-flex-width'."
  (let ((cur-width (org-real--get-width box))
        new-width)
    (org-real--flex-adjust-helper box)
    (setq new-width (org-real--get-width box))
    (while (and (< new-width cur-width)
                (> new-width org-real-flex-width))
      (setq cur-width new-width)
      (org-real--flex-adjust-helper box)
      (setq new-width (org-real--get-width box)))))

(cl-defmethod org-real--flex-adjust-helper ((box org-real-box))
  "Adjust BOX x and y orders to try to fit world within `org-real-flex-width'."
  (with-slots (children flex parent) box
    (when flex
      (let* ((world (org-real--get-world box))
             (cur-width (org-real--get-width world)))
        (when (> cur-width org-real-flex-width)
          (let ((left (org-real--get-left box))
                (width (org-real--get-width box)))
            (when (> (+ left width) org-real-flex-width)
              (with-slots ((siblings children) (hidden-siblings hidden-children)) parent
                (org-real--make-dirty world)
                (when-let* ((all-siblings (seq-filter
                                           (lambda (sibling)
                                             (with-slots (in-front on-top) sibling
                                               (not (or in-front on-top))))
                                           (append (org-real--get-all siblings)
                                                   (org-real--get-all hidden-siblings))))
                            (last-sibling (seq-reduce
                                           (lambda (max sibling)
                                             (with-slots ((max-x x-order) (max-y y-order)) max
                                               (with-slots
                                                   ((sibling-x x-order)
                                                    (sibling-y y-order))
                                                   sibling
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
                    (let ((when-last (org-real--get-width world)))
                      (when (> when-last org-real-flex-width)
                        (org-real--make-dirty world)
                        (oset box :y-order (+ 1 last-sibling-y))
                        (oset box :x-order 0)
                        (let ((when-new-row (org-real--get-width world)))
                          (when (>= when-new-row when-last)
                            (org-real--make-dirty world)
                            (oset box :y-order last-sibling-y)
                            (oset box :x-order (+ 1 last-sibling-x))))))))))))))
    (mapc 'org-real--flex-adjust (org-real--get-all children))))

(cl-defmethod org-real--apply-level ((box org-real-box) level)
  "Apply LEVEL to BOX and update all of its children."
  (oset box :level level)
  (with-slots (children hidden-children) box
    (mapc
     (lambda (child) (org-real--apply-level child (+ 1 level)))
     (append (org-real--get-all children)
             (org-real--get-all hidden-children)))))

(cl-defmethod org-real--add-headline (headline
                                      (parent org-real-box))
  "Add HEADLINE to world as a child of PARENT."
  (let* ((pos (org-element-property :begin headline))
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
    (if (= 1 (with-slots (level) parent level))
        (org-real--flex-add box parent)
      (org-real--add-next box parent))
    (mapc
     (lambda (h)
       (org-real--add-headline h box))
     (cddr headline))))

(cl-defmethod org-real--cycle-children ((box org-real-box))
  "Cycle visibility of children of BOX."
  (lambda ()
    (interactive)
    (with-slots (children hidden-children) box
      (cl-rotatef children hidden-children))
    (org-real-mode-redraw)
    (let ((top (org-real--get-top box))
          (left (org-real--get-left box)))
      (forward-line (- (+ org-real--current-offset top 1 org-real-padding-y)
                       (line-number-at-pos)))
      (move-to-column (+ left 1 org-real-padding-x)))))

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
  (let ((buffer (marker-buffer marker)))
    (lambda ()
      (interactive)
      (if-let ((window (get-buffer-window buffer)))
          (select-window window)
        (switch-to-buffer buffer))
      (goto-char (marker-position marker)))))

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
      `(("TAB"       . ,(org-real--cycle-children box))
        ("o"         . ,(org-real--jump-other-window locations))
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


(defun org-real--parse-headlines ()
  "Create an org real box from the current buffer's headlines."
  (let ((headlines (cddr (org-element-parse-buffer 'headline)))
        (world (org-real-box :level 1)))
    (mapc
     (lambda (headline)
        (org-real--add-headline headline world))
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
