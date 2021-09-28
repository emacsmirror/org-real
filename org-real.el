;;; org-real.el --- Keep track of real things as org-mode links -*- lexical-binding: t -*-

;; Author: Tyler Grinn <tylergrinn@gmail.com>
;; Version: 0.4.2
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
;;   The tooltip in `org-real-headlines' shows the values for each row
;;   in `org-columns' and can be customized the same way as org
;;   columns view.
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
;;   r     - Jump to the box directly related to the current box.
;;             Repeated presses will eventually take you to the
;;             top level box.
;;

;;; Code:

;;;; Requirements

(require 'eieio)
(require 'org-element)
(require 'org-colview)
(require 'cl-lib)
(require 'ispell)

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

;;;; Patch! 0.3.0 > 0.3.1+
;;;; Will be removed in version 1.0.0+

(and (fboundp 'org-real--apply) (advice-remove 'org-insert-link #'org-real--apply))
(and (fboundp 'org-real--maybe-edit-link) (advice-remove 'org-insert-link #'org-real--maybe-edit-link))

;;;; Patch! 0.3.2 > 0.4.0+
;;;; Will be removed in version 1.0.0+

(and (fboundp 'org-real--jump-other-window) (fmakunbound 'org-real--jump-other-window))
(and (fboundp 'org-real--jump-to) (fmakunbound 'org-real--jump-to))
(and (fboundp 'org-real--jump-all) (fmakunbound 'org-real--jump-all))

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

(defcustom org-real-tooltips t
  "Show tooltips in an org real diagram."
  :type 'boolean
  :group 'org-real)

(defcustom org-real-tooltip-timeout 0.5
  "Idle time before showing tooltip in org real diagram."
  :type 'number
  :group 'org-real)

(defcustom org-real-tooltip-max-width 30
  "Maximum width of all tooltips."
  :type 'number
  :group 'org-real)

;;;; Faces

(defface org-real-default nil
  "Default face used in Org Real mode."
  :group 'org-real)

(defface org-real-primary nil
  "Face for the last thing in a real link."
  :group 'org-real)

(face-spec-set
 'org-real-primary
 '((t :foreground "light slate blue"))
 'face-defface-spec)

(defface org-real-selected nil
  "Face for the current box under cursor."
  :group 'org-real)

(face-spec-set
 'org-real-selected
 '((t :foreground "light slate blue"))
 'face-defface-spec)

(defface org-real-rel nil
  "Face for the box which is related to the box under the cursor."
  :group 'org-real)

(face-spec-set
 'org-real-rel
 '((t :foreground "hot pink"))
 'face-defface-spec)

(defface org-real-popup nil
  "Face for popups in an Org Real diagram."
  :group 'org-real)

(face-spec-set
 'org-real-popup
 '((((background dark)) (:background "gray30" :foreground "gray"))
   (t (:background "gainsboro" :foreground "dim gray")))
 'face-defface-spec)

;;;; Constants & variables

(defconst org-real-prepositions
  '("in" "on" "behind" "in front of" "above" "below" "to the left of" "to the right of" "on top of")
  "List of available prepositions for things.")

(defconst org-real-children-prepositions
  '("in" "on" "behind" "in front of" "on top of")
  "List of prepositions which are rendered as children.")

(defconst org-real-flex-prepositions
  '("in" "on" "behind")
  "List of prepositions for which boxes are flexibly added to their parent.")

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

(defvar org-real--current-offset-y 0
  "Current offset rows for the box diagram.")
(make-variable-buffer-local 'org-real--current-offset-y)

(defvar org-real--current-offset-x 0
  "Current offset columns for the box diagram.")
(make-variable-buffer-local 'org-real--current-offset-x)

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
  (let ((coords (cons (line-number-at-pos) (current-column))))
    (goto-char (seq-reduce
                (lambda (closest pos)
                  (goto-char pos)
                  (if (<= (line-number-at-pos) (car coords))
                      closest
                    (let* ((pos-coords (cons (line-number-at-pos) (current-column)))
                           (pos-dist (sqrt (+ (expt (- (car pos-coords) (car coords)) 2)
                                              (expt (- (cdr pos-coords) (cdr coords)) 2))))
                           (closest-coords (and (goto-char closest) (cons (line-number-at-pos) (current-column))))
                           (closest-dist (sqrt (+ (expt (- (car closest-coords) (car coords)) 2)
                                                  (expt (- (cdr closest-coords) (cdr coords)) 2)))))
                      (if (< pos-dist closest-dist)
                          pos
                        closest))))
                org-real--box-ring
                (point-max)))))

(defun org-real-mode-cycle-up ()
  "Cycle to the next button on the row above."
  (interactive)
  (let ((coords (cons (line-number-at-pos) (current-column))))
    (goto-char (seq-reduce
                (lambda (closest pos)
                  (goto-char pos)
                  (if (>= (line-number-at-pos) (car coords))
                      closest
                    (let* ((pos-coords (cons (line-number-at-pos) (current-column)))
                           (pos-dist (sqrt (+ (expt (- (car pos-coords) (car coords)) 2)
                                              (expt (- (cdr pos-coords) (cdr coords)) 2))))
                           (closest-coords (and (goto-char closest) (cons (line-number-at-pos) (current-column))))
                           (closest-dist (sqrt (+ (expt (- (car closest-coords) (car coords)) 2)
                                                  (expt (- (cdr closest-coords) (cdr coords)) 2)))))
                      (if (< pos-dist closest-dist)
                          pos
                        closest))))
                org-real--box-ring
                (point-min)))))

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
  (org-real--flex-adjust org-real--current-box org-real--current-box)
  (org-real-mode-redraw))

(defun org-real-mode-redraw ()
  "Redraw `org-real--current-box' in the current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (if org-real--current-containers
        (org-real--pp-text org-real--current-containers))
    (setq org-real--current-offset-y (- (line-number-at-pos)
                                        2
                                        (* 2 org-real-padding-y)))
    (setq org-real--current-offset-x (- 0 1 org-real-padding-x))
    (org-real--draw org-real--current-box)
    (org-real-mode-recalculate-box-ring)
    (goto-char (point-max))
    (insert "\n")
    (goto-char (point-min))))

(defun org-real-mode-recalculate-box-ring ()
  "Recalculate the position of all boxes in `org-real--current-box'."
  (setq org-real--box-ring
        (seq-sort '< (org-real--get-positions org-real--current-box))))

(define-derived-mode org-real-mode special-mode
  "Org Real"
  "Mode for viewing an org-real diagram.

The following commands are available:

\\{org-real-mode-map}"
  :group 'org-mode
  (let ((inhibit-message t))
    (setq indent-tabs-mode nil)
    (cursor-sensor-mode t)
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

;;;; Interactive functions

(defun org-real-world ()
  "View all real links in the current buffer."
  (interactive)
  (let ((link (cond
               ((org-in-regexp org-link-bracket-re 1)
                (match-string-no-properties 1))
               ((org-in-regexp org-link-plain-re)
                (org-unbracket-string "<" ">" (match-string 0)))))
        (world (org-real--merge
                (mapcar
                 (lambda (containers)
                   (org-real--make-instance 'org-real-box containers))
                 (org-real--parse-buffer)))))
    (org-real--pp world nil nil t)
    (if (and link (string= "real" (ignore-errors (url-type (url-generic-parse-url link)))))
        (let ((containers (reverse (org-real--parse-url link)))
              match)
          (while (and containers (or (not match) (not (org-real--is-visible match t))))
            (setq match (org-real--find-matching
                         (org-real-box :name (plist-get (pop containers) :name))
                         world)))
          (when match
            (run-with-timer 0 nil (lambda () (org-real--jump-to-box match))))))))

(defun org-real-headlines ()
  "View all org headlines as an org real diagram.

MAX-LEVEL is the maximum level to show headlines for."
  (interactive)
  (let ((path (seq-filter 'identity (append (list (org-entry-get nil "ITEM")) (reverse (org-get-outline-path)))))
        (world (save-excursion (org-real--parse-headlines)))
        match)
    (org-real--pp world nil 'display-buffer-same-window t 1 2)
    (while (and path (or (not match) (not (org-real--is-visible match t))))
      (setq match (org-real--find-matching (org-real-box :name (pop path)) world)))
    (when match
      (run-with-timer 0 nil (lambda () (org-real--jump-to-box match))))))

(defun org-real-apply ()
  "Apply any change from the real link at point to the current buffer."
  (interactive)
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
  (if-let ((buffer (get-buffer "Org Real")))
      (kill-buffer buffer))
  (let ((buffer (get-buffer-create "Org Real")))
    (with-current-buffer buffer
      (org-real-mode)
      (setq org-real--current-box box)
      (setq org-real--current-containers containers)
      (setq org-real--visibility (or visibility org-real-default-visibility))
      (setq org-real--max-visibility (or max-visibility 3))
      (org-real--update-visibility box)
      (org-real--flex-adjust box box)
      (org-real-mode-redraw)
      (let* ((width (apply 'max (mapcar 'length (split-string (buffer-string) "\n"))))
             (height (count-lines (point-min) (point-max)))
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
    (dotimes (_ org-real-margin-y) (insert "\n"))
    (insert (make-string org-real-margin-x ?\s))
    (insert "The ")
    (put-text-property 0 (length primary-name) 'face 'org-real-primary
                       primary-name)
    (insert primary-name)
    (if reversed (insert (if (org-real--is-plural primary-name) " are" " is")))
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
               (container-matrix (seq-filter
                                  (lambda (containers)
                                    (let ((rel-containers (reverse containers)))
                                      (pop rel-containers) ;; Exclude copies of the same thing
                                      (seq-some
                                       (lambda (rel-container)
                                         (string= primary-name (plist-get rel-container :name)))
                                       rel-containers)))
                                  (org-real--parse-buffer)))
               (context-boxes (mapcar
                               (lambda (containers)
                                 (org-real--make-instance 'org-real-box containers t))
                               container-matrix)))
          (mapc
           (lambda (context) (org-real--merge-into context box))
           context-boxes)))
    (org-real--pp box (copy-tree containers) nil nil 0)))

(defun org-real-complete (&optional existing)
  "Complete a real link or edit EXISTING link."
  (let* ((container-matrix (org-real--parse-buffer))
         (containers (if existing
                         (org-real--parse-url existing (point-marker))
                       (org-real--complete-thing "Thing: " container-matrix '()))))
    (catch 'confirm
      (while t
        (org-real--pp (org-real--make-instance 'org-real-box containers) containers nil nil 0)
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

;;; Advice

(defun org-real--read-string-advice (orig prompt link &rest args)
  "Advise `read-string' during `org-insert-link' to use custom completion.

ORIG is `read-string', PROMPT and LINK and ARGS are the arguments
passed to it."
  (if (string= "real" (ignore-errors (url-type (url-generic-parse-url link))))
      (org-real-complete link)
    (apply orig prompt link args)))

(defun org-real--insert-link-advice (orig &rest args)
  "Advise `org-insert-link' to advise `read-string' during editing of a link.

ORIG is `org-insert-link', ARGS are the arguments passed to it."
  (advice-add 'read-string :around #'org-real--read-string-advice)
  (let* ((old-desc-fn org-link-make-description-function)
         (org-link-make-description-function (lambda (link desc)
                                               (cond
                                                (old-desc-fn (funcall old-desc-fn link desc))
                                                (desc)
                                                ((string= "real"
                                                          (ignore-errors
                                                            (url-type
                                                             (url-generic-parse-url link))))
                                                 (plist-get (car (last (org-real--parse-url link)))
                                                            :name))))))
    (unwind-protect
        (if (called-interactively-p 'any)
            (call-interactively orig)
          (apply orig args))
      (advice-remove 'read-string #'org-real--read-string-advice)))
  (org-real-apply))

(advice-add 'org-insert-link :around #'org-real--insert-link-advice)

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
   (primary :initarg :primary
            :initform nil
            :type boolean)
   (locations :initarg :locations
              :initform '()
              :type list)
   (metadata :initarg :metadata
             :type string)
   (help-echo :initarg :help-echo
             :type string)
   (rel-box :initarg :rel-box
            :type org-real-box)
   (display-rel :initarg :display-rel
                :type string)
   (display-rel-box :initarg :display-rel-box
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
   (expand-siblings :initarg :expand-siblings
                    :type function)
   (expand-children :initarg :expand-children
                    :type function)
   (extra-data :initarg :extra-data)
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

(cl-defmethod org-real--merge-into ((from org-real-box) (to org-real-box))
  "Merge FROM box into TO box."
  (let (match-found)
    (mapc
     (lambda (from-box)
       (let ((match (org-real--find-matching from-box to)))
         (while (and (not match) (slot-boundp from-box :rel-box))
           (setq from-box (with-slots (rel-box) from-box rel-box))
           (setq match (org-real--find-matching from-box to)))
         (when match
           (setq match-found t)
           (org-real--add-matching from-box match))))
     (org-real--primary-boxes from))
    (unless match-found
      (let ((all-from-children (org-real--get-children from 'all)))
        (if (= 1 (length all-from-children))
            (progn
              (oset (car all-from-children) :flex t)
              (org-real--add-child to (car all-from-children)))
          (oset from :flex t)
          (org-real--add-child to from))))))

(cl-defmethod org-real--update-visibility ((box org-real-box))
  "Update visibility of BOX and all of its children."
  (with-slots (level children hidden-children expand-children) box
    (if (not (org-real--is-visible box))
        (if (not (org-real--get-all hidden-children)) (cl-rotatef children hidden-children))
      (when (slot-boundp box :expand-children)
        (funcall expand-children box)
        (slot-makeunbound box :expand-children))
      (if (org-real--get-all hidden-children)
          (cl-rotatef children hidden-children))
      (let (fully-expanded)
        (while (not fully-expanded)
          (setq fully-expanded t)
          (mapc
           (lambda (child)
             (with-slots (expand-siblings) child
               (when (slot-boundp child :expand-siblings)
                 (funcall expand-siblings child)
                 (slot-makeunbound child :expand-siblings)
                 (setq fully-expanded nil))))
           (org-real--get-all children))))))
  (mapc 'org-real--update-visibility (org-real--get-children box 'all)))

(cl-defmethod org-real--is-visible ((box org-real-box) &optional calculate)
  "Determine if BOX is visible according to `org-real--visibility'.

If CALCULATE, determine if the box has been expanded manually."
  (if calculate
      (with-slots (parent) box
        (seq-find
         (lambda (sibling) (eq sibling box))
         (org-real--get-children parent)))
    (with-slots (level) box
      (or (= 0 org-real--visibility)
          (<= level org-real--visibility)))))

(cl-defmethod org-real--get-positions ((box org-real-box))
  "Get the buffer position of the names of BOX and its children."
  (if (slot-boundp box :name)
      (progn
        (org-real--jump-to-box box)
        (apply 'append (list (point)) (mapcar 'org-real--get-positions (org-real--get-children box))))
    (apply 'append (mapcar 'org-real--get-positions (org-real--get-children box)))))


(cl-defmethod org-real--jump-to-box ((box org-real-box))
  "Jump cursor to the first character in the label of BOX."
  (let ((top (org-real--get-top box))
        (left (org-real--get-left box)))
    (forward-line (- (+ org-real--current-offset-y top 1 org-real-padding-y)
                     (line-number-at-pos)))
    (move-to-column (+ org-real--current-offset-x left 1 org-real-padding-x))))

(cl-defmethod org-real--find-matching ((search-box org-real-box) (world org-real-box))
  "Find a box in WORLD with a matching name as SEARCH-BOX."
  (when (slot-boundp search-box :name)
    (with-slots ((search-name name)) search-box
      (seq-find
       (lambda (box)
         (and (slot-boundp box :name)
              (string= search-name
                       (with-slots (name) box name))))
       (org-real--expand world)))))

;;;; Drawing

(cl-defmethod org-real--draw ((box org-real-box) &optional arg)
  "Insert an ascii drawing of BOX into the current buffer.

If ARG is non-nil, skip drawing children boxes and only update
text properties on the border.  If ARG is 'selected, draw the
border using the `org-real-selected' face.  If ARG is 'rel, draw
the border using `org-real-rel' face, else use `org-real-default'
face.

Uses `org-real--current-offset-y' and
`org-real--current-offset-x' to determine row and column offsets.

Adds to list `org-real--box-ring' the buffer position of each
button drawn."
  (let (box-coords)
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
        (let* ((top (+ org-real--current-offset-y (org-real--get-top box)))
               (left (+ org-real--current-offset-x (org-real--get-left box)))
               (width (org-real--get-width box))
               (height (org-real--get-height box))
               (double (or (org-real--get-all hidden-children)
                           (slot-boundp box :expand-children)))
               (align-bottom (or in-front on-top)))
          (cl-flet* ((draw (coords str &optional primary)
                           (forward-line (- (car coords) (line-number-at-pos)))
                           (when (< (line-number-at-pos) (car coords))
                             (insert (make-string (- (car coords) (line-number-at-pos)) ?\n)))
                           (move-to-column (cdr coords) t)
                           (if arg
                               (ignore-errors
                                 (put-text-property (point) (+ (length str) (point))
                                                    'face (cond ((eq arg 'selected) 'org-real-selected)
                                                                ((eq arg 'rel) 'org-real-rel)
                                                                (t 'org-real-default))))
                             (put-text-property 0 (length str)
                                                'face (if primary
                                                          'org-real-primary
                                                        'org-real-default)
                                                str)
                             (insert str)
                             (let ((remaining-chars (- (save-excursion (end-of-line) (current-column))
                                                       (current-column))))
                               (delete-char (min (length str) remaining-chars)))))
                     (draw-name (coords str &optional primary)
                                (when (not arg)
                                  (forward-line (- (car coords) (line-number-at-pos)))
                                  (when (< (line-number-at-pos) (car coords))
                                    (insert (make-string (- (car coords) (line-number-at-pos)) ?\n)))
                                  (move-to-column (cdr coords) t)
                                  (setq box-coords coords)
                                  (if primary (put-text-property 0 (length str)
                                                                 'face 'org-real-primary
                                                                 str))
                                  (put-text-property 0 (length str)
                                                     'cursor-sensor-functions
                                                     (list (org-real--create-cursor-function box))
                                                     str)
                                  (insert-button str
                                                 'help-echo "Jump to first occurence"
                                                 'keymap (org-real--create-button-keymap box))
                                  (let ((remaining-chars (- (save-excursion (end-of-line)
                                                                            (current-column))
                                                            (current-column))))
                                    (delete-char (min (length str) remaining-chars))))))
            (draw (cons top left)
                  (concat (cond ((and double dashed) "┏")
                                (double "╔")
                                (t "╭"))
                          (make-string (- width 2) (cond ((and double dashed) #x2505)
                                                         (dashed #x254c)
                                                         (double #x2550)
                                                         (t #x2500)))
                          (cond ((and double dashed) "┓")
                                (double "╗")
                                (t "╮"))))
            (if align-bottom
                (draw (cons (+ top height) left)
                      (concat (cond ((and double dashed) "┸")
                                    (double "╨")
                                    (t "┴"))
                              (make-string (- width 2) (cond (dashed #x254c)
                                                             (t #x2500)))
                              (cond ((and double dashed) "┸")
                                    (double "╨")
                                    (t "┴"))))
              (draw (cons (+ top height -1) left)
                    (concat (cond ((and double dashed) "┗")
                                  (double "╚")
                                  (t "╰"))
                            (make-string (- width 2) (cond ((and double dashed) #x2505)
                                                           (dashed #x254c)
                                                           (double #x2550)
                                                           (t #x2500)))
                            (cond ((and double dashed) "┛")
                                  (double "╝")
                                  (t "╯")))))
            (draw-name (cons (+ top 1 org-real-padding-y)
                             (+ left 1 org-real-padding-x))
                       name
                       primary)
            (let ((r (+ top 1))
                  (c1 left)
                  (c2 (+ left width -1)))
              (dotimes (_ (- height (if align-bottom 1 2)))
                (draw (cons r c1) (cond ((and double dashed) "┇")
                                        (dashed "╎")
                                        (double "║")
                                        (t "│")))
                (draw (cons r c2) (cond ((and double dashed) "┇")
                                        (dashed "╎")
                                        (double "║")
                                        (t "│")))
                (setq r (+ r 1))))))))
    (if arg
        (if box-coords (list box-coords) nil)
      (apply 'append
             (if box-coords (list box-coords) nil)
             (mapcar
              'org-real--draw
              (org-real--get-children box))))))

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
             (children (org-real--get-children box)))
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
           (org-real--get-children box)))))

(cl-defmethod org-real--get-on-top-height-helper ((child org-real-box))
  "Get the height of any boxes on top of CHILD, including child."
  (with-slots (rel) child
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
              (org-real--get-children child)))))))

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
                         (org-real--get-children box))))
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
                                       (+ sum org-real-margin-y row))
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
                                     (* -1 org-real-margin-y))))

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
                 (setq stored-top (+ on-top-height org-real-margin-y))
               (let* ((siblings (seq-filter
                                 (lambda (sibling)
                                   (with-slots (on-top in-front) sibling
                                     (not (or on-top in-front))))
                                 (org-real--get-children parent)))
                      (offset (+ 2 (* 2 org-real-padding-y)))
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
  (with-slots ((stored-left left) parent x-order y-order) box
    (if (slot-boundp box :left)
        stored-left
      (if (not (slot-boundp box :parent))
          (setq stored-left org-real-margin-x)
        (let* ((left (+ 1
                        org-real-padding-x
                        (org-real--get-left parent)))
               (to-the-left (seq-filter
                             (lambda (child)
                               (with-slots ((child-y y-order) (child-x x-order)) child
                                 (and (= y-order child-y)
                                      (< child-x x-order))))
                             (org-real--get-children parent)))
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
                (setq stored-left left)))))))))

;;;; Org real mode buttons

(cl-defmethod org-real--create-cursor-function ((box org-real-box))
  "Create cursor functions for entering and leaving BOX."
  (with-slots (rel rel-box display-rel-box display-rel name metadata help-echo) box
    (let (tooltip-timer)
      (lambda (_window _oldpos dir)
        (let ((inhibit-read-only t))
          (save-excursion
            (if (eq dir 'entered)
                (progn
                  (if (slot-boundp box :help-echo)
                      (message help-echo))
                  (if (slot-boundp box :metadata)
                      (setq tooltip-timer (org-real--tooltip metadata))
                    (if (and (slot-boundp box :name) (slot-boundp box :rel))
                        (with-slots ((rel-name name)) (if (slot-boundp box :display-rel-box)
                                                          display-rel-box
                                                        rel-box)
                          (setq tooltip-timer
                                (org-real--tooltip
                                 (with-temp-buffer
                                   (insert (format (concat "The %s "
                                                           (if (org-real--is-plural name) "are" "is")
                                                           " %s the %s.")
                                                   name
                                                   (if (slot-boundp box :display-rel)
                                                       display-rel
                                                     rel)
                                                   rel-name))
                                   (let ((fill-column org-real-tooltip-max-width))
                                     (fill-paragraph t))
                                   (buffer-string)))))))
                  (if (slot-boundp box :display-rel-box)
                      (if (org-real--is-visible display-rel-box t)
                          (org-real--draw display-rel-box 'rel))
                    (if (and (slot-boundp box :rel-box)
                             (org-real--is-visible rel-box t))
                        (org-real--draw rel-box 'rel)))
                  (org-real--draw box 'selected))
              (if tooltip-timer (cancel-timer tooltip-timer))
              (if (slot-boundp box :display-rel-box)
                  (if (org-real--is-visible display-rel-box t)
                      (org-real--draw display-rel-box t))
                (if (and (slot-boundp box :rel-box)
                         (org-real--is-visible rel-box t))
                    (org-real--draw rel-box t)))
              (org-real--draw box t))))))))

(cl-defmethod org-real--jump-other-window ((box org-real-box))
  "Jump to location of link for BOX in other window."
  (with-slots (locations) box
    (lambda ()
      (interactive)
      (let ((first (car locations)))
        (object-remove-from-list box :locations first)
        (object-add-to-list box :locations first t))
      (let* ((marker (car locations))
             (buffer (marker-buffer marker))
             (pos (marker-position marker)))
        (save-selected-window
          (switch-to-buffer-other-window buffer)
          (goto-char pos))))))

(cl-defmethod org-real--jump-to ((box org-real-box))
  "Jump to the first occurrence of a link for BOX in the same window."
  (with-slots (locations) box
    (lambda ()
      (interactive)
      (let* ((marker (car locations))
             (buffer (marker-buffer marker))
             (pos (marker-position marker)))
        (if-let ((window (get-buffer-window buffer)))
            (select-window window)
          (switch-to-buffer buffer))
        (goto-char pos)))))

(cl-defmethod org-real--jump-all ((box org-real-box))
  "View all occurrences of links from BOX in the same window."
  (with-slots (locations) box
    (lambda ()
      (interactive)
      (let* ((size (/ (window-height) (length locations)))
             (marker (car locations)))
        (or (<= window-min-height size)
            (error "To many buffers to visit simultaneously"))
        (switch-to-buffer (marker-buffer marker))
        (goto-char (marker-position marker))
        (dolist (marker (cdr locations))
          (select-window (split-window nil size))
          (switch-to-buffer (marker-buffer marker))
          (goto-char (marker-position marker)))))))

(cl-defmethod org-real--jump-rel ((box org-real-box))
  "Jump to the box directly related to BOX."
  (with-slots (rel-box) box
    (if (not (slot-boundp box :rel-box))
        (lambda () (interactive))
      (lambda ()
        (interactive)
        (org-real--jump-to-box box)))))

(cl-defmethod org-real--create-button-keymap ((box org-real-box))
  "Create a keymap for a button in Org Real mode.

BOX is the box the button is being made for."
  (with-slots (locations) box
    (easy-mmode-define-keymap
     (mapcar
      (lambda (key) (cons (kbd (car key)) (cdr key)))
      (append
       `(("TAB"       . ,(org-real--cycle-children box))
         ("r"         . ,(org-real--jump-rel box)))
       (when (and (slot-boundp box :locations) locations)
         `(("o"         . ,(org-real--jump-other-window box))
           ("<mouse-1>" . ,(org-real--jump-to box))
           ("RET"       . ,(org-real--jump-to box))
           ("M-RET"     . ,(org-real--jump-all box)))))))))

;;;; Private class methods

(cl-defmethod org-real--get-children ((box org-real-box) &optional arg)
  "Get all visible children of BOX.

If optional ARG is 'all, include hidden children.

If optional ARG is 'hidden, only return hidden children"
  (with-slots (children hidden-children) box
    (cond
     ((eq 'all arg)
      (append (org-real--get-all children)
              (org-real--get-all hidden-children)))
     ((eq 'hidden arg)
      (org-real--get-all hidden-children))
     (t
      (org-real--get-all children)))))

(cl-defmethod org-real--add-child ((parent org-real-box)
                                   (child org-real-box)
                                   &optional force-visible)
  "Add CHILD to PARENT according to its visibility.

If FORCE-VISIBLE, always make CHILD visible in PARENT."
  (oset child :parent parent)
  (with-slots (children hidden-children) parent
    (if (org-real--get-all hidden-children)
        (progn
          (setq hidden-children (org-real--push hidden-children child))
          (if (or force-visible (org-real--is-visible child))
              (cl-rotatef children hidden-children)))
      (if (or force-visible (org-real--is-visible child))
          (setq children (org-real--push children child))
        (setq hidden-children (org-real--push hidden-children child))))))

(cl-defmethod org-real--get-world ((box org-real-box))
  "Get the top most box related to BOX."
  (with-slots (parent) box
    (if (slot-boundp box :parent)
        (org-real--get-world parent)
      box)))

(cl-defmethod org-real--primary-boxes ((box org-real-box))
  "Get a list of boxes from BOX which have no further relatives."
  (if (slot-boundp box :parent)
      (if-let ((next-boxes (org-real--next box)))
          (apply 'append (mapcar 'org-real--primary-boxes next-boxes))
        (list box))
    (apply 'append (mapcar 'org-real--primary-boxes (org-real--get-children box 'all)))))

(cl-defmethod org-real--expand ((box org-real-box))
  "Get a list of all boxes, including BOX, that are children of BOX."
  (if (slot-boundp box :parent)
      (apply 'append (list box) (mapcar 'org-real--expand (org-real--next box)))
    (apply 'append (mapcar 'org-real--expand (org-real--get-children box 'all)))))

(cl-defmethod org-real--make-dirty ((box org-real-box))
  "Clear all TOP LEFT WIDTH and HEIGHT coordinates from BOX and its children."
  (if (slot-boundp box :top) (slot-makeunbound box :top))
  (if (slot-boundp box :left) (slot-makeunbound box :left))
  (if (slot-boundp box :width) (slot-makeunbound box :width))
  (if (slot-boundp box :height) (slot-makeunbound box :height))
  (mapc 'org-real--make-dirty (org-real--get-children box 'all)))

;; TODO check if `eq' works
(cl-defmethod org-real--next ((box org-real-box) &optional exclude-children)
  "Retrieve any boxes for which the :rel-box slot is BOX.

If EXCLUDE-CHILDREN, only retrieve sibling boxes."
  (let ((relatives (append (if exclude-children '() (org-real--get-children box 'all))
                           (if (slot-boundp box :parent)
                               (with-slots (parent) box
                                 (org-real--get-children parent 'all))
                             '()))))
    (seq-filter
     (lambda (relative)
       (with-slots (rel-box) relative
         (and (slot-boundp relative :rel-box)
              (eq rel-box box))))
     relatives)))

(cl-defmethod org-real--apply-level ((box org-real-box) level)
  "Apply LEVEL to BOX and update all of its children."
  (oset box :level level)
  (mapc
   (lambda (child) (org-real--apply-level child (+ 1 level)))
   (org-real--get-children box 'all)))

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
        ((cur-level level)
         (cur-behind behind)
         (cur-on-top on-top)
         (cur-in-front in-front)
         display-rel
         display-rel-box
         flex)
        box
        (with-slots
            ((prev-level level)
             (prev-behind behind)
             (prev-on-top on-top)
             (prev-in-front in-front))
            prev
          (cond
           ((or (string= rel "in") (string= rel "on"))
            (setq flex t)
            (setq cur-level (+ 1 prev-level))
            (setq cur-behind prev-behind))
           ((string= rel "behind")
            (setq flex t)
            (setq cur-level (+ 1 prev-level))
            (setq cur-behind t))
           ((string= rel "in front of")
            (setq cur-level (+ 1 prev-level))
            (setq cur-behind prev-behind)
            (setq cur-in-front t))
           ((string= rel "on top of")
            (setq cur-level (+ 1 prev-level))
            (setq cur-behind prev-behind)
            (setq cur-on-top t))
           ((member rel '("above" "below"))
            (setq cur-behind prev-behind)
            (cond
             ((and prev-in-front (string= rel "below"))
              (setq display-rel-box prev)
              (while (with-slots (in-front) prev in-front)
                (setq prev (with-slots (parent) prev parent)))
              (setq parent (with-slots (parent) prev parent)))
             ((and prev-on-top (string= rel "above"))
              (setq display-rel-box prev)
              (while (with-slots (on-top) prev on-top)
                (setq prev (with-slots (parent) prev parent)))
              (setq parent (with-slots (parent) prev parent)))
             ((and prev-on-top (string= rel "below"))
              (setq display-rel rel)
              (setq display-rel-box prev)
              (setq rel "in")
              (setq prev parent))))
           ((member rel '("to the left of" "to the right of"))
            (setq cur-level prev-level)
            (setq cur-behind prev-behind)
            (setq cur-on-top prev-on-top)
            (setq cur-in-front prev-in-front)))
          (oset box :rel rel)
          (oset box :rel-box prev)
          (if (member rel org-real-children-prepositions)
              (progn
                (org-real--add-child prev box)
                (if containers
                    (org-real--make-instance-helper containers prev box skip-primary)
                  (unless skip-primary (oset box :primary t))))
            (org-real--add-child parent box)
            (if containers
                (org-real--make-instance-helper containers parent box skip-primary)
              (unless skip-primary (oset box :primary t))))))))

(cl-defmethod org-real--add-matching ((box org-real-box) (match org-real-box))
  "Add relatives of BOX to MATCH."
  (oset match :primary (or (with-slots (primary) match primary)
                           (with-slots (primary) box primary)))
  (oset match :locations (append (with-slots (locations) match locations)
                                 (with-slots (locations) box locations)))
  (mapc
   (lambda (next) (org-real--add-next next match))
   (org-real--next box)))

(cl-defmethod org-real--add-next ((next org-real-box)
                                  (prev org-real-box)
                                  &optional force-visible skip-next)
  "Add NEXT to world according to its relationship to PREV.

If FORCE-VISIBLE, show the box regardless of
`org-real--visibility'

If SKIP-NEXT, don't add expansion slots for boxes related to
NEXT."
  (with-slots
      (children
       hidden-children
       parent
       (prev-level level)
       (prev-primary primary)
       (prev-behind behind)
       (prev-in-front in-front)
       (prev-on-top on-top))
      prev
    (with-slots
        (rel
         rel-box
         extra-data
         flex
         (next-level level)
         (next-behind behind)
         (next-in-front in-front)
         (next-on-top on-top))
        next
      (let* ((next-boxes (org-real--next next))
             (partitioned (seq-group-by
                           (lambda (next-next)
                             (with-slots (rel) next-next
                               (if (member rel org-real-children-prepositions)
                                   'children
                                 'siblings)))
                           next-boxes))
             (children-boxes (alist-get 'children partitioned))
             (sibling-boxes (alist-get 'siblings partitioned)))
        (if-let ((match (org-real--find-matching next prev)))
            (mapc
             (lambda (next-next)
               (org-real--add-next next-next match))
             (org-real--next next))
          (setq extra-data partitioned)
          (cond
           ((member rel '("to the left of" "to the right of"))
            (setq next-level prev-level)
            (setq next-behind prev-behind)
            (setq next-in-front prev-in-front)
            (setq next-on-top prev-on-top))
           ((member rel '("above" "below"))
            (setq next-level prev-level)
            (setq next-behind prev-behind))
           ((or next-on-top next-in-front)
            (setq next-level (+ 1 prev-level))
            (setq next-behind prev-behind))
           ((member rel '("in" "on"))
            (setq flex t)
            (setq next-behind prev-behind)
            (setq next-level (+ 1 prev-level)))
           ((string= rel "behind")
            (setq flex t)
            (setq next-level (+ 1 prev-level))
            (setq next-behind t)))
          (oset next :rel-box prev)
          (if (member rel org-real-children-prepositions)
              (org-real--add-child prev next force-visible)
            (org-real--add-child parent next force-visible))
          (unless skip-next
            (if children-boxes
                (oset next :expand-children
                      '(lambda (box)
                         (mapc
                          (lambda (child) (org-real--add-next child box))
                          (alist-get 'children (oref box :extra-data))))))
            (if sibling-boxes
                (oset next :expand-siblings
                      '(lambda (box)
                         (mapc
                          (lambda (sibling) (org-real--add-next sibling box t))
                          (alist-get 'siblings (oref box :extra-data))))))))))))

(cl-defmethod org-real--position-box ((box org-real-box))
  "Adjust BOX's position."
  (with-slots (rel-box rel parent x-order y-order on-top in-front parent) box
    (with-slots ((rel-y y-order) (rel-x x-order)) rel-box
      (unless (org-real--find-matching box rel-box)
        (if on-top
            (setq y-order -1.0e+INF))
        (if in-front
            (setq y-order 1.0e+INF))
        (cond
         ((member rel '("to the left of" "to the right of"))
          (setq y-order rel-y)
          (if (string= rel "to the left of")
              (setq x-order rel-x)
            (setq x-order (+ 1 rel-x)))
          (let ((row-siblings (seq-filter
                               (lambda (sibling)
                                 (with-slots ((sibling-y y-order)) sibling
                                   (= sibling-y rel-y)))
                               (org-real--get-children parent 'all))))
            (mapc
             (lambda (sibling)
               (with-slots ((sibling-x x-order)) sibling
                 (if (>= sibling-x x-order)
                     (setq sibling-x (+ 1 sibling-x)))))
             row-siblings)))
         ((member rel '("above" "below"))
          (setq x-order rel-x)
          (let ((sibling-y-orders (mapcar
                                   (lambda (sibling) (with-slots (y-order) sibling y-order))
                                   (seq-filter
                                    (lambda (sibling)
                                      (with-slots (in-front on-top) sibling
                                        (not (or in-front on-top))))
                                    (org-real--get-children parent 'all)))))
            (if (string= rel "above")
                (setq y-order (- (apply 'min 0 sibling-y-orders) 1))
              (setq y-order (+ 1 (apply 'max 0 sibling-y-orders))))))
         ((or on-top in-front)
          (setq x-order (+ 1 (apply 'max 0
                                    (mapcar
                                     (lambda (child) (with-slots (x-order) child x-order))
                                     (seq-filter
                                      (lambda (child)
                                        (with-slots ((child-in-front in-front) (child-on-top on-top)) child
                                           (and (eq in-front child-in-front)
                                                (eq on-top child-on-top))))
                                      (org-real--get-children rel-box 'all))))))))
        (org-real--add-child parent box t)))))


(cl-defmethod org-real--flex-add ((box org-real-box)
                                  (parent org-real-box)
                                  (world org-real-box))
  "Add BOX to a PARENT box flexibly.

This function ignores the :rel slot and adds BOX in such a way
that the width of the WORLD is kept below `org-real-flex-width'
characters if possible."
  (let ((cur-width (org-real--get-width world)))
    (org-real--make-dirty world)
    (with-slots ((parent-level level) (parent-behind behind)) parent
      (let* ((level (+ 1 parent-level))
             (all-siblings (seq-filter
                            (lambda (sibling)
                              (with-slots (in-front on-top) sibling
                                (not (or in-front on-top))))
                            (org-real--get-children parent)))
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
        (org-real--apply-level box level)
        (org-real--add-child parent box t)
        (org-real--flex-adjust box world)
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
                (oset box :x-order 0)
                (org-real--flex-adjust box world)))))))))

(cl-defmethod org-real--partition (fn (collection org-real-box-collection))
  "Partition COLLECTION into two collections using predicate FN."
  (if (not (slot-boundp collection :box))
      (list (org-real-box-collection) (org-real-box-collection))
    (let ((pass (org-real-box-collection))
          (fail (org-real-box-collection)))
      (while (slot-boundp collection :box)
        (with-slots (box next) collection
          (if (funcall fn box)
              (setq pass (org-real--push pass box))
            (setq fail (org-real--push fail box)))
          (if (slot-boundp collection :next)
              (setq collection next)
            (setq collection (org-real-box-collection)))))
      (list pass fail))))

(cl-defmethod org-real--flex-adjust ((box org-real-box) (world org-real-box))
  "Adjust BOX x and y orders to try to fit WORLD within `org-real-flex-width'."
  (with-slots (children) box
    (let* ((partitioned (org-real--partition
                         (lambda (child) (with-slots (flex) child flex))
                         children))
           (flex-children (org-real--get-all (car partitioned)))
           (other-children (org-real--get-all (cadr partitioned))))
      (setq children (org-real-box-collection))
      (org-real--make-dirty world)
      (mapc
       (lambda (flex-child)
         (org-real--flex-add flex-child box world))
       flex-children)
      (mapc
       (lambda (other-child)
         (if (not (slot-boundp other-child :rel-box))
             (org-real--flex-add other-child box world)
           (org-real--position-box other-child)
           (org-real--flex-adjust other-child world)))
       other-children))))

(cl-defmethod org-real--add-headline (headline
                                      (parent org-real-box))
  "Add HEADLINE to world as a child of PARENT."
  (with-slots (locations (parent-level level)) parent
    (with-current-buffer (marker-buffer (car locations))
      (let* ((partitioned (seq-group-by
                           (lambda (h)
                             (let ((child-rel (or (org-entry-get
                                                   (org-element-property :begin h)
                                                   "REL")
                                                  "in")))
                               (if (member child-rel org-real-children-prepositions)
                                   'children
                                 'siblings)))
                           (cddr headline)))
             (children (alist-get 'children partitioned))
             (siblings (alist-get 'siblings partitioned))
             (pos (org-element-property :begin headline))
             (columns (save-excursion (goto-char pos) (org-columns--collect-values)))
             (max-column-length (apply 'max 0
                                       (mapcar
                                        (lambda (column)
                                          (length (cadr (car column))))
                                        columns)))
             (rel (save-excursion (goto-char pos) (or (org-entry-get nil "REL") "in")))
             (level (if (member rel org-real-children-prepositions)
                        (+ 1 parent-level)
                      parent-level))
             (name (org-element-property :title headline))
             (box (org-real-box :name (if (string-match org-link-bracket-re name)
                                          (match-string 2 name)
                                        name)
                                :rel rel
                                :level level
                                :rel-box parent
                                :parent parent
                                :metadata (mapconcat
                                           (lambda (column)
                                             (format
                                              (concat "%" (number-to-string max-column-length) "s : %s")
                                              (cadr (car column))
                                              (cadr column)))
                                           columns
                                           "\n")
                                :locations (list (set-marker (point-marker) pos))
                                :in-front (string= rel "in front of")
                                :on-top (string= rel "on top of")
                                :y-order (cond
                                          ((string= rel "in front of") 1.0e+INF)
                                          ((string= rel "on top of") -1.0e+INF)
                                          (t 0))
                                :primary t)))
        (org-real--add-next box parent)
        (oset box :extra-data partitioned)
        (if children
            (oset box :expand-children
                  '(lambda (box)
                     (mapc
                      (lambda (h) (org-real--add-headline h box))
                      (alist-get 'children (oref box :extra-data))))))
        (if siblings
            (oset box :expand-siblings
                  '(lambda (box)
                     (mapc
                      (lambda (h) (org-real--add-headline h box))
                      (alist-get 'siblings (oref box :extra-data))))))))))

(cl-defmethod org-real--cycle-children ((box org-real-box))
  "Cycle visibility of children of BOX."
  (lambda ()
    (interactive)
    (with-slots (children hidden-children expand-children expanded parent) box
      (if (slot-boundp box :expand-children)
          (progn
            (funcall expand-children box)
            (slot-makeunbound box :expand-children)
            (if (org-real--get-all hidden-children)
                (cl-rotatef children hidden-children)))
        (cl-rotatef children hidden-children))
      (let (fully-expanded)
        (while (not fully-expanded)
          (setq fully-expanded t)
          (mapc
           (lambda (child)
             (with-slots (expand-siblings) child
               (when (slot-boundp child :expand-siblings)
                 (setq fully-expanded nil)
                 (funcall expand-siblings child)
                 (slot-makeunbound child :expand-siblings))))
           (org-real--get-all children))))
      (org-real--flex-adjust parent (org-real--get-world parent)))
    (org-real-mode-redraw)
    (org-real--jump-to-box box)))

;;;; Utility expressions

(defun org-real--tooltip (content)
  "Show popup tooltip with CONTENT after `org-real-tooltip-timeout' idle time."
  (when (and org-real-tooltips (not (string-empty-p content)))
    (let ((marker (point-marker)))
      (run-with-idle-timer
       org-real-tooltip-timeout nil
       (lambda ()
         (if (and (eq (marker-buffer marker)
                      (current-buffer))
                  (eq (marker-position marker)
                      (point)))
             (org-real--tooltip-show content)))))))

(defun org-real--tooltip-show (content)
  "Show tooltip with CONTENT at point immediately."
  (let* ((cur-line (line-number-at-pos))
         (cur-column (current-column))
         (min-line (save-excursion
                    (goto-char (window-start))
                    (line-number-at-pos)))
         (max-column (+ (window-hscroll) (window-body-width)))
         (rows (split-string content "\n"))
         (height (length rows))
         (width (+ 2 (min org-real-tooltip-max-width
                          (apply 'max 0 (mapcar 'length rows)))))
         (top (if (< (- cur-line 2 height) min-line)
                  (+ cur-line 2)
                (- cur-line 1 height)))
         (left (if (> (+ cur-column width 1) max-column)
                   (- max-column width 1)
                 cur-column))
         overlay overlays)
    (dolist (str rows)
      (let* ((pos (save-excursion
                    (forward-line (- top (line-number-at-pos)))
                    (let ((inhibit-read-only t))
                      (move-to-column left t))
                    (point)))
             (remaining-chars (save-excursion
                                (goto-char pos)
                                (- (save-excursion
                                     (end-of-line)
                                     (current-column))
                                   (current-column)))))
        (setq str (format
                   (concat " %-" (number-to-string (- width 2)) "s ")
                   (truncate-string-to-width str org-real-tooltip-max-width nil nil t)))
        (when (= 0 remaining-chars)
          (save-excursion (goto-char pos) (let ((inhibit-read-only t)) (insert " ")))
          (setq remaining-chars 1))
        (setq overlay (make-overlay pos (+ pos (min remaining-chars width))))
        (overlay-put overlay 'face 'org-real-popup)
        (overlay-put overlay 'display `((margin nil) ,str))
        (push overlay overlays)
        (setq top (+ top 1))))
    (save-excursion (org-real-mode-recalculate-box-ring))
    (push (read-event nil) unread-command-events)
    (mapc 'delete-overlay overlays)))

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

(defun org-real--parse-url (str &optional marker)
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
                               (rel (or (and (cadr location)
                                             (string-match "&?rel=\\([^&]*\\)" (cadr location))
                                             (match-string 1 (cadr location)))
                                        "in")))
                          (list :name (car location)
                                :loc marker
                                :rel rel)))
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
  (org-columns-get-format)
  (let* ((headlines (cddr (org-element-parse-buffer 'headline)))
         (filename (buffer-file-name))
         (title (or (concat (file-name-base filename) "." (file-name-extension filename))
                    "Document"))
         (world (org-real-box))
         (document (org-real-box :name title
                                 :metadata ""
                                 :locations (list (point-min-marker)))))
    (org-real--flex-add document world world)
    (mapc
     (lambda (headline)
        (org-real--add-headline headline document))
     headlines)
    world))

(defun org-real--to-link (containers)
  "Create a link string from CONTAINERS."
  (concat "real://"
          (mapconcat
           (lambda (container)
             (concat (plist-get container :name)
                     (when-let ((rel (plist-get container :rel)))
                       (if (not (string= "in" rel))
                           (concat "?rel=" (plist-get container :rel))))))
           containers
           "/")))

(defun org-real--is-plural (noun)
  "Determine if any word in NOUN has a base (root) word.

Uses either Ispell, aspell, or hunspell based on user settings."
  (condition-case err
      (progn
        (ispell-set-spellchecker-params)
        (let* ((words (split-string noun))
               (orig-args (ispell-get-ispell-args))
               (args (append
                      (if (and ispell-current-dictionary
                               (not (member "-d" orig-args)))
                          (list "-d" ispell-current-dictionary))
                      orig-args
                      (if ispell-current-personal-dictionary
                          (list "-p" ispell-current-personal-dictionary))
                      (if ispell-encoding8-command
	                        (if ispell-really-hunspell
		                          (list ispell-encoding8-command
			                              (upcase (symbol-name (ispell-get-coding-system))))
		                        (list
		                         (concat ispell-encoding8-command
			                               (symbol-name (ispell-get-coding-system))))))
                      ispell-extra-args))
               (mode (cond (ispell-really-aspell "munch")
                           ((or ispell-really-hunspell
                                (not (not (string-match-p "ispell" ispell-program-name))))
                            "-m")
                           (t (error (concat ispell-program-name " is not supported.")))))
               (program (concat ispell-program-name " " mode " " (string-join args " ")))
               (results (mapcar
                         (lambda (word)
                           (shell-command-to-string (concat "echo " word " | " program)))
                         words)))
          (cond
           (ispell-really-aspell
            (seq-some
             (lambda (result)
               (not (not (string-match-p "/S" result))))
             results))
           (ispell-really-hunspell
            (seq-some
             (lambda (result)
               (not (not (string-match-p "fl:[[:alnum:]]*S[[:alnum:]]*" result))))
             results))
           ((not (not (string-match-p "ispell" ispell-program-name)))
            (seq-some
             (lambda (result)
               (not (not (string-match-p "(derives from root" result))))
             results))
           (t
            (error (concat ispell-program-name " is not supported."))))))
    (error (progn
             (message (error-message-string err))
             nil))))

(provide 'org-real)

;;; org-real.el ends here
