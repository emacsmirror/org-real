;;; org-real.el --- Keep track of real things as org-mode links -*- lexical-binding: t -*-

;; Copyright (C) 2021 Free Software Foundation, Inc.

;; Author: Taylor Grinn <grinntaylor@gmail.com>
;; Version: 1.0.6
;; File: org-real.el
;; Package-Requires: ((emacs "26.1") (boxy "1.0") (org "9.3"))
;; Keywords: tools
;; URL: https://gitlab.com/tygrdev/org-real

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package adds a 'real' type link to org mode to create links to
;; real things.
;;
;; The function `org-real-world' will display all real links in the
;; current buffer.
;;
;; When in a boxy mode diagram, the standard movement keys will move
;; by boxes rather than characters.  S-TAB will cycle the visibility
;; of all children.  Each box has the following keys:
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

;;;###autoload
(require 'ol)

(require 'boxy)
(require 'eieio)
(require 'org-element)
(require 'cl-lib)
(require 'ispell)
(require 'url-parse)
(require 'subr-x)

;;;; Options

(defgroup org-real nil
  "Customization options for org-real."
  :group 'applications)

(defcustom org-real-margin-x 2
  "Horizontal margin to be used when displaying boxes."
  :type 'number)

(defcustom org-real-margin-y 1
  "Vertical margin to be used when displaying boxes."
  :type 'number)

(defcustom org-real-padding-x 2
  "Horizontal padding to be used when displaying boxes."
  :type 'number)

(defcustom org-real-padding-y 1
  "Vertical padding to be used when displaying boxes."
  :type 'number)

(defcustom org-real-include-context t
  "Whether to show context when opening a real link."
  :type 'boolean)

(defcustom org-real-flex-width 80
  "When merging links, try to keep width below this."
  :type 'number)

(defcustom org-real-default-visibility 2
  "Default level to display boxes."
  :type 'number)

(defcustom org-real-tooltips t
  "Show tooltips in an org real diagram."
  :type 'boolean)

(defcustom org-real-tooltip-timeout 0.5
  "Idle time before showing tooltip in org real diagram."
  :type 'number)

(defcustom org-real-tooltip-max-width 30
  "Maximum width of all tooltips."
  :type 'number)

;;;; Faces

(defface org-real-default nil
  "Default face used in Boxy mode.")

(defface org-real-primary
  '((((background dark)) (:foreground "turquoise"))
    (t (:foreground "dark cyan")))
  "Face for highlighting the name of a box.")

(defface org-real-selected
   '((t :foreground "light slate blue"))
  "Face for the current box border under cursor.")

(defface org-real-rel
   '((t :foreground "hot pink"))
  "Face for the box which is related to the box under the cursor.")

(defface org-real-tooltip
   '((((background dark)) (:background "gray30" :foreground "gray"))
     (t (:background "gainsboro" :foreground "dim gray")))
  "Face for tooltips in a boxy diagram.")

;;;; Pretty printing

(cl-defun org-real-pp (box
                       &key
                       (display-buffer-fn 'display-buffer-pop-up-window)
                       (visibility org-real-default-visibility)
                       (max-visibility 3)
                       select
                       header
                       (default-margin-x org-real-margin-x)
                       (default-margin-y org-real-margin-y)
                       (default-padding-x org-real-padding-x)
                       (default-padding-y org-real-padding-y)
                       (flex-width org-real-flex-width)
                       (tooltips org-real-tooltips)
                       (tooltip-timeout org-real-tooltip-timeout)
                       (tooltip-max-width org-real-tooltip-max-width)
                       (default-face 'org-real-default)
                       (primary-face 'org-real-primary)
                       (tooltip-face 'org-real-tooltip)
                       (rel-face 'org-real-rel)
                       (selected-face 'org-real-selected))
  "Pretty print BOX in a popup buffer.

If HEADER is passed in, it will be printed above the diagram.

DISPLAY-BUFFER-FN is used to display the diagram, by
default `display-buffer-pop-up-window'.

If SELECT is non-nil, select the boxy window after displaying
it.

VISIBILITY is the initial visibility of children and
MAX-VISIBILITY is the maximum depth to display when cycling
visibility.

DEFAULT-MARGIN-X, DEFAULT-MARGIN-Y, DEFAULT-PADDING-X and
DEFAULT-PADDING-Y will be the fallback values to use if a box's
margin and padding slots are not set.

When adding boxes, boxy will try to keep the width below
FLEX-WIDTH.

If TOOLTIPS is nil, don't show any tooltips.

TOOLTIP-TIMEOUT is the idle time to wait before showing a
tooltip.

TOOLTIP-MAX-WIDTH is the maximum width of a tooltip.  Lines
longer than this will be truncated.

DEFAULT-FACE, PRIMARY-FACE, TOOLTIP-FACE, REL-FACE, and
SELECTED-FACE can be set to change the appearance of the boxy
diagram."
  (boxy-pp box
           :display-buffer-fn display-buffer-fn
           :visibility visibility
           :max-visibility max-visibility
           :select select
           :header header
           :default-margin-x default-margin-x
           :default-margin-y default-margin-y
           :default-padding-x default-padding-x
           :default-padding-y default-padding-y
           :flex-width flex-width
           :tooltips tooltips
           :tooltip-timeout tooltip-timeout
           :tooltip-max-width tooltip-max-width
           :default-face default-face
           :primary-face primary-face
           :tooltip-face tooltip-face
           :rel-face rel-face
           :selected-face selected-face))

(defun org-real--get-header (containers)
  "Get a textual representation of CONTAINERS."
  (let* ((reversed (reverse containers))
         (container (pop reversed))
         (primary-name (plist-get container :name))
         (header ""))
    (put-text-property 0 (length primary-name) 'face 'org-real-primary
                       primary-name)
    (cl-flet ((append-str (&rest strings)
                          (setq header (apply #'concat header strings))))
      (append-str (make-string org-real-margin-y ?\n)
                  (make-string org-real-margin-x ?\s)
                  "The " primary-name)
      (if reversed (append-str (if (org-real--is-plural primary-name) " are" " is")))
      (while reversed
        (append-str " " (plist-get container :rel))
        (setq container (pop reversed))
        (append-str " the " (plist-get container :name)))
      (append-str ".")
      header)))

;;;; Commands

;;;###autoload
(defun org-real-world ()
  "View all real links in the current buffer."
  (interactive)
  (let* ((link (cond
                ((org-in-regexp org-link-bracket-re 1)
                 (match-string-no-properties 1))
                ((org-in-regexp org-link-plain-re)
                 (org-unbracket-string "<" ">" (match-string 0)))))
         (world (boxy-merge
                 (mapcar
                  #'org-real--make-box
                  (org-real--parse-buffer)))))
    (org-real-pp world
             :display-buffer-fn 'display-buffer-same-window
             :select t)
    (if (and link (string= "real" (ignore-errors (url-type (url-generic-parse-url link)))))
        (let ((containers (reverse (org-real--parse-url link)))
              match)
          (while (and containers (or (not match) (not (boxy-is-visible match t))))
            (setq match (boxy-find-matching
                         (boxy-box :name (plist-get (pop containers) :name))
                         world)))
          (when match
            (with-current-buffer (get-buffer "*Boxy*")
              (boxy-jump-to-box match)))))))

;;;###autoload
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
                             (new-link (org-link-make-string replace-link old-desc)))
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
              (mapc #'funcall changes)))
          (pop new-containers)))))
  (message nil))

;;;; `org-insert-link' configuration

;;;###autoload
(org-link-set-parameters "real"
                         :follow #'org-real-follow
                         :complete #'org-real-complete)

;;;###autoload
(defun org-real-follow (url &rest _)
  "Open a real link URL in a popup buffer."
  (let* ((containers (org-real--parse-url url (point-marker)))
         (box (org-real--make-box (copy-tree containers))))
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
                                 (org-real--make-box containers t))
                               container-matrix)))
          (mapc
           (lambda (context) (boxy-merge-into context box))
           context-boxes)))
    (org-real-pp box
             :header (org-real--get-header containers)
             :visibility 0)))

;;;###autoload
(defun org-real-complete (&optional existing)
  "Complete a real link or edit EXISTING link."
  (let* ((container-matrix (org-real--parse-buffer))
         (containers (if existing
                         (org-real--parse-url existing (point-marker))
                       (org-real--complete-thing "Thing: " container-matrix '()))))
    (catch 'confirm
      (while t
        (org-real-pp (org-real--make-box containers)
                 :header (org-real--get-header containers)
                 :visibility 0)
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
                   (verb (if (org-real--is-plural top) "are" "is"))
                   (preposition
                    (completing-read (concat "The " top " " verb ": ") boxy-relationships nil t))
                   (additional-containers
                    (org-real--complete-thing (concat "The " top " " verb " " preposition " the: ")
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
                         (apply #'append container-matrix)))))
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

;;;; Boxy box implementation

(defun org-real--make-box (containers &optional skip-primary)
  "Create a `boxy-box' from CONTAINERS.

If SKIP-PRIMARY is non-nil, don't highlight the primary box."
  (let ((world (boxy-box
                :margin-x org-real-margin-x
                :margin-y org-real-margin-y
                :padding-x org-real-padding-x
                :padding-y org-real-padding-y)))
    (org-real--add-container containers world skip-primary)
    world))

(defun org-real--add-container (containers
                                prev
                                &optional
                                skip-primary
                                force-visible)
  "Add the first container from CONTAINERS to PREV.

If SKIP-PRIMARY, don't highlight the primary box.

If FORCE-VISIBLE, force the child to be visible regardless of its
level."
  (let* ((container (pop containers))
         (name (plist-get container :name))
         (box (boxy-box :name name
                        :markers (list (plist-get container :loc))
                        :post-jump-hook 'org-reveal)))
    (when-let* ((rel (plist-get container :rel))
                (rel-name (and (slot-boundp prev :name) (with-slots (name) prev name)))
                (verb (if (org-real--is-plural name) " are " " is "))
                (tooltip (concat "The " name verb rel " the " rel-name ".")))
      (oset box tooltip (boxy-fill-tooltip tooltip))
      (oset box rel rel))
    (if (not containers)
        (unless skip-primary (oset box primary t))
      (let ((next-rel (plist-get (car containers) :rel)))
        (cond
         ((member next-rel boxy-children-relationships)
          (object-add-to-list box :expand-children
                              `(lambda (box)
                                 (org-real--add-container ',containers box ,skip-primary))))
         ((member next-rel boxy-sibling-relationships)
          (object-add-to-list box :expand-siblings
                              `(lambda (box)
                                 (org-real--add-container ',containers box ,skip-primary t)))))))
    (boxy-add-next box prev force-visible)))


;;;; Utility expressions

(defun org-real--find-last-index (pred sequence)
  "Return the index of the last matching element.

Calls (PRED element) for each element in SEQUENCE until a match
is found."
  (let ((i (- (length sequence) 1)))
    (catch 'match
      (mapc
       (lambda (elt)
         (if (funcall pred elt) (throw 'match i))
         (setq i (- i 1)))
       (reverse sequence))
      nil)))

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
