;;; org-real.el --- Create org-mode links to real things -*- lexical-binding: t -*-

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

(require 'org-element)
(require 'cl-extra)

(require 'org-real--box)

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
       (org-real--make-instance 'org-real--box containers))
     (org-real--parse-buffer)))))

;;;; `org-insert-link' configuration

(org-link-set-parameters "real"
                         :follow #'org-real-follow
                         :complete #'org-real-complete)

(defun org-real-follow (url &rest _)
  "Open a real link URL in a popup buffer."
  (let* ((containers (org-real--parse-url url))
         (box (org-real--make-instance 'org-real--box (copy-tree containers))))
    (org-real--pp box (copy-tree containers))))

(defun org-real-complete (&optional existing)
  "Complete a real link or edit EXISTING link."
  (let* ((container-matrix (org-real--parse-buffer))
         (containers (if existing
                         (org-real--parse-url existing)
                       (org-real--complete-thing "Thing: " container-matrix))))
    (catch 'confirm
      (while t
        (org-real--pp (org-real--make-instance 'org-real--box containers) containers)
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
  "Apply any changes to the current buffer if last inserted link is real."
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
                          (insert (org-link-make-string ,replace-link ,old-desc)))
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
                       (cdr org-real--margin)
                       (* 2 (cdr org-real--padding)))))
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
    (dotimes (_ (cdr org-real--padding)) (insert "\n"))
    (insert (make-string (car org-real--padding) ?\s))
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
