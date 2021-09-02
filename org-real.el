(require 'eieio)
(require 'org)
(require 'cl)


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

(defvar org-real-prepositions
  '("in" "behind" "in front of" "above" "below" "to the left of" "to the right of"))

(defun org-real--create-box (containers &optional parent prev)
  (if (not parent)
      (let ((world (org-real--box)))
        (org-real--create-box containers world)
        world)
    (let* ((container (pop containers))
           (rel (plist-get container :rel))
           (box (org-real--box :name (plist-get container :name))))
      (when prev
        (oset box :rel (plist-get container :rel))
        (oset box :rel-box prev)
        (cond ((string= rel "in")
               (oset box :x-order (oref prev :x-order))
               (oset box :y-order (oref prev :y-order))
               (oset box :behind (oref prev :behind)))
              ((string= rel "behind")
               (oset box :x-order (oref prev :x-order))
               (oset box :y-order (oref prev :y-order))
               (oset box :behind t))
              ((string= rel "in front of")
               (oset box :x-order (oref prev :x-order))
               (oset box :y-order (oref prev :y-order))
               (oset box :behind (oref prev :behind))
               (oset box :in-front t))
              ((string= rel "above")
               (oset box :x-order (oref prev :x-order))
               (oset box :y-order (- 1 (oref prev :y-order)))
               (oset box :behind (oref prev :behind)))
              ((string= rel "below")
               (oset box :x-order (oref prev :x-order))
               (oset box :y-order (+ 1 (oref prev :y-order)))
               (oset box :behind (oref prev :behind))
               (oset box :in-front (oref prev :in-front)))
              ((string= rel "to the left of")
               (oset box :x-order (- 1 (oref prev :x-order)))
               (oset box :y-order (oref prev :y-order))
               (oset box :behind (oref prev :behind))
               (oset box :in-front (oref prev :in-front)))
              ((string= rel "to the right of")
               (oset box :x-order (+ 1 (oref prev :x-order)))
               (oset box :y-order (oref prev :y-order))
               (oset box :behind (oref prev :behind))
               (oset box :in-front (oref prev :in-front)))))

      (if (and prev (member (oref box :rel)
                            '("in" "behind" "in front of")))
          (progn
            (oset box :parent prev)
            (object-add-to-list prev :children box)
            (if containers
                (org-real--create-box containers prev box)
              (oset box :primary t)))
        (oset box :parent parent)
        (object-add-to-list parent :children box)
        (if containers
            (org-real--create-box containers parent box)
          (oset box :primary t))))))
    
(defun org-real--parse-url (str)
  "Parse URL into an org real object"
  (let* ((url (url-generic-parse-url str))
         (host (url-host url))
         (path-and-query (url-path-and-query url))
         (tokens (cdr
                     (split-string (concat (car path-and-query) "?"
                                           (cdr path-and-query))
                                   "/")))
         (containers (mapcar
                      (lambda (token)
                        (let* ((location (split-string token "?"))
                               (container (list :name (car location)))
                               (rel (and (string-match "&?rel=\\([^&]*\\)" (cadr location))
                                         (match-string 1 (cadr location)))))
                          (if rel
                              (plist-put container :rel rel)
                            container)))
                      tokens)))
    (add-to-list 'containers (list :name host))))

(org-link-set-parameters "real"
                         :follow #'org-real-follow)

(defun org-real-follow (url &rest args)
  (let* ((containers (org-real--parse-url url))
         (box (org-real--create-box (copy-tree containers))))
    (org-real--pp box (copy-tree containers))))

(defvar org-real--level)

(defvar org-real--padding '(2 . 1))
(defvar org-real--margin '(2 . 1))

(defun org-real--pp (box containers)
  (let ((width (org-real--get-width box))
        (height (org-real--get-height box)))
    (with-current-buffer-window "Org Real" nil nil
      (org-real--pp-text containers)
      (let ((offset (line-number-at-pos)))
        (dotimes (_ (+ 10 height)) (insert (concat (make-string width ?\s) "\n")))
        (org-real--draw box offset)
        (special-mode)))))

(defface org-real-primary
  '((t :background "aquamarine"
       :foreground "black"))
  "Face for the last thing in a url"
  :group 'org-real)

(defun org-real--pp-text (containers)
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
    (fill-paragraph)))

(defun org-real--draw (box offset)
  (let ((children (oref box :children)))
    (if (slot-boundp box :name)
        (let* ((top (+ offset (org-real--get-top box)))
               (left (org-real--get-left box))
               (width (org-real--get-width box))
               (height (org-real--get-height box))
               (name (oref box :name))
               (children (oref box :children))
               (dashed (oref box :behind))
               (align-bottom (oref box :in-front))
               (primary (oref box :primary)))
          (cl-flet ((draw (coords str &optional primary)
                       (goto-line (car coords))
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
              (dotimes (_var (- height (if align-bottom 1 2)))
                (draw (cons r c1) (if dashed "╎" "│"))
                (draw (cons r c2) (if dashed "╎" "│"))
                (setq r (+ r 1)))))))
    (mapc
     (lambda (child) (org-real--draw child offset))
     children)))
    

(defun org-real--get-width (box)
  (let* ((base-width (+ 2 ; box walls
                        (* 2 (car org-real--padding))))
         (width (+ base-width (if (slot-boundp box :name)
                                  (length (oref box :name))
                                0)))
         (children (oref box :children)))
    (if (not children)
        width
      (let ((rows '()))
        (mapc
         (lambda (child)
           (add-to-list 'rows (oref child :y-order)))
         children)
        (let ((child-widths (mapcar 
                             (lambda (row)
                               (+ base-width
                                  (seq-reduce
                                   (lambda (sum child) (+ sum
                                                          (car org-real--padding)
                                                          (org-real--get-width child)))
                                   (seq-filter
                                    (lambda (child) (= row (oref child :y-order)))
                                    children)
                                   (* -1 (car org-real--padding)))))
                             rows)))
          (apply 'max width child-widths))))))

(defun org-real--get-height (box)
  (let ((height (+ (if (oref box :in-front)
                       (* -1 (cdr org-real--margin))
                     0)
                   2 ; box walls
                   (* 2 (cdr org-real--padding))
                   (cdr org-real--margin)))
        (children (oref box :children))
        (in-front (oref box :in-front)))
    (if (not children)
        height
      (let ((columns '()))
        (mapc
         (lambda (child) (add-to-list 'columns (oref child :x-order)))
         children)
        (let ((child-heights (mapcar
                              (lambda (col)
                                (+ height
                                   (seq-reduce
                                    (lambda (sum child) (+ sum (org-real--get-height child)))
                                    (seq-filter
                                     (lambda (child) (= col (oref child :x-order)))
                                     children)
                                    0)))
                              columns)))
          (apply 'max height child-heights))))))
                     
(defun org-real--get-top (box)
  (if (not (slot-boundp box :parent))
      0
    (let* ((offset (+ 1 (* 2 (cdr org-real--padding)) (cdr org-real--margin)))
           (parent (oref box :parent))
           (top (+ offset (org-real--get-top parent))))
      (let* ((x-order (oref box :x-order))
             (y-order (oref box :y-order))
             (above (seq-filter
                      (lambda (child) (and (= x-order (oref child :x-order))
                                           (< y-order (oref child :y-order))))
                      (oref parent :children)))
             (directly-above (and above (seq-reduce
                                         (lambda (max child)
                                           (if (> (oref child :y-order) (oref max :y-order))
                                               child
                                             max))
                                         above
                                         (org-real--box :y-order -9999)))))
        (if directly-above
            (+ (cdr org-real--margin) offset (org-real--get-top directly-above))
          top)))))

(defun org-real--get-left (box)
  (if (not (slot-boundp box :parent))
      0
    (let* ((offset (+ 2 (* 2 (car org-real--padding)) (car org-real--margin)))
           (parent (oref box :parent))
           (left (+ 1
                    (car org-real--padding)
                    (org-real--get-left parent)))
           (to-the-left (seq-filter
                          (lambda (child) (and (= (oref box :y-order) (oref child :y-order))
                                               (< (oref box :x-order) (oref child :x-order))))
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
             (if (slot-boundp directly-left :name)
                 (length (oref directly-left :name))
               0)
             offset)
        left))))
                             
