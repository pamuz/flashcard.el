;; flashcard.el
;; A major mode for creating and studying flashcards
;; within the emacs text editor.
;; AUTHOR: Pablo Muñoz Haro

(require 'json)

(defvar flashcard-front-identifier 
  "FRONT"
  "Text to prepend to the front side of flashcards.")

(defvar flashcard-back-identifier 
  "BACK"
  "Text to prepend to the front side of flashcards.")

(defvar flashcard-header-delim-format
  "\[%d\] %s\n----------------------------------------------------------------------\n"
  "Format string to use as a header for both flashcard sides.")

(defun flashcard--make-set (string)
  "Converts STRING (assumed to be valid json) into a deck."
  (json-read-from-string string))

(defun flashcard--read-set (filename)
  "Read a file and convert its contents to a deck."
  (let ((file-contents nil))
    (setq file-contents (with-temp-buffer
                   (insert-file-contents filename)
                   (buffer-string)))
    (if file-contents
        (flashcard--make-set file-contents)
      nil)))

(defmacro flashcard--with-editable (&rest subexprs)
  "Disables read-only mode temporarily to execute SUBEXPRS."
  `(progn
     (setq buffer-read-only nil)
     ,@subexprs
     (setq buffer-read-only t)))

(defun flashcard--clear-buffer ()
  "Erases everything from the buffer."
  (flashcard--with-editable (erase-buffer)))

(defun deck--get-flashcard (id)
  "Returns the flashcard with given ID"
  (let* ((data (cdr (assq 'data deck)))
         (flashcard nil))
    (let ((i 0)
          (n (length data)))
      (while (< i n)
        (when (seq-contains (aref data i) `(id . ,id))
          (setq flashcard (aref data i)))
        (setq i (+ i 1))))
    flashcard))

(defun flashcard--get-current-side ()
  "Return the symbol representing the currently displayed side of the currently
displayed flashcard."
  (alist-get 'side current-flashcard-data))

(defun flashcard--set-current-side (side)
  "Set the currently displayed side of the currently displayed flashcard
to SIDE. Assumes SIDE is one of 'front or 'back."
  (setcdr (assq 'side current-flashcard-data) side))

(defun flashcard--viewing-front-p ()
  (equal (flashcard--get-current-side) 'front))

(defun flashcard--viewing-back-p ()
  (equal (flashcard--get-current-side) 'back))

(defun deck--get-current-flashcard-id ()
  "Returns the id of the current flashcard."
  (alist-get 'id current-flashcard-data))

(defun deck--get-current-flashcard ()
  "Returns the flashcard currently being displayed."
  (deck--get-flashcard (alist-get 'id current-flashcard-data)))

(defun flashcard--update-display ()
  "Updates the display to show the current flashcard."
  (flashcard--clear-buffer)
  (let* ((flashcard-id (alist-get 'id current-flashcard-data))
         (flashcard-side (flashcard--get-current-side))
         (flashcard (deck--get-flashcard flashcard-id))
         (attributes (alist-get 'attributes flashcard))
         (content (alist-get flashcard-side attributes))
         (header (format flashcard-header-delim-format
                         flashcard-id
                         (if (flashcard--viewing-front-p)
                             flashcard-front-identifier
                           flashcard-back-identifier))))
    (flashcard--with-editable
     (insert header content))))

(defun flashcard--get-metadata ()
  "Return the portion of the JSON with the metadata data."
  (alist-get 'metadata deck))

(defun flashcard--get-flashcards ()
  "Return the portion of the JSON with the flashcard data."
  (alist-get 'data deck))

(defun flashcard--new (id front-content back-content)
  "Create a new flashcard with the given data."
  `((type . "flashcard")
    (id . ,id)
    (attributes . ((front . ,front-content)
                   (back . ,back-content)))))

(defun deck--override-flashcard-side (flashcard side new-content)
  (let ((side (flashcard--get-current-side))
        (attributes (alist-get 'attributes flashcard)))
    (setcdr (assq side attributes) new-content)))

(defun flashcard-flip-current ()
  "Flips the currently displayed card to reveal the other side."
  (interactive)
  (flashcard-save-current)
  (let ((flashcard-side (flashcard--get-current-side)))
    (flashcard--set-current-side (if (equal flashcard-side 'front)
                                     'back
                                   'front))
    (flashcard--update-display)))

(defun flashcard-next ()
  "Displays the next card."
  (interactive)
  (flashcard-save-current)
  (let* ((id (alist-get 'id current-flashcard-data))
         (metadata (alist-get 'metadata deck))
         (num-flashcards (alist-get 'num_flashcards metadata))
         (last-flashcard-id (alist-get 'last_flashcard_id metadata)))
    (setcdr (assq 'id current-flashcard-data)
            (if (> num-flashcards id)
                (+ id 1)
              1)))
  (flashcard--set-current-side 'front)
  (flashcard--update-display))

(defun flashcard-create ()
  "Create a new flashcard."
  (interactive)
  (let* ((metadata (flashcard--get-metadata))
         (new-num-flashcards (+ 1 (alist-get 'num_flashcards metadata)))
         (new-flashcard-id (+ 1 (alist-get 'last_flashcard_id metadata)))
         (flashcards (flashcard--get-flashcards))
         (new-flashcard (flashcard--new new-flashcard-id "" "")))
    (setcdr (assq 'data deck) (vconcat flashcards `(,new-flashcard)))
    (setcdr (assq 'num_flashcards metadata) new-num-flashcards)
    (setcdr (assq 'last_flashcard_id metadata) new-flashcard-id)
    (flashcard--set-current-side 'front)
    (setcdr (assq 'id current-flashcard-data) new-flashcard-id))
  (flashcard--update-display)
  (flashcard-edit-current))

(defun flashcard-edit-current ()
  "Allow edition for the currently displayed flashcard."
  (interactive)
  (setq buffer-read-only nil)
  (setq current-flashcard-editable t))

(defun flashcard-save-current ()
  "Save the current flashcard. Assumes the flashcard was being edited."
  (interactive)
  (setq buffer-read-only t)
  (save-excursion
    (goto-line 3)
    (set-buffer-modified-p nil)
    (let ((content (buffer-substring (point) (point-max))))
      (deck--override-flashcard-side (deck--get-current-flashcard)
                                     (flashcard--get-current-side)
                                     content))))

(defun flashcard-save-deck ()
  "Saves the current deck to the buffer's file."
  (interactive)
  (setq buffer-read-only t)
  (flashcard-save-current)
  (set-buffer-modified-p nil)
  (let ((string-content (json-encode deck)))
    (write-region string-content nil source-file-name nil)))

(defvar flashcard-mode-map nil
  "Keymap for Flashcard mode.")

(if flashcard-mode-map
    nil
  (setq flashcard-mode-map (make-keymap))
  (let ((mappings
         '(("C-c n" . flashcard-next)
           ("C-c e" . flashcard-edit-current)
           ("C-c s" . flashcard-save-deck)
           ("C-c c" . flashcard-create)
           ("C-c f" . flashcard-flip-current))))
    (while mappings
      (define-key flashcard-mode-map (kbd (car (car mappings))) (cdr (car mappings)))
      (setq mappings (cdr mappings)))))

(defun flashcard-mode ()
  "Major mode for creating and studying flashcards.
Special commands:
\{flashcard-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'flashcard)
  (setq mode-name "Flashcard")
  (use-local-map flashcard-mode-map)

  ;; We want to disassociate the buffer with the file as the contents
  ;; of the buffer wont be a faithful representation of the file. We
  ;; remember the file (for saving and the like) in another variable
  ;; SOURCE-FILE-NAME.
  (make-local-variable 'source-file-name)
  (setq source-file-name (buffer-file-name))

  (setq buffer-read-only t)
  (flashcard--clear-buffer)
  (make-local-variable 'deck)
  (make-local-variable 'current-flashcard-data)
  (setq buffer-file-name nil)
  (setq current-flashcard-data '((id . 1)
                                 (side . front)))
  (setq current-flashcard-editable nil)
  (setq deck (flashcard--read-set source-file-name))
  (flashcard--update-display)
  (run-hooks 'flashcard-mode-hook))
