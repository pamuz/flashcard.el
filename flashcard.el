;; flashcard.el
;; A major mode for creating and studying flashcards
;; within the emacs text editor.
;; AUTHOR: Pablo Muñoz Haro

(require 'json)

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

(defun flashcard-flip-current ()
  "Flips the currently displayed card to reveal the other side."
  (interactive)
  (let ((flashcard-side (alist-get 'side current-flashcard-data)))
    (setcdr (assq 'side current-flashcard-data)
            (if (equal flashcard-side 'front)
                'back
              'front))
    (flashcard--update-display)))

(defun flashcard-next ()
  "Displays the next card."
  (interactive)
  (setcdr (assq 'side current-flashcard-data) 'front)
  (let ((id (alist-get 'id current-flashcard-data)))
    (setcdr (assq 'id current-flashcard-data) (+ id 1)))
  (flashcard--update-display))

(defun flashcard--update-display ()
  "Updates the display to show the current flashcard."
  (flashcard--clear-buffer)
  (let* ((flashcard-id (alist-get 'id current-flashcard-data))
         (flashcard-side (alist-get 'side current-flashcard-data))
         (flashcard (deck--get-flashcard flashcard-id))
         (attributes (alist-get 'attributes flashcard))
         (content (alist-get flashcard-side attributes))) 
    (flashcard--with-editable
     (insert "[" (number-to-string flashcard-id) "] "
             (if (equal flashcard-side 'front)
                 "FRONT\n----------------------------------------------------------------------\n"
               "BACK\n----------------------------------------------------------------------\n")
             content))))

(defun flashcard-mode ()
  "Major mode for creating and studying flashcards."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'flashcard)
  (setq mode-name "Flashcard")
  (setq buffer-read-only t)
  (flashcard--clear-buffer)
  (make-local-variable 'deck)
  (make-local-variable 'current-flashcard-data)
  (setq current-flashcard-data '((id . 1)
                                 (side . front)))
  (setq deck (flashcard--read-set (buffer-file-name)))
  (flashcard--update-display)
  (run-hooks 'flashcard-mode-hook))

