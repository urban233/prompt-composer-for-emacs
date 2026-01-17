;;; prompt-composer.el --- Minor mode for managing LLM prompts -*- lexical-binding: t; -*-

;; Author: Martin Urban
;; Version: 0.7 (Smart Content)
;; Keywords: prompts, lisp, emacs
;; Package-Requires: ((emacs "26.1") (org "9.0") (transient "0.3.7"))

;;; Commentary:
;; A drop-in minor mode to compose prompts.
;; Includes compatibility shim and smart content cleaning.

;;; Code:

(require 'org)
(require 'org-id)
(require 'subr-x)
(require 'transient)
(require 'dired)

;; -------------------------------------------------------------------------
;; COMPATIBILITY SHIM (Do not remove)
;; -------------------------------------------------------------------------
(unless (fboundp 'org-element--cache-active-p)
  (defun org-element--cache-active-p (&optional _element)
    "Dummy function to prevent crashes in mixed Org versions."
    nil))

;; -----------------------------
;; Configurable variables
;; -----------------------------

(defgroup prompt-composer nil
  "Minor mode for composing LLM prompts."
  :group 'applications)

(defcustom prompt-composer-org-directory
  (expand-file-name "~/.emacs.d/prompts/")
  "Directory containing Org files with prompts and modifiers."
  :type 'directory
  :group 'prompt-composer)

(defvar prompt-composer--structure-buffer "*Prompt Structure*"
  "Name of the buffer for defining prompt structure.")

(defvar prompt-composer--content-buffer "*Prompt Content*"
  "Name of the buffer for dynamic input content.")

(defvar prompt-composer--preview-buffer "*Prompt Preview*"
  "Name of the buffer for the final output.")

;; NEW: Centralized default text
(defvar prompt-composer--default-content-text "Paste your dynamic content here..."
  "The default placeholder text used in the content buffer.")

;; -----------------------------
;; Core functions
;; -----------------------------

(defun prompt-composer--update-id-locations ()
  "Scan the prompt directory and update Org ID locations."
  (let ((files (directory-files-recursively prompt-composer-org-directory "\\.org$")))
    (org-id-update-id-locations files t)))

(defun prompt-composer--get-all-ids ()
  "Return a list of all known Org IDs for autocompletion."
  (unless org-id-locations (org-id-locations-load))
  (let (ids)
    (if (hash-table-p org-id-locations)
        (maphash (lambda (k _v) (push k ids)) org-id-locations)
      (setq ids (mapcar #'car org-id-locations)))
    ids))

(defun prompt-composer-get-by-id (id)
  "Return the body text of the Org entry with ID."
  (unless (org-id-find id)
    (prompt-composer--update-id-locations))

  (let ((marker (org-id-find id t)))
    (if (not marker)
        (format "[Error: ID '%s' not found]" id)
      (with-current-buffer (marker-buffer marker)
        (org-with-wide-buffer
         (goto-char marker)
         (org-narrow-to-subtree)
         (prog1
             (string-trim
              (buffer-substring-no-properties
               (progn (org-end-of-meta-data t) (point))
               (point-max)))
           (widen)))))))

(defun prompt-composer-fill (prompt variables)
  "Replace {{var}} in PROMPT using VARIABLES alist."
  (dolist (var variables prompt)
    (setq prompt
          (replace-regexp-in-string
           (format "{{%s}}" (car var))
           (or (cdr var) "")
           prompt t t))))

(defun prompt-composer--extract-vars (text)
  "Return a list of {{variable}} names found in TEXT, excluding 'content'."
  (let ((vars '())
        (start 0))
    (while (string-match "{{\\([^}]+\\)}}" text start)
      (let ((var (match-string 1 text)))
        (unless (string-equal var "content")
          (push var vars)))
      (setq start (match-end 0)))
    (delete-dups (nreverse vars))))

;; -----------------------------
;; Autocomplete Support
;; -----------------------------

(defun prompt-composer-completion-at-point ()
  "Completion backend for Org IDs in the structure buffer."
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when (and bounds
               (save-excursion
                 (goto-char (line-beginning-position))
                 (looking-at-p "^\\(BASE\\|MODIFIERS\\):")))
      (list (car bounds)
            (cdr bounds)
            (prompt-composer--get-all-ids)
            :exclusive 'no))))

;; -----------------------------
;; Structure Parsing & Template Logic
;; -----------------------------

(defun prompt-composer-read-structure (buffer)
  "Read BASE, MODIFIERS, and VARIABLES from BUFFER."
  (if (not (get-buffer buffer))
      (list :base nil :modifiers nil :vars nil)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (let* ((base (when (re-search-forward "^BASE: \\(.*\\)$" nil t)
                       (string-trim (match-string 1))))
               (modifiers (progn
                            (goto-char (point-min))
                            (when (re-search-forward "^MODIFIERS: \\(.*\\)$" nil t)
                              (split-string (match-string 1) " " t))))
               (vars (progn
                       (goto-char (point-min))
                       (let (res)
                         (when (re-search-forward "^VARIABLES:$" nil t)
                           (while (re-search-forward "^\\([^:\n]+\\): \\(.*\\)$" nil t)
                             (push (cons (match-string 1) (match-string 2)) res)))
                         res))))
          (list :base base :modifiers modifiers :vars vars))))))

(defun prompt-composer-populate-vars ()
  "Scan selected BASE and MODIFIERS for {{vars}} and insert them into Structure buffer."
  (interactive)
  (let* ((props (prompt-composer-read-structure prompt-composer--structure-buffer))
         (base-id (plist-get props :base))
         (mod-ids (plist-get props :modifiers))
         (full-text (concat
                     (when base-id (prompt-composer-get-by-id base-id))
                     (when mod-ids (mapconcat #'prompt-composer-get-by-id mod-ids "\n"))))
         (found-vars (prompt-composer--extract-vars full-text)))

    (with-current-buffer prompt-composer--structure-buffer
      (goto-char (point-min))
      (unless (re-search-forward "^VARIABLES:$" nil t)
        (goto-char (point-max))
        (insert "\n\nVARIABLES:\n"))

      (goto-char (point-max))
      (dolist (var found-vars)
        (goto-char (point-min))
        (unless (re-search-forward (format "^%s:" (regexp-quote var)) nil t)
          (goto-char (point-max))
          (insert (format "%s: \n" var)))))
    (message "Template variables populated.")))

(defun prompt-composer-compose (structure-buffer content-buffer)
  "Compose a prompt using STRUCTURE-BUFFER and CONTENT-BUFFER."
  (let* ((props (prompt-composer-read-structure structure-buffer))
         (base-id (plist-get props :base))
         (mod-ids (plist-get props :modifiers))
         (vars (plist-get props :vars))
         (base (if base-id (prompt-composer-get-by-id base-id) ""))
         (mods (if mod-ids (mapconcat #'prompt-composer-get-by-id mod-ids "\n\n") ""))

         ;; UPDATED LOGIC: Check for placeholder text
         (content (if (get-buffer content-buffer)
                      (with-current-buffer content-buffer
                        (let ((raw-text (string-trim (buffer-string))))
                          ;; If text matches default placeholder, return empty string
                          (if (string-equal raw-text prompt-composer--default-content-text)
                              ""
                            raw-text)))
                    ""))

         (all-vars (append vars `(("content" . ,content)))))

    (prompt-composer-fill
     (string-join (delq nil (list base mods "{{content}}")) "\n\n")
     all-vars)))

(defun prompt-composer-copy (structure-buffer content-buffer)
  (interactive
   (list (read-buffer "Structure buffer: " prompt-composer--structure-buffer)
         (read-buffer "Content buffer: " prompt-composer--content-buffer)))
  (let ((final (prompt-composer-compose structure-buffer content-buffer)))
    (kill-new final)
    (message "Prompt composed and copied to clipboard.")))

(defun prompt-composer-preview (structure-buffer content-buffer)
  (interactive
   (list (read-buffer "Structure buffer: " prompt-composer--structure-buffer)
         (read-buffer "Content buffer: " prompt-composer--content-buffer)))
  (let ((final (prompt-composer-compose structure-buffer content-buffer))
        (preview-buf (get-buffer-create prompt-composer--preview-buffer)))
    (with-current-buffer preview-buf
      (erase-buffer)
      (visual-line-mode)
      (insert final)
      (goto-char (point-min)))
    (when-let ((win (get-buffer-window preview-buf)))
      (set-window-point win (point-min)))))

;; -----------------------------
;; Workspace Management
;; -----------------------------

(defun prompt-composer--init-buffer (name mode &optional initial-text)
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (funcall mode)
      (when (and initial-text (= (buffer-size) 0))
        (insert initial-text)))
    buf))

;;;###autoload
(defun prompt-composer-workspace ()
  "Set up a 4-pane workspace for prompt engineering with autocomplete."
  (interactive)
  (prompt-composer--update-id-locations)

  (let ((struct-buf (prompt-composer--init-buffer prompt-composer--structure-buffer 'text-mode "BASE: \nMODIFIERS: \n\nVARIABLES:\n")))
    (with-current-buffer struct-buf
      (add-hook 'completion-at-point-functions #'prompt-composer-completion-at-point nil t)))

  ;; UPDATED: Use the variable for initialization
  (prompt-composer--init-buffer prompt-composer--content-buffer 'text-mode prompt-composer--default-content-text)
  (get-buffer-create prompt-composer--preview-buffer)

  (delete-other-windows)
  (let ((win-left (selected-window))
        (win-right (split-window-right)))

    (select-window win-left)
    (dired prompt-composer-org-directory)
    (split-window-below)
    (windmove-down)
    (switch-to-buffer prompt-composer--structure-buffer)

    (select-window win-right)
    (switch-to-buffer prompt-composer--content-buffer)
    (split-window-below)
    (windmove-down)
    (switch-to-buffer prompt-composer--preview-buffer))

  (message "Prompt Composer Workspace initialized."))

;; -----------------------------
;; Transient Menu
;; -----------------------------

(defun prompt-composer--select-structure-buffer (buffer)
  (interactive
   (list (read-buffer "Select Structure buffer: " prompt-composer--structure-buffer)))
  (setq prompt-composer--structure-buffer buffer)
  (transient-setup 'prompt-composer-transient))

(defun prompt-composer--select-content-buffer (buffer)
  (interactive
   (list (read-buffer "Select Content buffer: " prompt-composer--content-buffer)))
  (setq prompt-composer--content-buffer buffer)
  (transient-setup 'prompt-composer-transient))

;;;###autoload
(transient-define-prefix prompt-composer-transient ()
  "Prompt Composer Menu"
  [:description
   (lambda ()
     (format "Prompt Composer (Struct: %s | Content: %s)"
             prompt-composer--structure-buffer
             prompt-composer--content-buffer))

   ["Workspace"
    ("w" "Initialize Workspace" prompt-composer-workspace)
    ("i" "Populate Template Vars" prompt-composer-populate-vars)]

   ["Actions"
    ("p" "Preview Prompt" (lambda () (interactive)
                            (prompt-composer-preview
                             prompt-composer--structure-buffer
                             prompt-composer--content-buffer)))
    ("y" "Copy Prompt"    (lambda () (interactive)
                            (prompt-composer-copy
                             prompt-composer--structure-buffer
                             prompt-composer--content-buffer)))]

   ["Quit"
    ("q" "Quit" transient-quit-one)]])

(defvar prompt-composer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c p m") #'prompt-composer-transient)
    (define-key map (kbd "C-c p w") #'prompt-composer-workspace)
    (define-key map (kbd "C-c p v") #'prompt-composer-preview)
    (define-key map (kbd "C-c p i") #'prompt-composer-populate-vars)
    map)
  "Keymap for `prompt-composer-mode'.")

;;;###autoload
(define-minor-mode prompt-composer-mode
  "Minor mode to manage and compose reusable prompts."
  :lighter " Prompt"
  :keymap prompt-composer-mode-map
  :group 'prompt-composer
  (if prompt-composer-mode
      (message "Prompt Composer mode enabled")
    (message "Prompt Composer mode disabled")))

(provide 'prompt-composer)
;;; prompt-composer.el ends here
