# Prompt Composer for Emacs
[![License](https://img.shields.io/badge/License-BSD_3--Clause-green?style=for-the-badge)](https://opensource.org/license/bsd-3-clause)
![Emacs](https://img.shields.io/badge/Emacs-%237F5AB6.svg?&style=for-the-badge&logo=gnu-emacs&logoColor=white)

Prompt Composer is a powerful Emacs minor mode designed to streamline the management, composition, and reuse of LLM (Large Language Model) prompts. It transforms Emacs into a dedicated prompt management tool, allowing you to construct complex prompts dynamically from modular building blocks stored in Org files.

## ✨ Features

* **Modular Prompt Engineering:** Compose prompts by combining "Base" personas with "Modifier" instructions (e.g., "Explain Like I'm 5", "JSON Output").
* **Org-Mode Integration:** Store your prompt library in standard Org files. Uses Org IDs for robust linking and discovery.
* **Dynamic Variables:** Create templates with placeholders (e.g., `{{tone}}`, `{{audience}}`) and fill them interactively.
* **Workspace Management:** One-command setup (`C-c p w`) creates a 4-pane layout: Library (Dired), Structure (Config), Content (Input), and Preview (Output).
* **Smart Autocomplete:** Integrated with `completion-at-point` (compatible with Company/Corfu) to auto-suggest prompt IDs from your library.
* **Smart Content Handling:** Automatically strips placeholder text so you don't accidentally send "Paste your dynamic content here..." to the AI.
* **Robust & Crash-Proof:** Includes built-in shims to prevent common "mixed Org version" crashes.

## 🚀 Installation

### 1. Prerequisites
Ensure you have a `~/.emacs.d/lisp/` directory (or equivalent custom load path).

```bash
mkdir -p ~/.emacs.d/lisp

```

### 2. Save the File

Save the `prompt-composer.el` file into your lisp directory:
`~/.emacs.d/lisp/prompt-composer.el`

### 3. Configure `init.el`

Add the following to your `~/.emacs.d/init.el`. This configuration handles package dependencies (Company, Transient) and sets up the mode safely.

```elisp
;;; init.el --- Emacs Configuration

;; 1. DISABLE ORG CACHE (The Fix)
;; This stops the "org-element--cache-active-p" error by turning off the feature entirely.
(setq org-element-use-cache nil)

;; 2. Initialize Package Manager
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu"   . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; 3. Install Dependencies
(defvar my-packages '(transient company))
(dolist (pkg my-packages)
  (unless (package-installed-p pkg)
    (package-refresh-contents)
    (package-install pkg)))

;; 4. Configure Autocomplete
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0.1
      company-minimum-prefix-length 1)

;; 5. Load Prompt Composer
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'prompt-composer)

;; 6. Keybindings
(global-set-key (kbd "C-c p m") #'prompt-composer-transient)
(global-set-key (kbd "C-c p w") #'prompt-composer-workspace)
(global-set-key (kbd "C-c p v") #'prompt-composer-preview)
(global-set-key (kbd "C-c p i") #'prompt-composer-populate-vars)

;; 7. Load Org (Built-in)
(require 'org)
(require 'org-id)
```

## 🛠 Usage

### 1. Create Your Prompt Library

Create a directory at `~/.emacs.d/prompts/`. Inside, create Org files (e.g., `personas.org`, `modifiers.org`).

**Important:** Every prompt entry must have an `:ID:` property. You can generate one by running `M-x org-id-get-create` on the headline.

**Example `personas.org`:**

```org
* Python Expert
  :PROPERTIES:
  :ID:       python-expert
  :END:
  You are a senior Python developer. Write clean, PEP8-compliant code.

* Email Generator
  :PROPERTIES:
  :ID:       email-gen
  :END:
  Write an email to {{recipient}} about {{topic}}. Use a {{tone}} tone.

```

### 2. The Workflow

1. **Launch Workspace:** Press `C-c p w`. Emacs will split into 4 panes.
2. **Define Structure (Bottom-Left):**
* Use autocomplete (type and wait for popup) to find IDs.
* Example:
```text
BASE: email-gen
MODIFIERS: eli5

```


3. **Populate Variables (Optional):**
* If your prompt uses `{{variables}}`, press `C-c p i`.
* The buffer will update with fields for you to fill in.


4. **Add Content (Top-Right):**
* Paste your specific input (code, question, text) into the `*Prompt Content*` buffer.


5. **Preview & Copy:**
* Press `C-c p v` to generate the final prompt in the Preview buffer.
* Press `C-c p m` then `y` to copy it to the clipboard.



## ⌨️ Keybindings

| Key | Command | Description |
| --- | --- | --- |
| `C-c p w` | `prompt-composer-workspace` | Initialize the 4-pane workspace |
| `C-c p v` | `prompt-composer-preview` | Generate and show the prompt |
| `C-c p i` | `prompt-composer-populate-vars` | Scan for variables (`{{...}}`) and create input fields |
| `C-c p m` | `prompt-composer-transient` | Open the main menu (Transient) |
