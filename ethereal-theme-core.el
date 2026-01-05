;;; ethereal-theme-core.el --- Ethereal theme core -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Michael J. Seiter

;; Author: Michael J. Seiter
;; Keywords: faces, themes

;;; Commentary:

;; Core utilities, palettes, and face definitions for ethereal theme.
;; Not meant to be loaded directly - use ethereal-theme.el instead.
;;
;; This file contains:
;; - Color palette definitions (two-level: named colors + semantic mappings)
;; - Helper functions for color resolution
;; - Face definitions for syntax highlighting and UI elements
;;
;; Color palette based on Ethereal theme by Bjarne Øverli:
;; - VS Code: https://github.com/bjarneo/ethereal-vscode
;; - Neovim: https://github.com/bjarneo/ethereal.nvim
;;
;; Architecture inspired by modus-themes by Protesilaos Stavrou.

;;; Code:

(require 'cl-lib)

;;;; Customization Variables

(defgroup ethereal-theme nil
  "Ethereal theme for Emacs."
  :group 'faces
  :prefix "ethereal-")

(defcustom ethereal-palette-overrides nil
  "Override specific palette colors.

This is an association list of (COLOR-NAME . VALUE) pairs where
COLOR-NAME is a symbol and VALUE is either a hex string or another
color symbol.

Example:
  (setq ethereal-palette-overrides
        \\='((fg-keyword blue)
          (bg-main \"#000000\")))"
  :type '(repeat (list symbol (choice symbol string)))
  :group 'ethereal-theme)

;;;; Palette Definition

(defconst ethereal-palette
  '(;; Level 1: Named colors (direct hex values from Ethereal theme)
    (bg-main       "#060B1E")
    (bg-dim        "#0D1426")
    (bg-alt        "#1A2235")
    (bg-active     "#465176")
    (bg-dark       "#000000")

    (fg-main       "#ffcead")
    (fg-dim        "#F99957")
    (fg-alt        "#a89984")

    (red           "#ED5B5A")
    (orange        "#F99957")
    (yellow        "#E9BB4F")
    (green         "#92a593")
    (cyan          "#a3bfd1")
    (blue          "#7d82d9")
    (blue-light    "#a3bfd1")
    (purple        "#c89dc1")
    (magenta       "#c89dc1")
    (coral         "#faaaa9")

    (comment       "#6d7db6")
    (selection     "#7d82d9")

    (bg-red        "#2D1416")
    (bg-green      "#1A2420")
    (bg-blue       "#1e3a5f")
    (bg-yellow     "#2D2816")

    ;; Level 2: Semantic mappings (purpose-based color assignments)
    (bg-region         selection)
    (bg-hover          bg-active)
    (fg-link           blue)
    (fg-link-visited   purple)

    (fg-keyword        purple)
    (fg-string         green)
    (fg-comment        comment)
    (fg-doc            comment)
    (fg-function       blue)
    (fg-type           yellow)
    (fg-constant       coral)
    (fg-variable       red)
    (fg-builtin        purple)

    (fg-err            red)
    (fg-warning        yellow)
    (fg-info           blue)
    (fg-success        green)

    (bg-added          bg-green)
    (bg-removed        bg-red)
    (bg-changed        bg-blue)
    (fg-added          green)
    (fg-removed        red)
    (fg-changed        coral)

    (bg-mode-line      bg-alt)
    (fg-mode-line      fg-main)
    (bg-mode-line-inactive bg-dim)
    (fg-mode-line-inactive fg-alt))
  "Ethereal color palette with named and semantic colors.

The palette is organized in two levels:
1. Named colors: Direct hex values from Ethereal VS Code theme
2. Semantic mappings: Purpose-based assignments that reference named colors

This structure allows easy customization and variant creation.")

;;;; Helper Functions

(defun ethereal--get-palette ()
  "Return the palette with user overrides applied."
  (let ((palette (copy-alist ethereal-palette)))
    (dolist (override ethereal-palette-overrides)
      (setf (alist-get (car override) palette) (cadr override)))
    palette))

(defun ethereal--resolve-color (color palette)
  "Resolve COLOR to its hex value using PALETTE.

COLOR can be:
- A hex string (returned as-is)
- A symbol referencing a palette color
- A nested symbol reference (resolved recursively)

Returns the final hex color string."
  (cond
   ((stringp color) color)  ; Already a hex value
   ((symbolp color)
    (let* ((entry (assq color palette))
           (resolved (and entry (cadr entry))))  ; Get second element of (color value) list
      (if (and resolved (symbolp resolved))
          (ethereal--resolve-color resolved palette)  ; Recurse for nested refs
        (or resolved (symbol-name color)))))  ; Fallback to symbol name if not found
   (t (format "%s" color))))  ; Fallback for other types

;;;; Face Definitions

(defconst ethereal-faces
  '(
    ;;;; Essential faces
    (default (:background bg-main :foreground fg-main))
    (cursor (:background green))
    (region (:background bg-region :extend t))
    (highlight (:background bg-hover))
    (fringe (:background bg-dim :foreground fg-alt))
    (vertical-border (:foreground fg-alt))
    (shadow (:foreground fg-alt))

    ;;;; Font lock (syntax highlighting)
    (font-lock-keyword-face (:foreground fg-keyword :weight bold))
    (font-lock-builtin-face (:foreground fg-builtin))
    (font-lock-comment-face (:foreground fg-comment :slant italic))
    (font-lock-comment-delimiter-face (:inherit font-lock-comment-face))
    (font-lock-doc-face (:foreground fg-doc :slant italic))
    (font-lock-string-face (:foreground fg-string))
    (font-lock-function-name-face (:foreground fg-function :weight bold))
    (font-lock-type-face (:foreground fg-type))
    (font-lock-constant-face (:foreground fg-constant))
    (font-lock-variable-name-face (:foreground fg-variable))
    (font-lock-warning-face (:foreground fg-warning :weight bold))
    (font-lock-preprocessor-face (:foreground magenta))

    ;;;; UI Elements
    (link (:foreground fg-link :underline t))
    (link-visited (:foreground fg-link-visited :underline t))
    (button (:inherit link))

    (mode-line (:background bg-mode-line :foreground fg-mode-line
                :box (:line-width 1 :color fg-alt)))
    (mode-line-inactive (:background bg-mode-line-inactive
                         :foreground fg-mode-line-inactive
                         :box (:line-width 1 :color bg-alt)))
    (mode-line-buffer-id (:weight bold))
    (mode-line-emphasis (:foreground blue :weight bold))

    (minibuffer-prompt (:foreground blue :weight bold))
    (header-line (:inherit mode-line))

    ;;;; Line numbers (Emacs 26+)
    (line-number (:foreground fg-alt :background bg-dim))
    (line-number-current-line (:foreground fg-dim :background bg-alt :weight bold))

    ;;;; Search
    (isearch (:background bg-yellow :foreground bg-dark :weight bold))
    (isearch-fail (:background bg-red :foreground fg-main))
    (lazy-highlight (:background bg-active :foreground fg-main))
    (match (:background bg-hover :foreground fg-main :weight bold))

    ;;;; Errors/Warnings/Success
    (error (:foreground fg-err :weight bold))
    (warning (:foreground fg-warning :weight bold))
    (success (:foreground fg-success :weight bold))

    ;;;; Diff/Version Control
    (diff-added (:background bg-added :foreground fg-added))
    (diff-removed (:background bg-removed :foreground fg-removed))
    (diff-changed (:background bg-changed :foreground fg-changed))
    (diff-refine-added (:background green :foreground bg-dark))
    (diff-refine-removed (:background red :foreground bg-dark))
    (diff-header (:background bg-dim :foreground fg-main))
    (diff-file-header (:background bg-alt :foreground blue :weight bold))

    ;;;; Compilation
    (compilation-info (:foreground green))
    (compilation-warning (:foreground yellow))
    (compilation-error (:foreground red))
    (compilation-line-number (:foreground fg-alt))

    ;;;; Show-paren
    (show-paren-match (:background bg-yellow :foreground bg-dark :weight bold))
    (show-paren-mismatch (:background bg-red :foreground fg-main :weight bold))

    ;;;; Additional UI
    (trailing-whitespace (:background red))
    (escape-glyph (:foreground magenta))
    (homoglyph (:foreground magenta))
    (nobreak-space (:inherit escape-glyph :underline t))
    (nobreak-hyphen (:inherit escape-glyph))

    ;;;; Completions
    (completions-annotations (:foreground fg-alt :slant italic))
    (completions-common-part (:foreground blue :weight bold))
    (completions-first-difference (:foreground yellow :weight bold))

    ;;;; Outline/Org headings
    (outline-1 (:foreground blue :weight bold))
    (outline-2 (:foreground purple :weight bold))
    (outline-3 (:foreground cyan :weight bold))
    (outline-4 (:foreground green :weight bold))
    (outline-5 (:foreground yellow :weight bold))
    (outline-6 (:foreground orange :weight bold))
    (outline-7 (:foreground magenta :weight bold))
    (outline-8 (:foreground fg-main :weight bold))
    )
  "Face definitions for ethereal theme.")

;;;; Theme Application Function

(defun ethereal-theme-apply ()
  "Apply ethereal theme faces.

This function is called by ethereal-theme.el to apply all face
definitions with the current palette (including user overrides)."
  (let ((palette (ethereal--get-palette)))
    ;; Convert face definitions to theme specs with resolved colors
    (apply #'custom-theme-set-faces
           'ethereal
           (mapcar (lambda (face-spec)
                     (let ((face-name (car face-spec))
                           (face-attrs (cadr face-spec)))
                       ;; Resolve all color symbols in attributes
                       ;; Format: (face-name ((t attributes)))
                       (list face-name
                             (list (list 't (ethereal--resolve-attrs face-attrs palette))))))
                   ethereal-faces))))

(defun ethereal--resolve-attrs (attrs palette)
  "Resolve color symbols in ATTRS plist using PALETTE.
Returns a new plist with all color symbols resolved to hex values."
  (let ((result '())
        (standard-values '(bold italic normal light unspecified
                          underline t nil)))
    (while attrs
      (let ((key (car attrs))
            (val (cadr attrs)))
        (push key result)
        (push (cond
               ;; Don't resolve standard face attribute values
               ((memq val standard-values) val)
               ;; If it's a potential color symbol, try to resolve it
               ((and (symbolp val) (not (keywordp val)))
                (let ((resolved (ethereal--resolve-color val palette)))
                  ;; If resolution returns same symbol name, it wasn't in palette
                  ;; In that case, return the original symbol
                  (if (equal resolved (symbol-name val))
                      val
                    resolved)))
               ;; If it's a nested plist (like :box), recurse
               ((and (listp val) (not (null val)) (keywordp (car val)))
                (ethereal--resolve-attrs val palette))
               ;; Otherwise return as-is
               (t val))
              result))
      (setq attrs (cddr attrs)))
    (nreverse result)))

(provide 'ethereal-theme-core)

;;; ethereal-theme-core.el ends here
