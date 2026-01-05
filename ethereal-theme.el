;;; ethereal-theme.el --- Ethereal theme -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Michael J. Seiter

;; Author: Michael J. Seiter
;; URL: https://github.com/gigalope/omarchy-emacs-themes
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: faces, themes

;;; Commentary:

;; Ethereal is a dark Emacs theme with deep navy background and warm
;; cream foreground, inspired by the Ethereal theme by Bjarne Øverli.
;;
;; The color palette is derived from:
;; - Ethereal VS Code theme (https://github.com/bjarneo/ethereal-vscode)
;; - Ethereal Neovim theme (https://github.com/bjarneo/ethereal.nvim)
;;
;; Features:
;; - Deep navy background (#060B1E) optimized for low-light environments
;; - Warm cream foreground (#ffcead) for excellent readability
;; - Balanced syntax highlighting with purple, blue, and green accents
;; - Customizable via `ethereal-palette-overrides'
;;
;; Installation:
;;
;;   (add-to-list 'custom-theme-load-path "/path/to/omarchy-emacs-themes")
;;   (load-theme 'ethereal t)
;;
;; Customization example:
;;
;;   (setq ethereal-palette-overrides
;;         '((fg-keyword blue)
;;           (bg-main "#000000")))
;;   (load-theme 'ethereal t)
;;
;; For integration with omarchy-emacs-theme-sync, see:
;; https://github.com/gigalope/omarchy-emacs-theme-sync

;;; Code:

;;;###autoload
(when (and load-file-name (boundp 'custom-theme-load-path))
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(require 'ethereal-theme-core)

(deftheme ethereal
  "A dark theme with deep navy background inspired by Ethereal.

The color palette emphasizes a sophisticated navy/purple aesthetic
with warm cream text, designed for extended coding sessions with
minimal eye strain.")

;; Apply faces from core module
(ethereal-theme-apply)

;; Theme is now ready
(provide-theme 'ethereal)
(provide 'ethereal-theme)

;;; ethereal-theme.el ends here
