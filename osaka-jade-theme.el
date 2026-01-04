;;; osaka-jade-theme.el --- Osaka Jade theme -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Michael J. Seiter

;; Author: Michael J. Seiter
;; URL: https://github.com/gigalope/omarchy-emacs-themes
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: faces, themes

;;; Commentary:

;; Osaka Jade is a dark Emacs theme with jade tones inspired by the
;; Bamboo.nvim "vulgaris" variant.  It features earthy greens and warm
;; tones designed to complement the Omarchy desktop environment.
;;
;; The theme provides:
;; - Carefully chosen colors optimized for extended coding sessions
;; - Support for syntax highlighting across major modes
;; - Customizable color palette via `osaka-jade-palette-overrides'
;; - Semantic color mappings for consistency
;;
;; Installation:
;;
;;   (add-to-list 'custom-theme-load-path "/path/to/omarchy-emacs-themes")
;;   (load-theme 'osaka-jade t)
;;
;; Customization example:
;;
;;   (setq osaka-jade-palette-overrides
;;         '((fg-keyword blue)
;;           (bg-main "#000000")))
;;   (load-theme 'osaka-jade t)
;;
;; For integration with omarchy-emacs-theme-sync, see:
;; https://github.com/gigalope/omarchy-emacs-theme-sync

;;; Code:

;;;###autoload
(when (and load-file-name (boundp 'custom-theme-load-path))
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(require 'osaka-jade-theme-core)

(deftheme osaka-jade
  "A dark theme with jade tones inspired by Bamboo.nvim.

The color palette is derived from the Bamboo.nvim vulgaris variant,
featuring earthy greens, warm oranges, and muted purples.  Designed
to reduce eye strain during long coding sessions while maintaining
clear syntax distinction.")

;; Apply faces from core module
(osaka-jade-theme-apply)

;; Theme is now ready
(provide-theme 'osaka-jade)
(provide 'osaka-jade-theme)

;;; osaka-jade-theme.el ends here
