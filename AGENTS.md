# AGENTS.md - Omarchy Emacs Themes

## Project Overview

This is an **Emacs theme collection** designed to complement the [Omarchy](https://github.com/basecamp/omarchy) desktop environment. Currently contains one theme:

- **osaka-jade**: Dark theme with jade tones inspired by [Bamboo.nvim](https://github.com/ribru17/bamboo.nvim) vulgaris variant

The package is designed for distribution via MELPA, manual installation, or integration with Doom Emacs.

## Repository Structure

```
omarchy-emacs-themes/
├── osaka-jade-theme.el           # Main theme file (user-facing)
├── osaka-jade-theme-core.el      # Core utilities, palette, face definitions
├── omarchy-emacs-themes-pkg.el   # Package metadata for MELPA
├── README.md                     # User documentation
├── LICENSE                       # MIT License
├── .gitignore                    # Emacs/editor artifacts
└── screenshots/                  # Theme screenshots
```

## Project Type

**Emacs Lisp Theme Package** - No build system, no tests, no CI/CD. Pure Elisp code meant to be loaded directly into Emacs.

## Code Organization

### Two-File Architecture

The theme uses a **core + theme** architecture inspired by [modus-themes](https://github.com/protesilaos/modus-themes):

1. **`osaka-jade-theme-core.el`**
   - Color palette definitions (two-level system)
   - Helper functions for color resolution
   - Face definitions for syntax highlighting and UI elements
   - Theme application logic
   - Customization support

2. **`osaka-jade-theme.el`**
   - Thin wrapper that loads core and registers the theme
   - User-facing entry point
   - Contains `deftheme` declaration

This separation enables:
- Creating theme variants without duplicating face definitions
- Easier maintenance of core utilities
- Clean separation of concerns

### Color Palette System

The palette uses a **two-level hierarchy**:

#### Level 1: Named Colors
Direct hex values from source theme (Bamboo.nvim vulgaris):
```elisp
(bg-main    "#252623")
(fg-main    "#f1e9d2")
(red        "#e75a7c")
(blue       "#57a5e5")
;; etc.
```

#### Level 2: Semantic Mappings
Purpose-based assignments that reference named colors:
```elisp
(fg-keyword    purple)      ; References named color
(fg-string     green)
(bg-region     bg-active)
```

**Why two levels?**
- Named colors define the raw palette
- Semantic mappings define usage intent
- Users can override either level
- Easy to create variants (e.g., light mode version)

### Color Resolution System

Colors are resolved recursively via `osaka-jade--resolve-color`:

1. Hex strings → returned as-is (`"#252623"`)
2. Symbol references → looked up in palette (`purple` → `#aaaaff`)
3. Nested references → resolved recursively (`fg-keyword` → `purple` → `#aaaaff`)

This allows the two-level system to work seamlessly.

## File Header Conventions

All `.el` files follow standard Emacs package conventions:

```elisp
;;; filename.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) YEAR Author Name

;; Author: Author Name
;; URL: https://github.com/user/repo
;; Version: X.Y.Z
;; Package-Requires: ((emacs "27.1"))
;; Keywords: faces, themes

;;; Commentary:
;; ... detailed description ...

;;; Code:

;;;###autoload
(when (and load-file-name (boundp 'custom-theme-load-path))
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;; ... actual code ...

(provide 'feature-name)
;;; filename.el ends here
```

**Critical elements:**
- First line must match: `;;; filename.el --- Description -*- lexical-binding: t; -*-`
- Must end with `(provide 'feature-name)` matching the file
- Must end with `;;; filename.el ends here`
- Always use `lexical-binding: t` (Emacs best practice)
- Triple semicolons `;;;` for top-level section headers
- Double semicolons `;;` for commentary blocks
- Single semicolons `;` for inline comments

**Autoload cookie for themes:**
Theme files (e.g., `osaka-jade-theme.el`) include an `;;;###autoload` block that automatically adds the theme directory to `custom-theme-load-path` when the package is loaded. This allows users to simply `(require 'osaka-jade-theme)` without manually configuring paths.

## Face Definition Pattern

All faces follow this structure in `osaka-jade-faces`:

```elisp
(face-name (:attribute1 value1 :attribute2 value2))
```

Examples:
```elisp
(default (:background bg-main :foreground fg-main))
(font-lock-keyword-face (:foreground fg-keyword :weight bold))
(mode-line (:background bg-mode-line :foreground fg-mode-line
            :box (:line-width 1 :color fg-alt)))
```

**Attribute resolution:**
- Color symbols (e.g., `bg-main`, `fg-keyword`) → resolved via palette
- Standard values (e.g., `bold`, `italic`, `t`, `nil`) → kept as-is
- Nested plists (e.g., `:box`) → recursively resolved

## Customization Support

Users can override palette colors via:

```elisp
(setq osaka-jade-palette-overrides
      '((fg-keyword blue)
        (bg-main "#000000")))
(load-theme 'osaka-jade t)
```

Implementation:
1. `osaka-jade--get-palette` merges user overrides with base palette
2. Overrides applied before face rendering
3. Supports both semantic AND named color overrides

## Naming Conventions

### Color Names

**Background colors:** `bg-*`
- `bg-main` - primary background
- `bg-dim` - dimmer background
- `bg-alt` - alternative background
- `bg-active` - active selection
- `bg-dark` - darker variant

**Foreground colors:** `fg-*`
- `fg-main` - primary text
- `fg-dim` - dimmed text (e.g., comments)
- `fg-alt` - alternative text

**Semantic colors:** `{fg,bg}-{purpose}`
- `fg-keyword`, `fg-string`, `fg-comment`
- `fg-err`, `fg-warning`, `fg-info`
- `bg-region`, `bg-hover`

**UI-specific:** `{fg,bg}-{component}`
- `bg-mode-line`, `fg-mode-line`
- `bg-added`, `fg-added` (diff/git)

### Function Names

All theme functions prefixed with `osaka-jade-`:
- `osaka-jade-theme-apply` - public API
- `osaka-jade--resolve-color` - internal (note double dash)
- `osaka-jade--get-palette` - internal

**Convention:** Double dash `--` indicates private/internal functions.

## Code Style

### Indentation
- **2 spaces** for Elisp indentation
- List items aligned when multi-line

### Docstrings
- All public functions have docstrings
- Internal functions (`--` prefix) have docstrings for maintainability
- Format: One-line summary, then blank line, then details

### Whitespace
- Blank line between function definitions
- Section separators using `;;;;` (four semicolons) for major sections:
  ```elisp
  ;;;; Customization Variables
  ;;;; Palette Definition
  ;;;; Helper Functions
  ```

### Comments
- Use `Commentary:` section for file-level documentation
- Section headers with `;;;;` (four semicolons)
- Subsection headers with `;;;` (three semicolons)
- Block comments with `;;` (two semicolons)
- Inline comments with `;` (single semicolon)

## Common Workflows

### Adding a New Face

1. Add face definition to `osaka-jade-faces` in `osaka-jade-theme-core.el`:
   ```elisp
   (my-new-face (:foreground cyan :weight bold))
   ```

2. Use semantic color references when possible (not direct colors)

3. Test by reloading theme in Emacs:
   ```elisp
   M-x load-theme RET osaka-jade RET
   ```

### Creating a New Theme Variant

1. Copy `osaka-jade-theme.el` to `new-theme-theme.el`
2. Copy `osaka-jade-theme-core.el` to `new-theme-theme-core.el`
3. Update color palette in the core file
4. Update theme name and metadata in wrapper file
5. Update `(provide ...)` statements in both files

### Testing Color Changes

1. Edit palette in `osaka-jade-theme-core.el`
2. In Emacs:
   ```elisp
   M-x load-file RET osaka-jade-theme-core.el RET
   M-x load-file RET osaka-jade-theme.el RET
   ```
3. Or reload via `M-x load-theme RET osaka-jade RET`

## Git Workflow

Standard git workflow - no special conventions observed. Current repo shows:
- Simple commit messages
- Working directly on `main` branch
- No tags yet (version 0.1.0 in package metadata)

## Integration Points

### omarchy-emacs-theme-sync
This package is designed to work with [omarchy-emacs-theme-sync](https://github.com/gigalope/omarchy-emacs-theme-sync), which:
- Automatically switches Emacs themes when Omarchy desktop theme changes
- Prefers native themes (like this package) over approximations

### Doom Emacs
Users can install via `packages.el`:
```elisp
(package! omarchy-emacs-themes
  :recipe (:host github :repo "gigalope/omarchy-emacs-themes"))
```

Then set in `config.el`:
```elisp
(setq doom-theme 'osaka-jade)
```

## Package Distribution

The package includes `omarchy-emacs-themes-pkg.el` for MELPA compatibility:
- Package name: `omarchy-emacs-themes`
- Version: `0.1.0`
- Emacs requirement: `27.1`
- Keywords: `faces`, `themes`

## No Build/Test System

This project has:
- ❌ No Makefile
- ❌ No test suite
- ❌ No CI/CD
- ❌ No linting configuration
- ❌ No byte-compilation scripts

This is normal for simple Emacs themes. Testing is manual (load the theme in Emacs and visually verify).

## Future Themes Planned

Per README, 5 more themes planned:
- everforest
- kanagawa
- ethereal
- hackerman
- matte-black

When adding these, follow the osaka-jade pattern:
1. Create `{theme-name}-theme-core.el` with palette and faces
2. Create `{theme-name}-theme.el` as thin wrapper
3. Update `omarchy-emacs-themes-pkg.el` version

## Gotchas & Important Notes

### 1. Color Resolution Happens at Theme Load Time
User overrides via `osaka-jade-palette-overrides` must be set **before** calling `(load-theme 'osaka-jade t)`. Setting them after has no effect until theme is reloaded.

### 2. Lexical Binding Required
All `.el` files use `lexical-binding: t` - don't remove this. It's required for proper variable scoping in modern Emacs.

### 3. Face Inheritance
Some faces use `:inherit` instead of explicit colors:
```elisp
(font-lock-comment-delimiter-face (:inherit font-lock-comment-face))
```
This maintains consistency when users override parent faces.

### 4. The Palette is a Constant
`osaka-jade-palette` is defined with `defconst`, not `defvar`. User overrides work via `osaka-jade-palette-overrides`, not by modifying the palette directly.

### 5. Two-Element Lists in Palette
Palette entries are two-element lists: `(symbol value)`, not alists/plists:
```elisp
(bg-main "#252623")  ; Correct: (symbol value)
```
The resolution code uses `cadr` to extract the value.

### 6. Standard Face Attribute Values
The resolver has a hardcoded list of standard values that should NOT be resolved as colors:
```elisp
(standard-values '(bold italic normal light unspecified underline t nil))
```
If adding new face attributes with special values, update this list.

### 7. No Dynamic Theme Switching Support
The theme doesn't support Emacs 29's `enable-theme-functions` hook or dynamic reloading on palette changes. Users must manually reload via `M-x load-theme`.

### 8. Git Status Shows Modified File
Current repo state shows `osaka-jade-theme.el` as modified. When working on changes, check git status to see what's actually changed vs. what's just in your working copy.

## Related Documentation

- [modus-themes documentation](https://protesilaos.com/emacs/modus-themes) - Architectural inspiration
- [GNU Emacs Themes Manual](https://www.gnu.org/software/emacs/manual/html_node/emacs/Custom-Themes.html)
- [Bamboo.nvim](https://github.com/ribru17/bamboo.nvim) - Color palette source

## License

MIT License - See LICENSE file
