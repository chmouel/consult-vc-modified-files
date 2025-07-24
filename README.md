[![MELPA](https://stable.melpa.org/packages/consult-vc-modified-files-badge.svg)](https://stable.melpa.org/#/consult-vc-modified-files)

# 🔍 consult-vc-modified-files

`consult-vc-modified-files` provides a easy way to list Git-tracked files that have been modified, newly added, or are part of the HEAD commit in a project.

It integrates with Emacs' built-in [vc](https://www.gnu.org/software/emacs/manual/html_node/emacs/Version-Control.html) and
[diff-mode](https://github.com/emacs-mirror/emacs/blob/master/lisp/vc/diff-mode.el) libraries,
as well as the [consult](https://github.com/minad/consult) package for navigation.

## ✨ Features

- 📄 View **locally modified** files in the current Git project
- 📦 List files from the HEAD commit
- 🆕 List newly added (untracked) files separately
- 📋 Show files **staged for commit** in the Git staging area
- �️ Select and explore files from **any past commit** with diff previews
- �🛠️ Customize sources for specific use cases
- 🧭 Navigate the open files with `consult`
- 👁️ Preview the diff or the commit for the current selection
- 🔄 Smart preview window management (no more duplicate preview windows!)
- 🔍 Show the commit message of the modified files in HEAD
- 🧵 Narrow to the right type of files with the `consult-narrow-key` (`>` by
  default):
  - `h` for modified in HEAD
  - `a` added (untracked) files
  - `l` modified locally
  - `c` staged for commit.

> [!CAUTION]
> This only works with Git repositories and no other version control systems.

## 📸 Screenshot

![screenshot-consult-vc-modified-files](https://github.com/user-attachments/assets/6de35d17-4598-4727-a7b2-07dde993f002)

### Preview with git show the HEAD files and git diff the modified files

<https://github.com/user-attachments/assets/daa339f0-0311-4444-a3a7-a720335b6e4d>

### Browse modified files of a commit with `consult-vc-log-select-files`

<https://github.com/user-attachments/assets/8b452648-6298-45d7-8290-6138d3b0cab2>

## 📥 Installation

### Using `use-package` and [MELPA](https://melpa.org/)

```elisp
(use-package consult-vc-modified-files
  :bind
  ;; choose any other key bindings you prefer
  (("C-x v /" . consult-vc-modified-files)
   ("C-x v ." . consult-vc-log-select-files)))
```

## 🚀 Usage

### Browsing Modified Files

Call the interactive function `consult-vc-modified-files`, or use a key binding
like `C-x v /` (if configured with the configuration above).

When invoked, the command show a prompt for selecting files based on customizable sources:

- 🔄 **Modified locally**: Lists locally modified or untracked files
- 🆕 **Added files**: Lists new untracked files
- 📋 **Staged for commit**: Lists files added to the Git staging area
- 📦 **Modified in HEAD**: Lists files modified in the HEAD commit

### Browsing Files from a Specific Commit

Call the interactive function `consult-vc-log-select-files` to:

1. Select a commit from the repository history
2. Choose a file that was modified in that commit
3. See a live diff preview of the changes made to that file in the commit

This is useful for reviewing changes from past commits without having to use a separate git tool.

You can customize the available sources using the
`consult-vc-modified-files-sources` variable.

## 🔮 Preview Features

- 📊 **Live diff preview**: See the changes in each file as you navigate through options
- 🧠 **Commit diff preview**: View exact changes made to files in any historical commit
- 🌈 **Syntax highlighting**: Previews use diff-mode

## ⚙️ Customization

### Configure Sources

You can customize which file categories appear in the selection when using
`consult-vc-modified-files` by setting the `consult-vc-modified-files-sources`
variable. This allows you to control exactly which types of files are presented
in the interface.

For example, if you want to show all modified files *except* those modified in
branch heads:

```elisp
(setq consult-vc-modified-files-sources
      '(consult-vc-modified-files-source-modified-files
        consult-vc-modified-files-source-added-files
        consult-vc-modified-files-source-staged-files))
```

This configuration includes:

- Modified files (tracked files with changes)
- Added files (new untracked files)
- Staged files (changes ready for commit)

But it excludes files modified in branch heads.

### Display Git Commit Messages when showing the HEAD files

You can control whether to show commit message descriptions for files modified
in HEAD with the `consult-vc-modified-files-show-description` customization
option:

```elisp
(setq consult-vc-modified-files-show-description t)  ;; Show descriptions (default is nil)
```

When enabled, this option displays the commit message next to each file modified
in the HEAD commit, providing more context about the changes.

- When `t`: Shows commit message descriptions for HEAD files
- When `nil`: Shows only the filenames without descriptions

### Customize Faces

Adjust the appearance of listed files by customizing:

- 🎨 `consult-vc-modified-files-face`: For locally modified files
- 🎭 `consult-vc-modified-head-files-face`: For files modified in HEAD
- 🚩 `consult-vc-modified-files-added-face`: For new (untracked) files
- 📋 `consult-vc-modified-files-staged-face`: For files staged for commit

## 👥 Authors

### Chmouel Boudjnah

- 🐘 **Fediverse**: [@chmouel@chmouel.com](https://fosstodon.org/@chmouel) (preferred)
- 🐦 **Twitter**: [@chmouel](https://twitter.com/chmouel)
- 📝 **Blog**: [https://blog.chmouel.com](https://blog.chmouel.com)

## 📃 License

This project is licensed under the [GPL-3.0](./LICENSE).
