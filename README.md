[![MELPA](https://melpa.org/packages/consult-vc-modified-files-badge.svg)](https://melpa.org/#/consult-vc-modified-files)

# 🔍 consult-vc-modified-files

`consult-vc-modified-files` provides a easy way to list Git-tracked files that have been modified, newly added, or are part of the HEAD commit in a project.

It integrates with Emacs' built-in [vc](https://www.gnu.org/software/emacs/manual/html_node/emacs/Version-Control.html) and
[project](https://github.com/emacs-mirror/emacs/blob/master/lisp/progmodes/project.el) libraries,
as well as the [consult](https://github.com/minad/consult) package for navigation.

## ✨ Features

- 📄 View **locally modified** and **newly added** files in the current Git project
- 📦 List files from the HEAD commit
- 📋 Show files **staged for commit** in the Git staging area
- 🛠️ Customize sources for specific use cases
- 🧭 Navigate the open files with `consult`
- 👁️ Preview the diff or the commit for the current selection
- 🔄 Smart preview window management (no more duplicate preview windows!
- 🧵 Narrow to the right type of files with the `consult-narrow-key` (`>` by
  default):
  * `h` for modified in HEAD
  * `l` modified locally
  * `c` staged for commit.

## 📸 Screenshot

![image](https://github.com/chmouel/consult-vc-modified-files/assets/98980/00272a25-a0b1-4b90-b4a8-21807ead914e)

### Preview with git show the HEAD files and git diff the modified files

<https://github.com/user-attachments/assets/daa339f0-0311-4444-a3a7-a720335b6e4d>

## 📥 Installation

### Using `use-package` and [MELPA](https://melpa.org/)

```elisp
(use-package consult-vc-modified-files
  :bind
  ;; choose any other key binding you prefer
  ("C-x v /" . consult-vc-modified-files))
```

## 🚀 Usage

Call the interactive function `consult-vc-modified-files`, or use a key binding
like `C-x v /` (if configured with the configuration above).

When invoked, the command show a prompt for selecting files based on customizable sources:

- 🔄 **Modified locally**: Lists locally modified or untracked files
- 📋 **Staged for commit**: Lists files added to the Git staging area
- 📦 **Modified in HEAD**: Lists files modified in the HEAD commit

You can customize the available sources using the
`consult-vc-modified-files-sources` variable.

## 🔮 Preview Features

- 📊 **Live diff preview**: See the changes in each file as you navigate through options
- 🌈 **Syntax highlighting**: Previews use diff-mode

## ⚙️ Customization

### Configure Sources

Customize `consult-vc-modified-files-sources` to control which file categories appear in the selection. For example:

```elisp
(setq consult-vc-modified-files-sources
      '(consult-vc-modified-source-files
        consult-vc-modified-files-source-staged-files
        consult-vc-modified-source-head-files))
```

### Customize Faces

Adjust the appearance of listed files by customizing:

- 🎨 `consult-vc-modified-files-face`: For locally modified files
- 🎭 `consult-vc-modified-head-files-face`: For files modified in HEAD
- 📋 `consult-vc-modified-files-staged-face`: For files staged for commit

## 👥 Authors

### Chmouel Boudjnah

- 🐘 **Fediverse**: [@chmouel@chmouel.com](https://fosstodon.org/@chmouel) (preferred)
- 🐦 **Twitter**: [@chmouel](https://twitter.com/chmouel)
- 📝 **Blog**: [https://blog.chmouel.com](https://blog.chmouel.com)

## 📃 License

This project is licensed under the [GPL-3.0](./LICENSE).
