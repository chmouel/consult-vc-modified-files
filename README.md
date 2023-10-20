# consult-vc-modified-files

List git modified or newly added files with vc in a project

## Installation

Using package.el (with emacs29's package-vc.el):

```elisp
(unless (package-installed-p 'consult-vc-modified-files)
  (package-vc-install "https://github.com/chmouel/consult-vc-modified-files"))
(use-package consult-vc-modified-files
  :bind
  ("C-x v /" . consult-vc-modified-files))
```

I use [general.el](https://github.com/noctuid/general.el) with a leader key in [evil](https://evil.readthedocs.io/en/latest/overview.html#) so I define them like this:


```elisp
(use-package consult-vc-modified-files
  :general (general-leader '(normal) "sm" #'(consult-vc-modified-files :wk "Modified files")))
```

## Usage

Use the interactive function `consult-vc-modified-files` or bind it to a key
(`C-x v /` if you use the installation method above).

## Copyright

### License

[GPL-3.0](./LICENSE)

## Authors

### Chmouel Boudjnah

- Fediverse - <[@chmouel@chmouel.com](https://fosstodon.org/@chmouel)>
- Twitter - <[@chmouel](https://twitter.com/chmouel)>
- Blog - <[https://blog.chmouel.com](https://blog.chmouel.com)>
