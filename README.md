# epx -- Emacs Project eXecutor


Run and manage per-project shell commands with project.el


# Overview

`epx.el` helps you define, manage, and run shell commands specific to
your Emacs projects (those recognized by `project.el`).

It stores named commands, along with optional environment variables and
a flag to use `compilation-mode`, directly in each project's
`/.dir-locals.el` file. You get interactive commands to easily add,
remove, and run these project tasks.

# Features

-   Completion prompt for commands defined in the current project.
-   Re-uses an existing **shell** window or opens one with
    `project-shell`.
-   Can run the command in `compilation-mode` instead of a
    shell.
-   Stores everything as plain Elisp in `.dir-locals.el` .
-   Pure Elisp, only depends on built-in `project`.

# Installation

Ensure `epx.el` is available on your Emacs `load-path`. A common way
using `use-package` (assuming `project.el` is already set up) is:

```
(use-package epx
  :after project
  :bind
  ("C-c p r" . #'epx-run-command-in-shell)
  ("C-c p c" . #'epx-add-command)
  ("C-c p d" . #'epx-remove-command))
```

# Usage

-`epx-add-command`
:   Prompt for a command, name, env vars, and whether to use a
    compilation buffer; writes it to `.dir-locals.el`.

-`epx-run-command-in-shell`
:   Pick a saved command and execute it in a shell (or compilation)
    buffer scoped to the project.

-`epx-remove-command`
:   Remove a saved command from `.dir-locals.el`.

# Example entry in .dir-locals.el

```
((nil . ((local-project-cmds ;; This is the variable epx looks for
          . ((:name "lint"
              :command "ruff check ."
              :compile t)))))
```

With this entry present in your project root's `/.dir-locals.el` (and
after you approve loading local variables), running
`M-x epx-run-command-in-shell RET lint RET` will execute `ruff check .`
in a `compilation-mode` buffer, using your project's root as the
working directory.

# Contributing

Patches, bug reports, and suggestions are welcome! Please use the
SourceHut mailing list or issue tracker:
<https://git.sr.ht/~alex-iam/epx>.

# License

GPL v3 or later. See the LICENSE file for details.
