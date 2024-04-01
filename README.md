# Math-Info-L3-Project
L3 math-info project on recoloring, in algorithmic.

## Members

| Name                | Github username |
| :------------------ | :-------------: |
| Thomas ARROUS       |     @ThomArr     |
| Gabin DUDILLIEU     |    @GabinDDL    |

## Development environment setup

Install [Opam](https://opam.ocaml.org/doc/Install.html), the OCaml
package manager, on your system.

For convenience, we setup a [local](https://opam.ocaml.org/blog/opam-local-switches/) Opam distribution, using the following commands:

```
$ opam init
$ opam switch create . --deps-only --with-doc --with-test
$ eval $(opam env)
```

Configure your favorite text editor, see the [Real World OCaml setup](http://dev.realworldocaml.org/install.html#editor-setup).

You will want to have a working, integrated type-checker in your
editor, as well as type-directed completion. Your favorite text editor
being Emacs, this leads to:

```
$ opam install user-setup tuareg ocamlformat merlin
$ opam user-setup install
```

Make sure that your text editor applies
[OCamlformat](https://ocaml.org/p/ocamlformat/0.22.4/doc/editor_setup.html#editor-setup)
each time a file is modified, this helps settle styling war and avoids
line-noisy patches down the line.

In Emacs, this amounts to adding the following lines to your `.emacs`
configuration file:

```elisp
(require 'ocamlformat)

(add-hook 'tuareg-mode-hook (lambda ()
  (define-key tuareg-mode-map (kbd "C-M-<tab>") #'ocamlformat)
  (add-hook 'before-save-hook #'ocamlformat-before-save)))
```

If need be, you can invoke Dune to re-format the whole codebase:

```
$ dune fmt
```

## Building Math info L3 project

To build the project, type:

```
$ dune build
```

For continuous build, use

```
$ dune build --watch
```

instead.

## Running Math info L3 projet

To run the project, type:

```
$ dune exec prjt_mi_recolor file.txt
```

The file must be in the following format:
```
t l w c d
n11 n12 n13 ... n1l
n21 n22 ... ... ...
n31 ... ... ... ...
... ... ... ... ...
nw1 nw2 nw3 ... nwl
#
m11 m12 m13 ... m1l
m21 m22 ... ... ...
m31 ... ... ... ...
... ... ... ... ...
mw1 mw2 mw3 ... mwl
```
With t the maximal time, l the length of the grid, w the width, c the number of color, d = 0 if the solution has to be displayed and another number if not,
and each variable n the color of node at the position x and y of the initial grid,
smaller than c + 1 and greatest than -1.
\# the separator between the initial grid and the final grid, and each variable m the color of node at the position x and y of the final grid.


## Running the tests

To execute all the tests, type:

```
$ dune runtest
```

This can be combined with continuous build & test, using

```
$ dune runtest --watch
```

