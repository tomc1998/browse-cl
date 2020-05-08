# browse-cl

This repository contains a browser for a new web-based programming language.

This code has only been tested on Linux machines, using Steel Bank Common Lisp

To start the browser, the code must first be loaded into a Common Lisp image.
Then, the function `browse-cl:open-window` should be called, followed by
`browse-cl:main`.

Loading the project is managed by `asdf`, which is usually present in most
modern Common Lisp distributions.

## Running the project

Open a Lisp REPL.

```lisp
(load "browse-cl.lisp")
(asdf:load-system :browse-cl)
(in-package :browse-cl)
(open-window)
(main)
```

## Usage

Pressing `f5` will cause the browser to connect to `http://localhost:9898/`.
Whatever data is returned from this request is interpreted as code, and
displayed in the browser window.

Some example programs are provided in the `test-pages/` repository.
