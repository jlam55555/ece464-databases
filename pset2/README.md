# Problem set 2: Web scraping and MongoDB

---

### Project setup
The [assignment file][assignment] can be found in the [res][res] directory.

TODO.....

##### Prerequisites
Common Lisp (`sbcl`) and QuickLisp are required for this project. On Debian:
```bash
$ apt install sbcl cl-quicklisp
$ sbcl
> ;;; in the SBCL REPL
> (quicklisp-quickstart:install)
```

---

### Build instructions

##### Interactive (REPL) instructions
The path to the quicklisp installation may have to be customized for your system.
```bash
$ sbcl --load ~/quicklisp/setup.lisp --load scraper.lisp
> ; interact with scraper library here
```

Of course, using a purpose-built Common Lisp environment (SLIME) is a better experience than using the REPL directly.

##### Compilation instructions
TODO: https://stackoverflow.com/q/14171849/2397327

[assignment]: ./res/pset2_assignment.md
[res]: ./res/
