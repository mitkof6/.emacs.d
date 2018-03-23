Emacs Configuration
---

Features

- C++ language server protocol
- python: ein (jupiter notebook), language server protocol
- java: eclim, (TODO language server protocol)
- lisp: cl, racket, scheme
- latex: auctex
- pdf-tools
- octave support
- spell checking: en, el (install aspell-en, -el)
- magit
- w3m: integrated web-browser
- mail: multiple mails through gnus
- markdown support
- mini-buffer: ivy, smex, swiper

Installation
---

## C++

Install `cquery` (e.g. `yaourt -S cquery`).


## C++ (deprecated)

This is kept as it is for those interested in configuring rtags, irony,
etc. (see lisp/unused/ds-cpp.el).

Build irony server


` M-x irony-install-server `


Build rtags


` M-x rtags-instal `


## Python

Install `python-language server` (`pip install python-language server`).

[elpy (deprecated)](https://github.com/jorgenschaefer/elpy)

`elpy-config`


[ein](https://github.com/millejoh/emacs-ipython-notebook)


In order to configure ein you need to generate the jupyter configuration:


` jupyter notebook --generate-config`


and set the c.NotebookApp.password password and port


[jedi](http://tkf.github.io/emacs-jedi/latest/)


## Java (through eclipse/eclim)


` packages: eclipse-java, eclim, eclim-git `


[eclim](http://eclim.org/install.html)


[emacs-eclim](https://github.com/senny/emacs-eclim)


## pdf-tools


` package: poppler, poppler-utils `


## w3m (web-browser)


` package: w3m `


## setup gnus mail authentication

~/.authinfo.gpg

machine imap.gmail.com login <USER> password <PASSWORD> port 993

machine smtp.gmail.com login <USER> password <PASSWORD> port 587 ...

In case of multiple mails, additional entries can be added.
