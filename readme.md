Emacs Configuration
---

Features

- C++ IDE (completion: company-irony, company-irony-c-headers, tags:
  rtags, syntax analyzer: flycheck-irony, building: cmake-ide, docs:
  irony-eldoc)
- python: elpy, ein (jupiter notebook)
- java: eclim
- lisp: cl, racket, scheme
- latex: auctex
- pdf-tools
- octave support
- spell checking: en, gr
- magit
- w3m: integrated web-browser
- mail: multiple mails through gnus
- markdown support
- mini-buffer: ivy, smex, swiper

Installation
---

## C++

Build irony server


`` M-x irony-install-server ``


Build rtags


`` M-x rtags-instal ``


## Python


[elpy](https://github.com/jorgenschaefer/elpy)

``elpy-config``

[ein](https://github.com/millejoh/emacs-ipython-notebook)


In order to configure ein you need to generate the jupyter configureation:


`` jupyter notebook --generate-config``


and set the c.NotebookApp.password password and port


[jedi](http://tkf.github.io/emacs-jedi/latest/)


## Java (through eclipse/eclim)


`` packages: eclipse-java, eclim, eclim-git ``


[eclim](http://eclim.org/install.html)


[emacs-eclim](https://github.com/senny/emacs-eclim)


## pdf-tools


``` package: poppler, poppler-utils ```


## w3m (web-browser)


``` package: w3m ```


## setup gnus mail authentication

~/.authinfo.gpg

machine imap.gmail.com login <USER> password <PASSWORD> port 993

machine smtp.gmail.com login <USER> password <PASSWORD> port 587 ...

In case of multiple mails, additional entries can be added.
