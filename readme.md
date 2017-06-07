# Emacs Configuration

Features

- C++ IDE (irony, company, rtags, cmake-ide (optional))
- python: elpy, ein (jupiter notebook)
- java: eclim
- lisp: cl, racket, scheme
- latex: auctex, latex-pane-mode (disabled by default use pdf-tools)
- pdf-tools
- octave support
- spell checking: en, gr
- magit
- w3m: integrated web-browser
- mail: multiple mails through gnus
- markdown support
- [org-mode](http://orgmode.org/worg/org-tutorials/org4beginners.html)
- mini-buffer: ivy, smex, swiper
- misc: whitespace cleanup/show, 80 column rule, save desktop
- global bindings in seperate file

## Installation

### Build irony-server


`` M-x irony-install-server ``


### Build and install rtags


`` M-x rtags-instal ``


### Python


[elpy](https://github.com/jorgenschaefer/elpy)


[ein](https://github.com/millejoh/emacs-ipython-notebook)


In order to configure ein you need to generate the jupyter configureation:


`` jupyter notebook --generate-config``


and set the c.NotebookApp.password password and port


[jedi](http://tkf.github.io/emacs-jedi/latest/)


### Java (through eclipse/eclim)


`` packages: eclipse-java, eclim, eclim-git ``


[eclim](http://eclim.org/install.html)


[emacs-eclim](https://github.com/senny/emacs-eclim)


### pdf-tools


``` package: poppler, poppler-utils ```


### w3m (web-browser)


``` package: w3m ```


### setup gnus mail authentication

~/.authinfo.gpg

machine imap.gmail.com login <USER> password <PASSWORD> port 993

machine smtp.gmail.com login <USER> password <PASSWORD> port 587 ...

In case of multiple mails, additional entries can be added.

### find ac-sources (ds-auto-complete.el) for c++ completion


`` echo "" | g++ -v -x c++ -E - ``
