# Emacs Configuration

Features

- C++ IDE (irony, company, auto-complete, semantics, rtags, cmake-ide)
- python: elpy, ein (jupiter notebook), jedi
- lisp: cl, racket, scheme
- latex: auctex, latex-pane-mode
- octave support
- spell checking: en, gr
- magit
- w3m: integrated web-browser
- mail: [gnus](https://www.emacswiki.org/emacs/GnusGmail)
- markdown support
- [org-mode](http://orgmode.org/worg/org-tutorials/org4beginners.html)
- mini-buffer: ivy, smex, swiper
- misc:  whitespace autocleanup, 80 column rule, save desktop

## Installation

Build irony-server:

```
M-x irony-install-server
```

Build and install rtags:

```
git clone --recursive https://github.com/Andersbakken/rtags.git
cd rtags
cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 .
sudo make install
```

Python:

[elpy](https://github.com/jorgenschaefer/elpy)

[ein](https://github.com/millejoh/emacs-ipython-notebook)

In order to configure ein you need to generate the jupyter configureation:

```
jupyter notebook --generate-config

```

and set the c.NotebookApp.password password.

[jedi](http://tkf.github.io/emacs-jedi/latest/)

pdf-tools:

```
package: poppler, poppler-utils
```

w3m (web-browser):

```
package: w3m
```

setup gnus mail authentication:

~/.authinfo.gpg

machine imap.gmail.com login <USER> password <PASSWORD> port imaps

or

machine imap.gmail.com login <USER> password <PASSWORD> port 993

machine smtp.gmail.com login <USER> password <PASSWORD> port 587

find ac-sources (ds-auto-complete.el) for c++ completion:


```
echo "" | g++ -v -x c++ -E -
```
