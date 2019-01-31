Emacs Configuration
---

Features

- language server protocol: python, C++
- ein (jupiter notebook)
- lisp: cl, racket, scheme
<!-- - java: eclim, (TODO language server protocol) -->
<!-- - latex: auctex -->
- pdf-tools
- octave
- magit
<!-- - w3m: integrated web-browser -->
<!-- - mail: multiple mails through gnus -->
<!-- - markdown support -->
- mini-buffer: ivy, swiper, counsel
- spell checking: en, el (install aspell-en, -el), langtool

Installation
---

## C++

Install `cquery` (e.g. `yaourt -S cquery`).

## Python

Install `python-language server` (`pip install python-language server`).

## Jupyter Notebooks (ein)

[ein](https://github.com/millejoh/emacs-ipython-notebook)


In order to configure ein you need to generate the jupyter configuration:


` jupyter notebook --generate-config`


and set the c.NotebookApp.password password and port

<!-- ## Java (through eclipse/eclim) -->


<!-- ` packages: eclipse-java, eclim, eclim-git ` -->


<!-- [eclim](http://eclim.org/install.html) -->


<!-- [emacs-eclim](https://github.com/senny/emacs-eclim) -->


## pdf-tools


` package: poppler, poppler-utils `


<!-- ## w3m (web-browser) -->


<!-- ` package: w3m ` -->

## language tools

`packages: wordnet, languagetool`


<!-- ## setup gnus mail authentication -->

<!-- ~/.authinfo.gpg -->

<!-- machine imap.gmail.com login <USER> password <PASSWORD> port 993 -->

<!-- machine smtp.gmail.com login <USER> password <PASSWORD> port 587 ... -->

<!-- In case of multiple mails, additional entries can be added. -->
