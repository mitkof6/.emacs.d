# Emacs Configuration

Features

- language server protocol: python, C++, ...
- ein (jupiter notebook)
- lisp: cl, racket, scheme
- pdf-tools
- octave
- magit
- mini-buffer: ivy, swiper, counsel
- spell checking: en, el (install aspell-en, -el), langtool

# Installation

## C++

Install `clangd`.

## Python

Install `python-language server`.

## Jupyter Notebooks (ein)

[ein](https://github.com/millejoh/emacs-ipython-notebook)


In order to configure ein you need to generate the jupyter configuration:


` jupyter notebook --generate-config`


and set the c.NotebookApp.password password and port

## pdf-tools


` package: poppler, poppler-utils `


## language tools

`packages: wordnet, languagetool`
