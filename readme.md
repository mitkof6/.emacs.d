# An Emacs config

Based on: Steve Purcell and xyguo

Features

- C++ IDE (irony, company, auto-complete, semantics, rtags, cmake-ide)
- python jedi, ein, elpy, jython (TODO)
- lisp: sline, racket-mode
- latex: auctex
- spellchecking: en, gr
- magit
- markdown
- whitespace: autocleanup, 80 column rule
- save desktop
- helm
- ido


## Installation

Build irony-server

```
M-x irony-install-server
```

Build and install rtags

```
git clone --recursive https://github.com/Andersbakken/rtags.git
cd rtags
cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 .
sudo make install
```

Add yasnippet

```
git clone https://github.com/AndreaCrotti/yasnippet-snippets.git ~/.emacs.d/snippets/
```

Python

```
virutalenv

"M-x jedi:install-server"
```

### Auto-complete-clang

Note that the `ac-clang-flags` set in `init-ac-source.el` is
platform-dependent. It's actually clang's include file search
path. According to
the
[Troubleshooting section of auto-complete-clang](https://github.com/brianjcj/auto-complete-clang),
you can use the following method to find the correct path:

```
echo "" | g++ -v -x c++ -E -
```

and you'll get something like this:

```
/usr/include/c++/4.8
/usr/include/x86_64-linux-gnu/c++/4.8
/usr/include/c++/4.8/backward
/usr/lib/gcc/x86_64-linux-gnu/4.8/include
/usr/local/include
/usr/lib/gcc/x86_64-linux-gnu/4.8/include-fixed
/usr/include/x86_64-linux-gnu
/usr/include
```
Just use them to replace the corresponding string.

## Important note about `ido`

This config enables `ido-mode` completion in the minibuffer wherever
possible, which might confuse you when trying to open files using
<kbd>C-x C-f</kbd>, e.g. when you want to open a directory to use
`dired` -- if you get stuck, use <kbd>C-f</kbd> to drop into the
regular `find-file` prompt. (You might want to customize the
`ido-show-dot-for-dired` variable if this is an issue for you.)
