# A reasonable Emacs config

Based on: Steve Purcell

## Installation

Build irony-server

```
M-x irony-install-server
```

Build rtags

```
git clone --recursive https://github.com/Andersbakken/rtags.git
cd rtags
cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 .
make
```

### auto-complete-clang

Note that the `ac-clang-flags` set in `init-ac-source.el` is platform-dependent. It's actually clang's include file search path. According to the [Troubleshooting section of auto-complete-clang](https://github.com/brianjcj/auto-complete-clang), you can use the following method to find the correct path:

```
echo "" | g++ -v -x c++ -E -
```

and you'll get something like this:

```
#include "..." search starts here：
#include <...> search starts here：
 /usr/include/c++/4.8
 /usr/include/x86_64-linux-gnu/c++/4.8
 /usr/include/c++/4.8/backward
 /usr/lib/gcc/x86_64-linux-gnu/4.8/include
 /usr/local/include
 /usr/lib/gcc/x86_64-linux-gnu/4.8/include-fixed
 /usr/include/x86_64-linux-gnu
 /usr/include
End of search list.
```
Just use them to replace the corresponding string.

## Important note about `ido`

This config enables `ido-mode` completion in the minibuffer wherever
possible, which might confuse you when trying to open files using
<kbd>C-x C-f</kbd>, e.g. when you want to open a directory to use
`dired` -- if you get stuck, use <kbd>C-f</kbd> to drop into the
regular `find-file` prompt. (You might want to customize the
`ido-show-dot-for-dired` variable if this is an issue for you.)


