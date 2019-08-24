====================
Swift Pygments Lexer
====================

This is a hideous attempt to provide Pygments lexers for parsing both Swift and Swift REPL code listings. They are specifically tailored for code in the white paper.

The code is designed to be completely rewritten when someone has some free time. No programmers were hurt in the making of these lexers, but Python and Regular Expressions suffered grave losses.



Installation
============

**You don't need to install this file to use it with Sphinx** provided
you use a monkeypatch like the one in this project's docs/conf.py

Otherwise, in order to be made available to Pygments, you need to copy
the included `swift.py` file and run the Pygments `_mapping.py` config
script.

You'll need to find out the path to your Pygments install. For me, this was:
/Library/Python/2.7/site-packages/Pygments-1.6rc1-py2.7.egg/pygments/lexers

To find it:

    $ cd /Library/Python
    $ find . -name lexers

Copy the `swift.py` file to this directory.

Execute the Pygments config script:

    $ python _mapping.py

Usage
=====

For pure Swift code use the `swift` lexer, for REPL use `swift-console`.

The white paper is already marked up to use the Swift lexers.

