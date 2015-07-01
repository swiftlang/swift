:orphan:

Documentation Comment Syntax
============================

Documentation comments in Swift use Markdown-flavored markup syntax. Swift doc
comments utilize a full-fledged CommonMark_ parser library (known as *cmark* in
the Swift source tree), which is rendered as HTML in Xcode QuickHelp.

.. _CommonMark: http://commonmark.org

Swift Markup syntax is *exactly* CommonMark, *by design*, not only for its
straightforward implementation and completeness but also in the spirit of
Markdown's readability through limited special syntax.

In addition to full Markdown support, the following extensions are supported via
interpretation only after the CommonMark parser has finished with the doc
comment block. These appear as list items at the top level of the comment's
"document". The goal of these "extensions" was *zero changes* to the *cmark*
library. Because they are regular list items, you may nest arbitrary content
underneath them, such as multiple paragraphs, sublists, hyperlinks, etc. and
they will be rendered in QuickHelp correctly.

All of the below extension "keywords" are matched without regard to case.

Parameters
----------

There are two methods of documenting parameters: *Parameter Outlines* and
*Separate Parameter Fields*. You can mix and match these forms as you see fit in
any order or continuity throughout the doc comment.

Parameter Outlines
``````````````````
::

  - Parameters:
    - x: ...
    - y: ...
    - z: ...

Separate Parameter Fields
`````````````````````````
::

  - parameter x: ...
  - parameter y: ...

Returns Field
-------------
This field documents the return value of a function or method. You may specify
multiple ``returns`` items at the top level but only the last one will be
shown in Xcode's QuickHelp.

::

  - returns: ...

Throwing Functions
------------------

Functions that are marked as `throws` should contain the following describing
what kind of errors are thrown and in what situations:

::

  - throws: ...


Field Extensions
----------------
These extensions are also list items at the top level, which will also appear
highlighted in Xcode QuickHelp as first-class citizens.  Specifying more than
one is supported and appear in order inside QuickHelp.

::

  - Attention: ...
  - Author: ...
  - Authors: ...
  - Bug: ...
  - Complexity: ...
  - Copyright: ...
  - Date: ...
  - Experiment: ...
  - Important: ...
  - Invariant: ...
  - Note: ...
  - Postcondition: ...
  - Precondition: ...
  - Remark: ...
  - Remarks: ...
  - Requires: ...
  - See: ...
  - Since: ...
  - Todo: ...
  - Version: ...
  - Warning: ...
