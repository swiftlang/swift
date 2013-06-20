.. @raise litre.TestsAreMissing

Swift Coding Standards
======================

This document describes coding standards that apply to code in Swift.  It does
not apply to the C++ code that implements the compiler.

Column limit
------------

The preferred coding style is to reduce indentation as much as possible, using
early returns and factoring deeply nested code out into helper functions.
However, there is no fixed source file width.

Indentation
-----------

Indentation is 2 spaces.  Use spaces instead of tabs.

Don't indent ``get:`` or ``set:`` inside a property implementation:

::
  struct Foo {
    var _barTimes2 : Int

    var bar : Int {
    get:
      return _barTimes2 / 2
    set(value):
      _barTimes2 = bar * 2
    }
  }

Line breaks
-----------

Statements and declarations that start with a keyword should always be on their own line:

::
  // Correct:
  if foo() {
    doBar()
  }

  // Wrong:
  if foo() { doBar() }

You can put function body on the same line as the function declaration if it fits:

::
  func foo() -> Int { return doBar() + 1 }

Computed vars
-------------

Omit ``get:`` inside a computed var if it is read-only:

::
  struct Foo {
    var _bar : Int
    var bar : Int {
      return _bar
    }
  }

Private struct and class members
--------------------------------

Until Swift implements access control or visibility, names of "private" struct
and class members should start with a leading underscore.

