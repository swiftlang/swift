Text Formatting in Swift
========================

:Author: Dave Abrahams
:Author: Dave Zarzycki
:Date: 2013-06-26

**Abstract:** We propose a unified system for creating textual
representations of Swift objects.  Our proposal is inspired by and
respects the strengths of ``printf``, but improves on it in several
important ways.

What's Great about ``printf``
-----------------------------

The ``printf`` interface has three great strengths:

1. C programmers already know it by heart and use it reflexively

2. It is nice and compact.  By keeping the format string together it
   avoids the cost in extra quotes and whitespace incurred by other
   interfaces.

3. Because the format string acts as a template, it allows you to
   focus on the overall structure of your message, without being
   distracted by details of how the substitutions are computed.

What We'd Like to Improve about Swift's ``printf``
--------------------------------------------------

Still, there are problems to solve for both of ``printf``\ 's major
client groups:

Problems for Users
..................

* It's not (fully) statically checked.  While Swift has already made
  vast improvements over C in the type-safety of ``printf``, it
  relies on the use of formatting characters such as ``d`` and ``x``,
  not all of which apply to every type.  In the current
  implementation, after this project concluded it was unacceptable to
  silently ignore wrong format characters, the implementation was
  changed to ``DebugTrap`` when the user messes up.  Neither behavior
  is ideal; these mistakes can be prevented at compile-time.

* Swift's ``printf`` doesn't support reordering of parameters in the
  format string.  A string resource used for printf formatting should
  be able to express something like “eat *adjective* *noun*”, and be
  transformed into “manger *noun* *adjective*” in a French
  localization, without recompiling the application code that invokes
  ``printf``.  

  .. Note:: POSIX ``printf`` does this with “``%``\ *n*\ ``$``...”,
            where *n* is a 1-based integer.

* Swift's current ``printf`` is arguably too similar to C's
  ``printf``, while being just a little bit different.  This may leave
  habitual users of C's ``printf`` in an awkward “uncanny valley.”

* The full syntax of ``printf`` is a jargon that is comfortable for
  its habitual users, but cryptic and difficult for others.

Problems for Extenders
......................

* Extensibility of format kinds is limited to 2*26 characters, and
  isn't namespaced.  The “``%q``” used by my type might have a
  completely different meaning from the one used by your type.

* The interface to ``FormattedPrintable`` is awkward::

    protocol FormattedPrintable {
      func format(kind : Char, layout : String) -> String
    }

  Layout information from the format string, which has a standard
  meaning and representation, is delivered as a String instead of
  being parsed into useful values.  Each type conforming to
  ``FormattedPrintable`` has to ignore or parse the information
  itself.

* There's no way to satisfy the requirements of
  ``FormattedPrintable`` without rubbing up against “``kind``” and
  “``layout``,” at least enough to learn that you can ignore them.
  Delivering printability for a type should be as simple as building
  a ``String`` representation.

* Because all formatting styles are routed through a single function,
  there's no way to implement one of the format styles for a type
  (such as “``%x``”) without implementing all of them.

The Fix
-------

Our plan can be summarized like this:

* Keep width, precision, and alignment in the format string.  These
  features of ``printf`` have a reasonable interpretation
  for all argument types.
* Replace the “``kind``” flag, which is tied to the type of the
  argument, with a (zero-based) argument index.
* Move all type-specific formatting directives into the argument list
  (details to follow).

For example, ::

  printf("in %s, register 7 = %-5.7llx", name, r7)

would become ::

  printf("in %0, register 7 = %-5.7:1", name, r7 % hex)

Format String Syntax
....................

We propose the following grammar for *format directive*\ s, with
simple max-munch semantics. 

  | *format-directive* := ``%`` ( *format-directive-body* | *braced-format-directive-body* )
  | *braced-format-directive-body* := ``{`` *format-directive-body* ``}``
  | *format-directive-body* := *format-flags*\ ? *format-argument-index*
  | *format-flags* := ``-``\ ? *format-width* ? *format-precision*\ ? ``:``
  | *format-width* := [\ ``0``\ -\ ``9``\ ]+
  | *format-precision* := ``.``\ [\ ``0``\ -\ ``9``\ ]+
  | *format-argument-index* := [\ ``0``\ -\ ``9``\ ]+

This syntax optimizes for the 90% case where one simply wants the
default representation for each argument, with the advantage of
positional selection.  In the 10% case where width, precision, and/or
alignment are required, the user must type one additional character,
the colon, to separate the *format-flags* from the
*format-argument-index*.  We realize that the colon *could* be dropped
when only alignment is specified, but we think it serves clarity to
require it whenever *format-flags* are specified.  The optional braces
handle the 1% of cases where the *format-directive*\ is followed by a
number, and help make room in the syntax for future extension.

Type-Specific Formatting
........................

Formatting options like numeric radix, which apply only to certain
types, can appear in the printf argument list.  We suggest using the
``%`` operator to chain format modifications.  This allows us to
clearly distinguish potentially-lazy computations, like ::

  some_really_long_string % uppercase

from their more-eager cousins, which could be too
inefficient/memory-hungry for longer formatting.

Conforming to Printable
.......................

The simple story for beginners is this: 

  “to make your type ``Printable``, simply declare conformance to
  ``Printable``::

    extension Person : Printable {}

  and you'll get the same representation you see in the interpreter
  (REPL).  To customize the representation, give your type a ``func
  printFormat()`` that returns a ``String``::

    extension Person : Printable {
      func printFormat() -> String {
        return "\(lastName), \(firstName)"
      }
    }

The design of the formatting protocols (below) allows more
sophisticated and efficient formatting as a natural extension of this
simple story.

Framework Details
-----------------

Output Streams
..............

The most fundamental part of the framework is
``FormattedOutputStream``, a thing into which we can stream text::

  protocol FormattedOutputStream {
    func append(text: String)
  }

.. Note:: We don't support streaming ``Char`` (a.k.a. ``CodePoint``)
   directly because it's possible to create invalid sequences of code
   points.

Every String can be used as a FormattedOutputStream directly::

  extension String: FormattedOutputStream {
    func append(text: String)
  }

Debug Printing
..............

Via compiler magic, everything conforms to the ``DebugPrintable``
protocol.  To change the debug representation for a type, you don't
need to declare conformance: simply give the type a ``debugFormat()``
::

  /// \brief A thing that can be printed in the REPL and the Debugger
  protocol DebugPrintable {
    typealias DebugFormatter: Formattable = String

    /// \brief Produce a textual representation for the REPL and
    /// Debugger.
    func debugFormat() -> DebugFormatter
  }

Because ``String`` is a ``Formattable``, your implementation of
``debugFormat`` can just return a ``String``.  If you don't like
``String``\ 's default response to width, precision, and/or alignment,
or if you want to write directly to the ``FormattedOutputStream``
for efficiency reasons, (e.g. if your representation is huge),
you can return a custom ``DebugFormatter`` type.

.. Note:: producing a representation that can be consumed by the REPL
   to produce an equivalent object is strongly encouraged where
   possible!  For example, ``String.debugFormat()`` produces a
   representation starting and ending with “``"``”, where special
   characters are escaped, etc.  A ``struct Point { var x, y: Int }``
   might be represented as “``Point(x: 3, y: 5)``”.

(Non-Debug) Printing
....................

The ``Printable`` protocol provides a "pretty" textual representation
that can be distinct from the debug format.  For example,
``String.printFormat()`` returns the string itself, without quoting.

Conformance to ``Printable`` is explicit, but if you want to use the
``debugFormat()`` results for your type's ``printFormat()``, all you
need to do is declare conformance to ``Printable``; there's nothing to
implement.

.. Note:: explicitness here keeps us from automatically polluting
   completion results for every type with ``printFormat()`` and
   ``toString()`` functions.

::

  /// \brief A thing that can be print()ed and toString()ed.
  protocol Printable: DebugPrintable {
    typealias PrintFormatter: Formattable = DebugFormatter

    /// \brief produce a "pretty" textual representation.
    ///
    /// In general you can return a String here, but if you need more
    /// control, we strongly recommend returning a custom Formatter
    /// type, e.g. a nested struct of your type.  If you're lazy, you
    /// can conform to Formattable directly and just implement its
    /// write() func.
    func printFormat() -> PrintFormatter {
      return debugFormat()
    }

    /// \brief Simply convert to String
    ///
    /// Don't reimplement this: the default implementation always works.
    /// If you must reimplement toString(), make sure its results are
    /// consistent with those of printFormat() (i.e. you shouldn't
    /// change the behavior).
    func toString() -> String {
      var result: String
      this.printFormat().write(result)
      return result
    }
  }

``Formattable``
...............

For full control we provide ``Formattable``, a thing that can write
into a ``FormattedOutputStream`` while responding to width, precision,
and alignment.  Every ``Formattable`` is also a ``Printable``,
naturally. ::

  protocol Formattable: Printable {
    func write(
      target: [byref] FormattedOutputStream, 
      width: Int? = None, precision: Int? = None, 
      right_align: Bool? = None)

    // You'll never want to reimplement this
    func printFormat() -> PrintFormatter {
      return this
    }

    /// \brief get the debug representation.  
    ///
    /// A Formattable will usually want to override this, so that in the
    /// debugger and REPL, it doesn't appear to be the thing on whose
    /// behalf it is formatting.
    func debugFormat() -> DebugFormatter {
      return this
    }
  }

How ``String`` Fits In
......................

Making ``String`` conform to ``Formattable`` is the key to an
easy-to-use interface that still provides full control.

.. parsed-literal::

  extension String: Formattable {
    func write(
      target: [byref] FormattedOutputStream, knobs
      width: Int? = None, precision: Int? = None, 
      right_align: Bool? = None
    ) {
      *...*
    }

    func debugFormat() -> String {
      *...escape all the CodePoints...*
    }

    // Swift may get us this default automatically from the protocol,
    // but it's here as a reminder of how String behaves.
    func printFormat() -> String {
      return this
    }
  }

Conclusion
==========

We've proposed an interface that:

* Will be familiar to users of printf (we're not changing format
  string introducers, width, alignment, or precision specifiers)
* Turns potential runtime errors into compile-time errors.
* Makes simple cases simple and uncommon cases more explicit
* Offers major bang for the buck:
 - handles an important element of the internationalization picture
 - the common case uses the same number of characters
 - the case where you want to do formatting costs one extra character
   (``:``)

Options and Bike Sheds
----------------------

* Use something other than ``:`` to separate flags from argument
  number

* Use something other than ``{``\ … ``}`` to override max-munch
  parsing.

* Use something other than ``%`` to denote format modifiers.
  ``x.format.uppercase()`` is a possibility.

* Provide a type-safe binding directly to posix's printf in the posix
  module, for those who need that interface.

  .. Note:: this interface will not be as type-safe as Swift's
            ``printf``; more errors will only be caught at runtime

* Allow ``String``\ s to serve as format modifiers for some types,
  with POSIX semantics.  This would allow, e.g. 
  
  .. parsed-literal::

    printf("in %0, register 7 = %1", **name % "s"**, **r7 % "-5.7llx"**)

  .. Note:: reduced type-safety applies here as well, but less-so
            because we can *choose* the argument types for which
            ``String`` can act as a format modifier.
