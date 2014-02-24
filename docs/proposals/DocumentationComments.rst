:orphan:

===============================
Documentation Comments in Swift
===============================

In this document [P1] [P2] [P3] [P4] designate priorities.

March milestone
===============

[P1] Brief documentation and discussion
---------------------------------------

- Markup for a single-paragraph brief documentation.
- Markup for the "discussion" part.

We filter the comment text to find all "normal" paragraphs (not parameter
documentation, not return value documentation etc.)  Paragraphs are separated
by a blank line.  The first such paragraph is the brief description (to show in
code completion, for example), the rest is the "discussion".

Example::

  /// Lorem ipsum dolor sit amet, consectetur adipiscing elit.  Suspendisse             \
  /// ornare, ipsum vitae mattis adipiscing, lorem risus mattis elit, nec               | Brief documentation
  /// imperdiet justo est at nisi.                                                      /
  ///
  /// Quisque venenatis in erat non suscipit. Praesent varius tellus tortor, ut         \
  /// rutrum arcu vestibulum id.                                                        |
  ///                                                                                   | Discussion.
  /// Ut bibendum ligula sit amet arcu euismod sagittis. Morbi pulvinar nisl ac         |
  /// dapibus pharetra.                                                                 /
  func foo() {
  }

Note that we don't use ``\brief`` or ``\discussion`` in this simple case
because the command is redundant.

[P1] Documentation for function parameters
------------------------------------------

Proposed syntax::

  /// \param bar Ipsum vitae mattis adipiscing, lorem risus mattis
  /// elit, nec imperdiet justo est at nisi.
  func foo(bar: Ornare) {
  }

[P1] Documentation for generic parameters
-----------------------------------------

Proposed syntax::

  /// \genericparam T Ipsum vitae mattis adipiscing, lorem risus mattis
  /// elit, nec imperdiet justo est at nisi.
  class Array<T> {
  }

Note: HeaderDoc uses ``templatefield`` for C++ templates.  Using the term
``field`` with regards to template or generic parameters is not correct neither
for C++ not for Swift.

[P1] Documentation for the return value
---------------------------------------

Proposed syntax::

  /// \result The square root of a given number.
  func sqrt(x: Double) -> Double {
  }

  /// \returns the square root of a given number.
  func sqrt(x: Double) -> Double {
  }

  /// \return the square root of a given number.
  func sqrt(x: Double) -> Double {
  }

Discussion questions:

- Do we want just a single tag (``\result``), or do we want aliases?  Aliases
  are helpful to blend together commands and text in cases like this::

    /// \returns 0 on success, non-zero error code on failure.
    func f() -> Int {
    }

- If we want aliases, do we want both ``\returns`` and ``\return``, or just the
  former?  I think only ``\returns`` can be used to write correct prose here.

- Do we want to allow using multiple ``\returns`` commands?  Developers do this
  in Doxygen (it was probably not designed to be this way, but right now it
  works anyway)::

    /// \returns 0 on success.
    /// \returns error code on failure.
    func f() -> Int {
    }

[P2] Documentation for parameters of closures that are function parameters
--------------------------------------------------------------------------

For example, documentation for ``x`` in the following function declaration can
be specified like this::

  /// \param callback Ipsum vitae mattis adipiscing, lorem risus mattis
  /// elit, nec imperdiet justo est at nisi.
  ///
  /// \param x Quisque venenatis in erat non suscipit.
  func foo(callback: (x: Int)->())

For now we propose to use ``\param`` as well.  In GM we will look into using
indentation here, and showing indented in Quick Help::

  /// \param callback Ipsum vitae mattis adipiscing, lorem risus mattis
  /// elit, nec imperdiet justo est at nisi.
  ///
  ///     \param x Quisque venenatis in erat non suscipit.
  func foo(callback: (x: Int)->())

Special character to use in commands
------------------------------------

We should decide if we want to use the ``\`` character or ``@`` on commands,
or if we want something else.  We definitely don't want to allow to use
multiple styles and provoke another style discussion or per-project
preferences.

- We are going to reserve ``\`` anyway as an escape character.  Using ``\`` for
  commands as well will allow us to reserve ``@`` for now and use it for a
  different purpose in future.

- Using ``@`` would be "compatible" with HeaderDoc users, but otherwise it is
  just a waste of a special character.

Note that Clang's documentation parser supports both ``\`` and ``@``, so
"consistency with Clang" is not an argument either way.

A completely different direction.  What about using ``:command:``?  This would
be compatible with ReStructuredText.  For example::

  /// :param: bar Ipsum vitae mattis adipiscing, lorem risus mattis
  /// elit, nec imperdiet justo est at nisi.
  func foo(bar: Ornare) {
  }

  /// :returns: the square root of a given number.
  func sqrt(x: Double) -> Double {
  }

GM milestone
============

[P3] brief part that is longer than one paragraph
-------------------------------------------------

Sometimes the author wants to have brief description to run longer than one
paragraph.

Proposed syntax::

  /// \brief Lorem ipsum dolor sit amet, consectetur adipiscing elit.                   \
  ///                                                                                   | Brief description.
  /// Suspendisse ornare, ipsum vitae mattis adipiscing, lorem risus mattis             |
  /// elit, nec imperdiet justo est at nisi.                                            /
  ///
  /// \discussion Quisque venenatis in erat non suscipit. Praesent varius               \
  /// tellus tortor, ut rutrum arcu vestibulum id.                                      |
  ///                                                                                   | Discussion.
  /// Ut bibendum ligula sit amet arcu euismod sagittis. Morbi pulvinar nisl ac         |
  /// dapibus pharetra.                                                                 /
  func foo() {
  }

Discussion question: ``\brief`` or ``\abstract``?  "``\brief``" is shorter and
has a precedent in Doxygen, "``\abstract``" has precedent in HeaderDoc.

[P4] specifying whether a parameter is logically "out" or "in,out"
------------------------------------------------------------------

Swift supports multiple return values, so this feature is not needed for native
Swift code.  But it could be used for example, for pointers that are trafficed
through C interfaces.

Proposed syntax::

  /// \param [out] extraResult Set to zero if...
  func foo(extraResult: UnsafePointer<Int>) -> Int {
  }

[P4] documentation for parameters of closures that are function parameters
--------------------------------------------------------------------------

Consider using indentation, as described previously.

Features related to the markup
==============================

- [P1] inline, monospaced font
- [P3] inline, bold
- [P3] inline, italic
- [P1] block, monospaced font

HeaderDoc has a precedent for using opening and closing commands for inline
markup::

  /// Passing a negative number to
  /// <tt>
  /// @textblock
  /// func sqrt(x: Double) -> Double
  /// @/textblock
  /// </tt>
  /// is not allowed.

This syntax wastes *five lines* to mark up a few words with monospaced font.

I did a quick search on Clownfish, and ``textblock`` has 71 hits, while
``discussion`` has 6000+.  Basically, if the syntax is that bad, nobody is
going to use it.

Doxygen has a more concise alternative that has another shortcoming, though --
it is limited to a single word::

  /// Passing a negative number to \c sqrt() is not allowed.

For inline markup I explicitly don't want to follow HeaderDoc's or
Doxygen's precedent, because they are not good ones.

Proposed syntax for italics, bold and monospaced text, precedented by
ReStructuredText::

  /// *Text in italics.*  Normal text.  **Text in bold.**
  ///
  /// Passing a negative number to ``func sqrt(x: Double) -> Double`` is not
  /// allowed.

Proposed syntax for any other inline markup, precedented by ReStructuredText::

  /// :role:`text, multiple words allowed`
  ///
  /// Square root of x :sup:`2` is x.

Proposed syntax for monospaced block markup, precedented by ReStructuredText::

  /// Ipsum vitae mattis adipiscing, lorem risus mattis
  /// elit, nec imperdiet justo est at nisi:
  ///
  /// ::
  ///
  ///   func sqrt(x: Float) -> Float {
  ///     return Float(sqrt(Double(x)))
  ///   }

To mark up a paragraph as monospaced block, add a paragraph that contains just
"``::``", and indent the following paragraphs.

Shorthand syntax::

  /// Ipsum vitae mattis adipiscing, lorem risus mattis elit, nec imperdiet
  /// justo est at nisi::
  ///
  ///   func sqrt(x: Float) -> Float {
  ///     return Float(sqrt(Double(x)))
  ///   }

Finish the paragraph with "``::``" and indent the following paragraphs.

Why ReStructuredText?

- The syntax is concise and easy to read in raw form.  This is important
  because programmers are going to read raw comments *a lot*.

- ReStructuredText has a specification (public domain), and other people have
  already though about all the parsing ambiguities and other difficulties that
  arise when implementing parsing essentially free-form text.

- There are existing tools to process ReStructuredText.  While this is not
  helpful for the implementation inside the compiler (we have special needs to
  produce good diagnostics and point back to the source code), this will
  definitely help in future.

Features related to links in markup
===================================

Note that we don't need to implement the *parsing* and *resolving* of
refereneces, just specify what we want to expect there, and treat the reference
as 'inline monospaced' markup.

[P3] Reference a parameter of the current function
--------------------------------------------------

Proposed syntax::

  /// If :p:`flag` is true, then...
  func f(flag: Bool) {
  }

Do we need this feature?  Can we just use inline monospaced markup? ::

  /// If ``flag`` is true, then...
  func f(flag: Bool) {
  }

[P3] Reference any declaration
------------------------------

Proposed syntax::

  /// See also :ref:`cos(Double)`.
  func sin(x: Double) -> Double {
  }

Custom link text can be specified in angle brackets::

  /// See also :ref:`cos(Double) <cosine function>`.
  func sin(x: Double) -> Double {
  }

Use of HTML in markup
=====================

The proposed syntax is powerful enough to express anything, including lists and
tables.  We should just ban using HTML.

HeaderDoc features that we *don't* want
=======================================

Doxygen and HeaderDoc have shortcomings that we should not carry over to Swift.
Among those I would like to explicitly mention:

- Violation of DRY principle (don't repeat yourself).  From HeaderDoc
  documentation::

    /*!
     @function FunctionName
     This is a comment about FunctionName.
    */
    char *FunctionName(int k);

The tool should be smart enough to recognize where the comment is attached to.

- Pointless limitations of commands.  For example, Doxygen inline markup is
  limited to single words::

    /// \em emphasis \c monospaced etc.

Doxygen's brief descriptions are limited to a single paragraph.

Doxygen's parameter documentation is limited to a single paragraph.  (Did not
check HeaderDoc.)

- Strange semantics of ``\brief`` and ``\discussion`` in Doxygen.  Consider
  this example::

    /// Aaa.                      } Discussion, part 1.
    ///
    /// \brief Bbb.               } Brief description.
    ///
    /// Ccc.                      \
    ///                           | Discussion continues here.
    /// \discussion Ddd.          /


