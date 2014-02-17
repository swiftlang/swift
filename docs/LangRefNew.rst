.. @raise litre.TestsAreMissing

===============================
Swift Language Reference Manual
===============================

.. contents::

Introduction
============

.. admonition:: Commentary

  In addition to the main spec, there are lots of open ended questions,
  justification, and ideas of what best practices should be.  That random
  discussion is placed in boxes like this one to clarify what is normative and
  what is discussion.

This is the language reference manual for the Swift language, which is highly
volatile and constantly under development.  As the prototype evolves, this
document should be kept up to date with what is actually implemented.

The grammar and structure of the language is defined in BNF form in yellow
boxes.  Examples are shown in gray boxes, and assume that the standard library
is in use (unless otherwise specified).

Basic Goals
-----------

.. admonition:: Commentary

  A non-goal of the Swift project in general is to become some amazing research
  project.  We really want to focus on delivering a real product, and having
  the design and spec co-evolve.

In no particular order, and not explained well:

* Support building great frameworks and applications, with a specific focus on
  permiting rich and powerful APIs.
* Get the defaults right: this reduces the barrier to entry and increases the
  odds that the right thing happens.
* Through our support for building great APIs, we aim to provide an expressive
  and productive language that is fun to program in.
* Support low-level system programming.  We should want to write compilers,
  operating system kernels, and media codecs in Swift.  This means that being
  able to obtain high performance is really quite important.
* Provide really great tools, like an IDE, debugger, profiling, etc.
* Where possible, steal great ideas instead of innovating new things that will
  work out in unpredictable ways.  It turns out that there are a lot of good
  ideas already out there.
* Memory safe by default: array overrun errors, uninitialized values, and other
  problems endemic to C should not occur in Swift, even if it means some amount
  of runtime overhead.  Eventually these checks will be disablable for people
  who want ultimate performance in production builds.
* Efficiently implementable with a static compiler: runtime compilation is
  great technology and Swift may eventually get a runtime optimizer, but it is
  a strong goal to be able to implement swift with just a static compiler.
* Interoperate as transparently as possible with C, Objective-C, and C++
  without having to write an equivalent of "extern C" for every referenced
  definition.
* Great support for efficient by-value types.
* Elegant and natural syntax, aiming to be familiar and easy to transition to
  for "C" people.  Differences from the C family should only be done when it
  provides a significant win (e.g. eliminate declarator syntax).
* Lots of other stuff too.

A smaller wishlist goal is to support embedded sub-languages in swift, so that
we don't get the OpenCL-is-like-C-but-very-different-in-many-details
problem.

Basic Approach
--------------

.. admonition:: Commentary

  Pushing as much of the language as realistic out of the compiler and into the
  library is generally good for a few reasons: 1) we end up with a smaller core
  language.  2) we force the language that is left to be highly expressive and
  extensible.  3) this highly expressive language core can then be used to
  build a lot of other great libraries, hopefully many we can't even anticipate
  at this point.

The basic approach in designing and implementing the Swift prototype was to
start at the very bottom of the stack (simple expressions and the trivial bits
of the type system) and incrementally build things up one brick at a time.
There is a big focus on making things as simple as possible and having a clean
internal core.  Where it makes sense, sugar is added on top to make the core
more expressive for common situations.

One major aspect that dovetails with expressivity, learnability, and focus on
API development is that much of the language is implemented in a :ref:`standard
library <langref.stdlib>` (inspired in part by the Haskell Standard Prelude).
This means that things like ``Int`` and ``Void`` are not part of the language
itself, but are instead part of the standard library.

Phases of Translation
=====================

.. admonition:: Commentary

  Because Swift doesn't rely on a C-style "lexer hack" to know what is a type
  and what is a value, it is possible to fully parse a file without resolving
  import declarations.

Swift has a strict separation between its phases of translation, and the
compiler follows a conceptually simple design.  The phases of translation
are:

* :ref:`Lexing <langref.lexical>`: A source file is broken into tokens
  according to a (nearly, ``/**/`` comments can be nested) regular grammar.

* Parsing and AST Building: The tokens are parsed according to the grammar set
  out below.  The grammar is context free and does not require any "type
  feedback" from the lexer or later stages.  During parsing, name binding for
  references to local variables and other declarations that are not at module
  (and eventually namespace) scope are bound.

* :ref:`Name Binding <langref.namebind>`: At this phase, references to
  non-local types and values are bound, and :ref:`import directives
  <langref.decl.import>` are both validated and searched.  Name binding can
  cause recursive compilation of modules that are referenced but not yet built.

* :ref:`Type Checking <langref.typecheck>`: During this phase all types are
  resolved within value definitions, :ref:`function application
  <langref.expr.call>` and <a href="#expr-infix">binary expressions</a> are
  found and formed, and overloaded functions are resolved.

* Code Generation: The AST is converted the LLVM IR, optimizations are
  performed, and machine code generated.

* Linking: runtime libraries and referenced modules are linked in.

FIXME: "import Swift" implicitly added as the last import in a source file.

.. _langref.lexical:

Lexical Structure
=================

.. admonition:: Commentary

  Not all characters are "taken" in the language, this is because it is still
  growing.  As there becomes a reason to assign things into the identifier or
  punctuation bucket, we will do so as swift evolves.

The lexical structure of a Swift file is very simple: the files are tokenized
according to the following productions and categories.  As is usual with most
languages, tokenization uses the maximal munch rule and whitespace separates
tokens.  This means that "``a b``" and "``ab``" lex into different token
streams and are therefore different in the grammar.

.. _langref.lexical.whitespace:

Whitespace and Comments
-----------------------

.. admonition:: Commentary

  Nested block comments are important because we don't have the nestable ``#if
  0`` hack from C to rely on.

.. code-block:: none

  whitespace ::= ' '
  whitespace ::= '\n'
  whitespace ::= '\r'
  whitespace ::= '\t'
  whitespace ::= '\0'
  comment    ::= //.*[\n\r]
  comment    ::= /* .... */

Space, newline, tab, and the nul byte are all considered whitespace and are
discarded, with one exception:  a '``(``' or '``[``' which does not follow a
non-whitespace character is different kind of token (called *spaced*)
from one which does not (called *unspaced*).  A '``(``' or '``[``' at the
beginning of a file is spaced.

Comments may follow the BCPL style, starting with a "``//``" and running to the
end of the line, or may be recursively nested ``/**/`` style comments.  Comments
are ignored and treated as whitespace.

.. _langref.lexical.reserved_punctuation:

Reserved Punctuation Tokens
---------------------------

.. admonition:: Commentary

  Note that ``->`` is used for function types ``() -> Int``, not pointer
  dereferencing.

.. code-block:: none

  punctuation ::= '('
  punctuation ::= ')'
  punctuation ::= '{'
  punctuation ::= '}'
  punctuation ::= '['
  punctuation ::= ']'
  punctuation ::= '.'
  punctuation ::= ','
  punctuation ::= ';'
  punctuation ::= ':'
  punctuation ::= '='
  punctuation ::= '->'
  punctuation ::= '&' // unary prefix operator

These are all reserved punctuation that are lexed into tokens.  Most other
non-alphanumeric characters are matched as :ref:`operators
<langref.lexical.operator>`.  Unlike operators, these tokens are not
overloadable.

Reserved Keywords
-----------------

.. admonition:: Commentary

  The number of keywords is reduced by pushing most functionality into the
  library (e.g. "builtin" datatypes like ``Int`` and ``Bool``).  This allows us
  to add new stuff to the library in the future without worrying about
  conflicting with the user's namespace.

.. code-block:: none

  // Declarations and Type Keywords
  keyword ::= 'class'
  keyword ::= 'destructor'
  keyword ::= 'extension'
  keyword ::= 'import'
  keyword ::= 'init'
  keyword ::= 'func'
  keyword ::= 'metatype'
  keyword ::= 'enum'
  keyword ::= 'protocol'
  keyword ::= 'struct'
  keyword ::= 'subscript'
  keyword ::= 'typealias'
  keyword ::= 'var'
  keyword ::= 'where'

  // Statements
  keyword ::= 'break'
  keyword ::= 'case'
  keyword ::= 'continue'
  keyword ::= 'default'
  keyword ::= 'do'
  keyword ::= 'else'
  keyword ::= 'if'
  keyword ::= 'in'
  keyword ::= 'for'
  keyword ::= 'return'
  keyword ::= 'switch'
  keyword ::= 'then'
  keyword ::= 'while'

  // Expressions
  keyword ::= 'as'
  keyword ::= 'is'
  keyword ::= 'new'
  keyword ::= 'super'
  keyword ::= 'self'
  keyword ::= 'Self'
  keyword ::= '__COLUMN__'
  keyword ::= '__FILE__'
  keyword ::= '__LINE__'


These are the builtin keywords.

Contextual Keywords
-------------------

Swift uses several contextual keywords at various parts of the language.
Contextual keywords are not reserved words, meaning that they can be used as
identifiers.  However, in certain contexts, they act as keywords, and are
represented as such in the grammar below.  The following identifiers act as
contextual keywords within the language:

.. code-block:: none

  get
  infix
  operator
  postfix
  prefix
  set
  type

.. _langref.lexical.integer_literal:

Integer Literals
----------------

.. code-block:: none

  integer_literal ::= [0-9][0-9_]*
  integer_literal ::= 0x[0-9a-fA-F][0-9a-fA-F_]*
  integer_literal ::= 0o[0-7][0-7_]*
  integer_literal ::= 0b[01][01_]*

Integer literal tokens represent simple integer values of unspecified
precision.  They may be expressed in decimal, binary with the '``0b``' prefix,
octal with the '``0o``' prefix, or hexadecimal with the '``0x``' prefix.
Unlike C, a leading zero does not affect the base of the literal.

Integer literals may contain underscores at arbitrary positions after the first
digit.  These underscores may be used for human readability and do not affect
the value of the literal.

::

  789
  0789

  1000000
  1_000_000

  0b111_101_101
  0o755

  0b1111_1011
  0xFB

.. _langref.lexical.floating_literal:

Floating Point Literals
-----------------------

.. admonition:: Commentary

  We require a digit on both sides of the dot to allow lexing "``4.km``" as
  "``4 . km``" instead of "``4. km``" and for a series of dots to be an
  operator (for ranges).  The regex for decimal literals is same as Java, and
  the one for hex literals is the same as C99, except that we do not allow a
  trailing suffix that specifies a precision.

.. code-block:: none

  floating_literal ::= [0-9][0-9_]*\.[0-9][0-9_]*
  floating_literal ::= [0-9][0-9_]*\.[0-9][0-9_]*[eE][+-]?[0-9][0-9_]*
  floating_literal ::= [0-9][0-9_]*[eE][+-]?[0-9][0-9_]*
  floating_literal ::= 0x[0-9A-Fa-f][0-9A-Fa-f_]*
                         (\.[0-9A-Fa-f][0-9A-Fa-f_]*)?[pP][+-]?[0-9][0-9_]*

Floating point literal tokens represent floating point values of unspecified
precision.  Decimal and hexadecimal floating-point literals are supported.

The integer, fraction, and exponent of a floating point literal may each
contain underscores at arbitrary positions after their first digits.  These
underscores may be used for human readability and do not affect the value of
the literal.  Each part of the floating point literal must however start with a
digit; ``1._0`` would be a reference to the ``_0`` member of ``1``.

::

  1.0
  1000000.75
  1_000_000.75

  0x1.FFFFFFFFFFFFFp1022
  0x1.FFFF_FFFF_FFFF_Fp1_022

.. _langref.lexical.character_literal:

Character Literals
------------------

.. code-block:: none

  character_literal ::= '[^'\\\n\r]|character_escape'
  character_escape  ::= [\]0 [\][\] | [\]t | [\]n | [\]r | [\]" | [\]'
  character_escape  ::= [\]x hex hex
  character_escape  ::= [\]u hex hex hex hex
  character_escape  ::= [\]U hex hex hex hex hex hex hex hex
  hex               ::= [0-9a-fA-F]

``character_literal`` tokens represent a single character, and are surrounded
by single quotes.

The ASCII and Unicode character escapes:

.. code-block:: none

  \0 == nul
  \n == new line
  \r == carriage return
  \t == horizontal tab
  \u == small Unicode code points
  \U == large Unicode code points
  \x == raw ASCII byte (less than 0x80)

.. _langref.lexical.string_literal:

String Literals
---------------

.. admonition:: Commentary

  FIXME: Forcing ``+`` to concatenate strings is somewhat gross, a proper protocol
  would be better.

.. code-block:: none

  string_literal   ::= ["]([^"\\\n\r]|character_escape|escape_expr)*["]
  escape_expr      ::= [\]escape_expr_body
  escape_expr_body ::= [(]escape_expr_body[)]
  escape_expr_body ::= [^\n\r"()]

``string_literal`` tokens represent a string, and are surrounded by double
quotes.  String literals cannot span multiple lines.

String literals may contain embedded expressions in them (known as
"interpolated expressions") subject to some specific lexical constraints: the
expression may not contain a double quote ["], newline [\n], or carriage return
[\r].  All parentheses must be balanced.

In addition to these lexical rules, an interpolated expression must satisfy the
:ref:`expr <langref.expr>` production of the general swift grammar.  This
expression is evaluated, and passed to the constructor for the inferred type of
the string literal.  It is concatenated onto any fixed portions of the string
literal with a global "``+``" operator that is found through normal name
lookup.

::

  // Simple string literal.
  "Hello world!"

  // Interpolated expressions.
  "\(min)...\(max)" + "Result is \((4+i)*j)"

.. _langref.lexical.identifier:

Identifier Tokens
-----------------

.. code-block:: none

  identifier ::= id-start id-continue*

  // An identifier can start with an ASCII letter or underscore...
  id-start ::= [A-Za-z_]

  // or a Unicode alphanumeric character in the Basic Multilingual Plane...
  // (excluding combining characters, which can't appear initially)
  id-start ::= [\u00A8\u00AA\u00AD\u00AF\u00B2-\u00B5\u00B7-00BA]
  id-start ::= [\u00BC-\u00BE\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u00FF]
  id-start ::= [\u0100-\u02FF\u0370-\u167F\u1681-\u180D\u180F-\u1DBF]
  id-start ::= [\u1E00-\u1FFF]
  id-start ::= [\u200B-\u200D\u202A-\u202E\u203F-\u2040\u2054\u2060-\u206F]
  id-start ::= [\u2070-\u20CF\u2100-\u218F\u2460-\u24FF\u2776-\u2793]
  id-start ::= [\u2C00-\u2DFF\u2E80-\u2FFF]
  id-start ::= [\u3004-\u3007\u3021-\u302F\u3031-\u303F\u3040-\uD7FF]
  id-start ::= [\uF900-\uFD3D\uFD40-\uFDCF\uFDF0-\uFE1F\uFE30-FE44]
  id-start ::= [\uFE47-\uFFFD]

  // or a non-private-use, valid code point outside of the BMP.
  id-start ::= [\u10000-\u1FFFD\u20000-\u2FFFD\u30000-\u3FFFD\u40000-\u4FFFD]
  id-start ::= [\u50000-\u5FFFD\u60000-\u6FFFD\u70000-\u7FFFD\u80000-\u8FFFD]
  id-start ::= [\u90000-\u9FFFD\uA0000-\uAFFFD\uB0000-\uBFFFD\uC0000-\uCFFFD]
  id-start ::= [\uD0000-\uDFFFD\uE0000-\uEFFFD]

  // After the first code point, an identifier can contain ASCII digits...
  id-continue ::= [0-9]

  // and/or combining characters...
  id-continue ::= [\u0300-\u036F\u1DC0-\u1DFF\u20D0-\u20FF\uFE20-\uFE2F]

  // in addition to the starting character set.
  id-continue ::= id-start

  identifier-or-any ::= identifier
  identifier-or-any ::= '_'

The set of valid identifier characters is consistent with WG14 N1518,
"Recommendations for extended identifier characters for C and C++".  This
roughly corresponds to the alphanumeric characters in the Basic Multilingual
Plane and all non-private-use code points outside of the BMP.  It excludes
mathematical symbols, arrows, line and box drawing characters, and private-use
and invalid code points.  An identifier cannot begin with one of the ASCII
digits '0' through '9' or with a combining character.

The Swift compiler does not normalize Unicode source code, and matches
identifiers by code points only.  Source code must be normalized to a consistent
normalization form before being submitted to the compiler.

::

  // Valid identifiers
  foo
  _0
  swift
  vernissé
  闪亮
  מבריק
  😄

  // Invalid identifiers
  ☃     // Is a symbol
  0cool // Starts with an ASCII digit
   ́foo  // Starts with a combining character (U+0301)
       // Is a private-use character (U+F8FF)

.. _langref.lexical.operator:

Operator Tokens
---------------

.. code-block:: none

  <a name="operator">operator</a> ::= [/=-+*%<>!&|^~]+
  <a name="operator">operator</a> ::= \.+

  <a href="#reserved_punctuation">Reserved for punctuation</a>: '.', '=', '->', and unary prefix '&'
  <a href="#whitespace">Reserved for comments</a>: '//', '/*' and '*/'

  operator-binary ::= operator
  operator-prefix ::= operator
  operator-postfix ::= operator

  left-binder  ::= [ \r\n\t\(\[\{,;:]
  right-binder ::= [ \r\n\t\)\]\},;:]

  <a name="any-identifier">any-identifier</a> ::= identifier | operator

``operator-binary``, ``operator-prefix``, and ``operator-postfix`` are
distinguished by immediate lexical context.  An operator token is called
*left-bound* if it is immediately preceded by a character matching
``left-binder``.  An operator token is called *right-bound* if it is
immediately followed by a character matching ``right-binder``.  An operator
token is an ``operator-prefix`` if it is right-bound but not left-bound, an
``operator-postfix`` if it is left-bound but not right-bound, and an
``operator-binary`` in either of the other two cases.

As an exception, an operator immediately followed by a dot ('``.``') is only
considered right-bound if not already left-bound.  This allows ``a!.prop`` to
be parsed as ``(a!).prop`` rather than as ``a ! .prop``.

The '``!``' operator is postfix if it is left-bound.

The '``?``' operator is postfix (and therefore not the ternary operator) if it
is left-bound.  The sugar form for ``Optional`` types must be left-bound.

When parsing certain grammatical constructs that involve '``<``' and '``>``'
(such as <a href="#type-composition">protocol composition types</a>), an
``operator`` with a leading '``<``' or '``>``' may be split into two or more
tokens: the leading '``<``' or '``>``' and the remainder of the token, which
may be an ``operator`` or ``punctuation`` token that may itself be further
split.  This rule allows us to parse nested constructs such as ``A<B<C>>``
without requiring spaces between the closing '``>``'s.

.. _langref.lexical.dollarident:

Implementation Identifier Token
-------------------------------

.. code-block:: none

  dollarident ::= '$' id-continue+

Tokens that start with a ``$`` are separate class of identifier, which are
fixed purpose names that are defined by the implementation.


.. _langref.namebind:

Name Binding
============

.. _langref.typecheck:

Type Checking
=============

.. _langref.decl:

Declarations
============

...

.. _langref.decl.import:

Import Declarations
-------------------

.. _langref.expr:

Expressions
===========

...

.. _langref.expr.call:

Function Application
--------------------

.. _langref.stdlib:

Standard Library
================

.. admonition:: Commentary

  It would be really great to have literate swift code someday, that way this
  could be generated directly from the code.  This would also be powerful for
  Swift library developers to be able to depend on being available and
  standardized.

This describes some of the standard swift code as it is being built up.  Since
Swift is designed to give power to the library developers, much of what is
normally considered the "language" is actually just implemented in the
library.

All of this code is published by the '``swift``' module, which is implicitly
imported into each source file, unless some sort of pragma in the code
(attribute on an import?) is used to change or disable this behavior.

.. _langref.stdlib.builtin:

Builtin Module
==============

In the initial Swift implementation, a module named ``Builtin`` is imported
into every file.  Its declarations can only be found by <a href="#expr-dot">dot
syntax</a>.  It provides access to a small number of primitive representation
types and operations defined over them that map directly to LLVM IR.

The existance of and details of this module are a private implementation detail
used by our implementation of the standard library.  Swift code outside the
standard library should not be aware of this library, and an independent
implementation of the swift standard library should be allowed to be
implemented without the builtin library if it desires.

For reference below, the description of the standard library uses the
"``Builtin.``" namespace to refer to this module, but independent
implementations could use another implementation if they so desire.

.. _langref.stdlib.simple-types:

Simple Types
------------

Void
^^^^

::

  // Void is just a type alias for the empty tuple.
  typealias Void = ()

.. _langref.stdlib.int:

Int, Int8, Int16, Int32, Int64
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. admonition:: Commentary

  Having a single standardized integer type that can be used by default
  everywhere is important.  One advantage Swift has is that by the time it is
  in widespread use, 64-bit architectures will be pervasive, and the LLVM
  optimizer should grow to be good at shrinking 64-bit integers to 32-bit in
  many cases for those 32-bit architectures that persist.

::

  // Fixed size types are simple structs of the right size.
  struct Int8  { value : Builtin.Int8 }
  struct Int16 { value : Builtin.Int16 }
  struct Int32 { value : Builtin.Int32 }
  struct Int64 { value : Builtin.Int64 }
  struct Int128 { value : Builtin.Int128 }

  // Int is just an alias for the 64-bit integer type.
  typealias Int = Int64

.. _langref.stdlib.float:

Int, Int8, Int16, Int32, Int64
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

::

  struct Float  { value : Builtin.FPIEEE32 }
  struct Double { value : Builtin.FPIEEE64 }

.. _langref.stdlib.bool:

Bool, true, false
^^^^^^^^^^^^^^^^^

::

  // Bool is a simple enum.
  enum Bool {
    true, false
  }

  // Allow true and false to be used unqualified.
  var true = Bool.true
  var false = Bool.false

.. _langref.stdlib.oper:

Arithmetic and Logical Operations
---------------------------------

.. _langref.stdlib.oper.arithmetic:

Arithmetic Operators
^^^^^^^^^^^^^^^^^^^^

::

  func * (lhs: Int, rhs: Int) -> Int
  func / (lhs: Int, rhs: Int) -> Int
  func % (lhs: Int, rhs: Int) -> Int
  func + (lhs: Int, rhs: Int) -> Int
  func - (lhs: Int, rhs: Int) -> Int

.. _langref.stdlib.oper.comparison:

Relational and Equality Operators
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

::

  func <  (lhs : Int, rhs : Int) -> Bool
  func >  (lhs : Int, rhs : Int) -> Bool
  func <= (lhs : Int, rhs : Int) -> Bool
  func >= (lhs : Int, rhs : Int) -> Bool
  func == (lhs : Int, rhs : Int) -> Bool
  func != (lhs : Int, rhs : Int) -> Bool

.. _langref.stdlib.oper.short-circuit-logical:

Short Circuiting Logical Operators
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

::

  func && (lhs: Bool, rhs: ()->Bool) -> Bool
  func || (lhs: Bool, rhs: ()->Bool) -> Bool

Swift has a simplified precedence levels when compared with C.  From highest to
lowest:

::

  "exponentiative:" <<, >>
  "multiplicative:" *, /, %, &
  "additive:" +, -, |, ^
  "comparative:" ==, !=, <, <=, >=, >
  "conjunctive:" &&
  "disjunctive:" ||


