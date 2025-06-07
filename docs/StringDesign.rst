:orphan:

.. raw:: html

    <style>

    .repl,
    .emph,
    .look {
      color:rgb(47,175,187)
    }
    .emph {
      font-weight:bold
    }

    pre,
    .pre {
      font-family: Monaco, monospace; font-size:90%
    }

    pre.literal-block {
      overflow: hidden;
    }

    span.look,
    span.look1 {
      position: relative;
      border-bottom: .2em dotted rgb(255,165,165);
    }

    span.aside {
      font-family: sans-serif; white-space: normal;
    }

    span.look + span.aside,
    span.look1 + span.aside {
      display: none;
    }

    span.look:hover,
    span.look1:hover {
      background-color:greenyellow;
    }

    span.look:hover {
      color:rgb(23,87,94);
    }

    /* Main speech bubble*/
    span.look:hover + span.aside,
    span.look1:hover + span.aside {
      display: inline-block;
      position: relative;
      margin-top: -1000em;
      margin-bottom: -1000em;
      margin-right: -1000em;
      padding: 0.3em 1em 0.3em 1em;
      /*text-align: justify;*/
      max-width: 70%;
      /*width: 50%;*/
      left: 2em;
      background: gray;
      -moz-border-radius:10px;
      -webkit-border-radius:10px;
      border-radius:10px;
      color: #fff;
      z-index: 1;
    }

    /* Little triangle on the left */
    span.look:hover + span.aside:after,
    span.look1:hover + span.aside:after {
      content: "";
      position: absolute;
      bottom: 0.3em;
      left: -1.5em;
      border-style: solid;
      border-width: 0.6em 2em 0.6em 0;
      border-color: transparent gray;
      display: inline;
      width: 0;
      z-index: 2;
    }

    /*a:link {color:blue}*/
    </style>

.. role:: repl
.. default-role:: repl

.. |swift| replace:: (swift)

.. role:: look
.. role:: look1
.. role:: aside
.. role:: emph

===================
Swift String Design
===================

.. Admonition:: This Document
   :class: note

   * contains interactive HTML commentary that does not
     currently appear in printed output.  Hover your mouse over
     elements with a dotted pink underline to view the hidden
     commentary.

   * represents the intended design of Swift strings, not their
     current implementation state.

   * is being delivered in installments.  Content still to come is
     outlined in `Coming Installments`_.

.. warning:: This document was used in planning Swift 1.0; it has not been kept
  up to date and does not describe the current or planned behavior of Swift.

.. contents::
   :depth: 3

Introduction
============

Like all things Swift, our approach to strings begins with a deep
respect for the lessons learned from many languages and libraries,
especially Objective-C and Cocoa.

Goals
-----

``String`` should:

* honor industry standards such as Unicode
* when handling non-ASCII text, deliver "reasonably correct"
  results to users thinking only in terms of ASCII
* when handling ASCII text, provide "expected behavior" to users
  thinking only in terms of ASCII
* be hard to use incorrectly
* be easy to use correctly
* provide near-optimal efficiency for 99% of use cases
* provide a foundation upon which proper locale-sensitive operations
  can be built

Non-Goals
---------

``String`` need not:

* have behavior appropriate to all locales and contexts
* be an appropriate type (or base type) for all text storage
  applications

Overview By Example
===================

In this section, we'll walk through some basic examples of Swift
string usage while discovering its essential properties.

``String`` is a `First-Class Type`__
------------------------------------

__ https://en.wikipedia.org/wiki/First-class_citizen

.. parsed-literal::

  |swift| var s = "Yo"
  `// s:` :emph:`String` `= "Yo"`

Unlike, say, C's ``char*``, the meaning of a Swift string is always
unambiguous.

Strings are **Efficient**
-------------------------

The implementation of ``String`` takes advantage of state-of-the-art
optimizations, including:

- Storing short strings without heap allocation
- Sharing allocated buffers among copies and slices
- In-place modification of uniquely-owned buffers

As a result, copying_ and slicing__ strings, in particular, can be
viewed by most programmers as being "almost free."

__ sliceable_

Strings are **Mutable**
-----------------------

.. sidebar:: Why Mention It?

   The ability to change a string's value might not be worth noting
   except that *some languages make all strings immutable*, as a way
   of working around problems that Swift has defined away--by making
   strings pure values (see below).

.. parsed-literal::
  |swift| extension String {
            func addEcho() {
              self += self
            }
          }
  |swift| :look1:`s.addEcho()`\ :aside:`s is modified in place`
  |swift| s
  `// s: String =` :emph:`"YoYo"`

.. _copying:

Strings are **Value Types**
---------------------------

Distinct string variables have independent values: when you pass
someone a string they get a copy of the value, and when someone
passes you a string *you own it*.  Nobody can change a string value
"behind your back."

.. parsed-literal::
  |swift| class Cave {
            // Utter something in the cave
            func say(_ msg: String) -> String {
              :look1:`msg.addEcho()`\ :aside:`Modifying a parameter is safe because the callee sees a copy of the argument`
              self.lastSound = msg
              :look1:`return self.lastSound`\ :aside:`Returning a stored value is safe because the caller sees a copy of the value`
            }

            var lastSound: String   // a Cave remembers the last sound made
          }
  |swift| var c = Cave()
  `// c: Cave = <Cave instance>`
  |swift| s = "Hey"
  |swift| var t = :look1:`c.say(s)`\ :aside:`this call can't change s...`
  `// t: String = "HeyHey"`
  |swift| s
  `// s: String =` :look:`"Hey"`\ :aside:`...and it doesn't.`
  |swift| :look1:`t.addEcho()`\ :aside:`this call can't change c.lastSound...`
  |swift| [s, c.lastSound, t]
  `// r0: [String] = ["Hey",` :look:`"HeyHey"`\ :aside:`...and it doesn't.`\ `, "HeyHeyHeyHey"]`

Strings are **Unicode-Aware**
-----------------------------

.. sidebar:: Deviations from Unicode


   Any deviation from what Unicode
   specifies requires careful justification.  So far, we have found two
   possible points of deviation for Swift ``String``:

   1. The `Unicode Text Segmentation Specification`_ says, "`do not
      break between CR and LF`__."  However, breaking extended
      grapheme clusters between CR and LF may necessary if we wish
      ``String`` to "behave normally" for users of pure ASCII.  This
      point is still open for discussion.

      __ http://www.unicode.org/reports/tr29/#GB2

   2. The `Unicode Text Segmentation Specification`_ says,
      "`do not break between regional indicator symbols`__."  However, it also
      says "(Sequences of more than two RI characters should be separated
      by other characters, such as U+200B ZWSP)."  Although the
      parenthesized note probably has less official weight than the other
      admonition, breaking pairs of RI characters seems like the right
      thing for us to do given that Cocoa already forms strings with
      several adjacent pairs of RI characters, and the Unicode spec *can*
      be read as outlawing such strings anyway.

      __ http://www.unicode.org/reports/tr29/#GB8

.. _Unicode Text Segmentation Specification: http://www.unicode.org/reports/tr29

Swift applies Unicode algorithms wherever possible.  For example,
distinct sequences of code points are treated as equal if they
represent the same character: [#canonical]_

.. parsed-literal::
  |swift| var n1 = ":look1:`\\u006E\\u0303`\ :aside:`Multiple code points, but only one Character`"
  `// n1 : String =` **"ñ"**
  |swift| var n2 = "\\u00F1"
  `// n2 : String =` **"ñ"**
  |swift| n1 == n2
  `// r0 : Bool =` **true**

Note that individual code points are still observable by explicit request:

.. parsed-literal::
  |swift| n1.codePoints == n2.codePoints
  `// r0 : Bool =` **false**

.. _locale-agnostic:

Strings are **Locale-Agnostic**
-------------------------------

Strings neither carry their own locale information, nor provide
behaviors that depend on a global locale setting.  Thus, for any pair
of strings ``s1`` and ``s2``, "``s1 == s2``" yields the same result
regardless of system state.  Strings *do* provide a suitable
foundation on which to build locale-aware interfaces.\ [#locales]_

Strings are **Containers**
--------------------------

.. sidebar:: String Indices

          ``String`` implements the ``Container`` protocol, but
          **cannot be indexed by integers**.  Instead,
          ``String.IndexType`` is a library type conforming to the
          ``BidirectionalIndex`` protocol.

          This might seem surprising at first, but code that indexes
          strings with arbitrary integers is seldom Unicode-correct in
          the first place, and Swift provides alternative interfaces
          that encourage Unicode-correct code.  For example, instead
          of ``s[0] == 'S'`` you'd write ``s.startsWith("S")``.

.. parsed-literal::
   |swift| var s = "Strings are awesome"
   `// s : String = "Strings are awesome"`
   |swift| var r = s.find("awe")
   `// r : Range<StringIndex> = <"...are a̲w̲e̲some">`
   |swift| s[r.start]
   `// r0 : Character =` :look:`Character("a")`\ :aside:`String elements have type Character (see below)`

.. |Character| replace:: ``Character``
.. _Character:

Strings are Composed of ``Character``\ s
----------------------------------------

``Character``, the element type of ``String``, represents a **grapheme
cluster**, as specified by a default or tailored Unicode segmentation
algorithm.  This term is `precisely defined`__ by the Unicode
specification, but it roughly means `what the user thinks of when she
hears "character"`__. For example, the pair of code points "LATIN
SMALL LETTER N, COMBINING TILDE" forms a single grapheme cluster, "ñ".

__ http://www.unicode.org/glossary/#grapheme_cluster
__ http://useless-factor.blogspot.com/2007/08/unicode-implementers-guide-part-4.html

Access to lower-level elements is still possible by explicit request:

.. parsed-literal::
   |swift| s.codePoints[s.codePoints.start]
   `// r1 : CodePoint = CodePoint(83) /* S */`
   |swift| s.bytes[s.bytes.start]
   `// r2 : UInt8 = UInt8(83)`

Strings Support Flexible Segmentation
=====================================

The ``Character``\ s enumerated when simply looping over elements of a
Swift string are `extended grapheme clusters`__ as determined by
Unicode's `Default Grapheme Cluster Boundary
Specification`__. [#char]_

__ http://www.unicode.org/glossary/#extended_grapheme_cluster
__ http://www.unicode.org/reports/tr29/#Default_Grapheme_Cluster_Table

This segmentation offers naïve users of English, Chinese, French, and
probably a few other languages what we think of as the "expected
results."  However, not every script_ can be segmented uniformly for
all purposes.  For example, searching and collation require different
segmentations in order to handle Indic scripts correctly.  To that
end, strings support properties for more-specific segmentations:

.. Note:: The following example needs a more interesting string in
          order to demonstrate anything interesting.  Hopefully Aki
          has some advice for us.

.. parsed-literal::
   |swift| for c in s { print("Extended Grapheme Cluster: \(c)") }
   `Extended Grapheme Cluster: f`
   `Extended Grapheme Cluster: o`
   `Extended Grapheme Cluster: o`
   |swift| for c in s.collationCharacters {
             print("Collation Grapheme Cluster: \(c)")
           }
   `Collation Grapheme Cluster: f`
   `Collation Grapheme Cluster: o`
   `Collation Grapheme Cluster: o`
   |swift| for c in s.searchCharacters {
             print("Search Grapheme Cluster: \(c)")
           }
   `Search Grapheme Cluster: f`
   `Search Grapheme Cluster: o`
   `Search Grapheme Cluster: o`

Also, each such segmentation provides a unique ``IndexType``, allowing
a string to be indexed directly with different indexing schemes

.. code-block:: swift-console

   |swift| var i = s.searchCharacters.startIndex
   `// r2 : UInt8 = UInt8(83)`

.. _script: http://www.unicode.org/glossary/#script

.. _sliceable:

Strings are **Sliceable**
-------------------------

.. parsed-literal::
   |swift| s[r.start...r.end]
   `// r2 : String = "awe"`
   |swift| s[\ :look1:`r.start...`\ ]\ :aside:`postfix slice operator means "through the end"`
   `// r3 : String = "awesome"`
   |swift| s[\ :look1:`...r.start`\ ]\ :aside:`prefix slice operator means "from the beginning"`
   `// r4 : String = "Strings are "`
   |swift| :look1:`s[r]`\ :aside:`indexing with a range is the same as slicing`
   `// r5 : String = "awe"`
   |swift| s[r] = "hand"
   |swift| s
   `// s : String = "Strings are` :look:`handsome`\ :aside:`slice replacement can resize the string`\ `"`

.. _extending:

Strings are **Encoded as UTF-8**
--------------------------------

.. sidebar:: Encoding Conversion

   Conversion to and from other encodings is out-of-scope for
   ``String`` itself, but could be provided, e.g., by an ``Encoding``
   module.

.. parsed-literal::
   |swift| for x in "bump"\ **.bytes** {
            print(x)
          }
   98
   117
   109
   112

Coming Installments
===================

* Reference Manual

* Rationales

* Cocoa Bridging Strategy

* Comparisons with NSString

  - High Level
  - Member-by-member

Reference Manual
================


* s.bytes
* s.indices
* s[i]
* s[start...end]
* s == t, s != t
* s < t, s > t, s <= t, s >= t
* s.hash()
* s.startsWith(), s.endsWith()
* s + t, s += t, s.append(t)
* s.split(), s.split(n), s.split(sep, n)
* s.strip(), s.stripStart(), s.stripEnd()
* s.commonPrefix(t), s.mismatch(t)
* s.toUpper(), s.toLower()
* s.trim(predicate)
* s.replace(old, new, count)
* s.join(sequenceOfStrings)

.. Stuff from Python that we don't need to do

   * s.capitalize()
   * s.find(), s.rfind()
   * Stuff for monospace
     * s * 20
     * s.center()
     * s.count() [no arguments]
     * s.expandTabs(tabsize)
     * s.leftJustify(width, fillchar)
     * s.rightJustify(width, fillchar)
     * s.count()
   * s.isAlphanumeric()
   * s.isAlphabetic()
   * s.isNumeric()
   * s.isDecimal()
   * s.isDigit()?
   * s.isLower()
   * s.isUpper()
   * s.isSpace()
   * s.isTitle()

Cocoa Bridging Strategy
=======================
..


Rationales
==========

Why a Built-In String Type?
---------------------------

.. Admonition:: DaveZ Sez

   In the "why a built-in string type" section, I think the main
   narrative is that two string types is bad, but that we have two
   string types in Objective-C for historically good reasons. To get
   one string type, we need to merge the high-level features of
   Objective-C with the performance of C, all while not having the
   respective bad the bad semantics of either (reference semantics and
   "anarchy" memory-management respectively). Furthermore, I'd write
   "value semantics" in place of "C++ semantics". I know that is what
   you meant, but we need to tread carefully in the final document.

``NSString`` and ``NSMutableString``\ --the string types provided by
Cocoa--are full-featured classes with high-level functionality for
writing fully-localized applications.  They have served Apple
programmers well; so, why does Swift have its own string type?

* ObjCMessageSend

* Error Prone Mutability
  Reference semantics don't line up with how people think about strings

* 2 is too many string types.
  two APIs
  duplication of effort
  documentation
  Complexity adds decisions for users
  etc.

* ObjC needed to innovate because C strings suck
  O(N) length
  no localization
  no memory management
  no specified encoding

* C strings had to stay around for performance reasons and
  interoperability

Want performance of C, sound semantics of C++ strings, and high-level
goodness of ObjC.

   The design of ``NSString`` is *very* different from the string
   designs of most modern programming languages, which all tend to be
   very similar to one another.  Although existing ``NSString`` users
   are a critical constituency today, current trends indicate that
   most of our *future* target audience will not be ``NSString``
   users. Absent compelling justification, it's important to make the
   Swift programming environment as familiar as possible for them.


How Would You Design It?
------------------------

.. Admonition:: DaveZ Sez

   In the "how would you design it" section, the main narrative is
   twofold: how does it "feel" and how efficient is it? The former is
   about feeling built in, which we can easily argue that both C
   strings or Cocoa strings fail at for their respective semantic (and
   often memory management related) reasons. Additionally, the "feel"
   should be modern, which is where the Cocoa framework and the
   Unicode standard body do better than C. Nevertheless, we can still
   do better than Objective-C and your strong work at helping people
   reason about grapheme clusters instead of code points (or worse,
   units) is wonderful and it feels right to developers. The second
   part of the narrative is about being efficient, which is where
   arguing for UTF8 is the non-obvious but "right" answer for the
   reasons we have discussed.

* It'd be an independent *value* so you don't have to micromanage
  sharing and mutation

* It'd be UTF-8 because:

  - UTF-8 has been the clear winner__ among Unicode encodings since at
    least 2008; Swift should interoperate smoothly and efficiently
    with the rest of the world's systems

    __ http://www.artima.com/weblogs/viewpost.jsp?thread=230157

  - UTF-8 is a fairly efficient storage format, especially for ASCII
    but also for the most common non-ASCII code points.

  - This__ posting elaborates on some other nice qualities of UTF-8:

    1. All ASCII files are already UTF-8 files
    2. ASCII bytes always represent themselves in UTF-8 files. They
       never appear as part of other UTF-8 sequences
    3. ASCII code points are always represented as themselves in UTF-8
       files. They cannot be hidden inside multibyte UTF-8
       sequences
    4. UTF-8 is self-synchronizing
    5. CodePoint substring search is just byte string search
    6. Most programs that handle 8-bit files safely can handle UTF-8 safely
    7. UTF-8 sequences sort in code point order.
    8. UTF-8 has no "byte order."

    __ http://research.swtch.com/utf8

* It would be efficient, taking advantage of state-of-the-art
  optimizations, including:

  - Storing short strings without heap allocation
  - Sharing allocated buffers among copies and slices
  - In-place modification of uniquely-owned buffers


Comparisons with ``NSString``
=============================

High-Level Comparison with ``NSString``
---------------------------------------

.. Admonition:: DaveZ Sez

   I think the main message of the API breadth subsection is that
   URLs, paths, etc would be modeled as formal types in Swift
   (i.e. not as extensions on String). Second, I'd speculate less on
   what Foundation could do (like extending String) and instead focus
   on the fact that NSString still exists as an escape hatch for those
   that feel that they need or want it. Furthermore, I'd move up the
   "element access" discussion above the "escape hatch" discussion
   (which should be last in the comparison with NSString discussion).

API Breadth
~~~~~~~~~~~

The ``NSString`` interface clearly shows the effects of 20 years of
evolution through accretion.  It is broad, with functionality
addressing encodings, paths, URLs, localization, and more.  By
contrast, the interface to Swift's ``String`` is much narrower.

.. _TBD:

Of course, there's a reason for every ``NSString`` method, and the
full breadth of ``NSString`` functionality must remain accessible to
the Cocoa/Swift programmer.  Fortunately, there are many ways to
address this need.  For example:

* The ``Foundation`` module can extend ``String`` with the methods of
  ``NSString``.  The extent to which we provide an identical-feeling
  interface and/or correct any ``NSString`` misfeatures is still TBD
  and wide open for discussion.

* We can create a new modular interface in pure Swift, including a
  ``Locale`` module that addresses localized string operations, an
  ``Encoding`` module that addresses character encoding schemes, a
  ``Regex`` module that provides regular expression functionality,
  etc.  Again, the specifics are TBD.

* When all else fails, users can convert their Swift ``String``\ s to
  ``NSString``\ s when they want to access ``NSString``-specific
  functionality:

  .. parsed-literal::

    **NString(mySwiftString)**\ .localizedStandardCompare(otherSwiftString)

For Swift version 1.0, we err on the side of keeping the string
interface small, coherent, and sufficient for implementing
higher-level functionality.

Element Access
~~~~~~~~~~~~~~

``NSString`` exposes UTF-16 `code units`__ as the primary element on
which indexing, slicing, and iteration operate.  Swift's UTF-8 code
units are only available as a secondary interface.

__ http://www.unicode.org/glossary/#code_unit

``NSString`` is indexable and sliceable using ``Int``\ s, and so
exposes a ``length`` attribute. Swift's ``String`` is indexable and
sliceable using an abstract ``BidirectionalIndex`` type, and `does not
expose its length`__.

__ length_

Sub-Strings
~~~~~~~~~~~

.. _range:

Creating substrings in Swift is very fast. Therefore, Cocoa APIs that
operate on a substring given as an ``NSRange`` are replaced with Swift
APIs that just operate on ``String``\ s. One can use range-based
subscripting to achieve the same effect. For example: ``[str doFoo:arg
withRange:subrange]`` becomes ``str[subrange].doFoo(arg)``.

``NSString`` Member-by-Member Comparison
----------------------------------------

:Notes:
  * The following are from public headers from public frameworks, which
    are AppKit and Foundation (verified).

  * Deprecated Cocoa APIs are not considered

  * A status of "*Remove*" below indicates a feature whose removal is
    anticipated.  Rationale is provided for these cases.

Indexing
~~~~~~~~

.. _length:

---------

.. sidebar:: Why doesn't ``String`` support ``.length``?

    In Swift, by convention, ``x.length`` is used to represent
    the number of elements in a container, and since ``String`` is a
    container of abstract |Character|_\ s, ``length`` would have to
    count those.

    This meaning of ``length`` is unimplementable in O(1).  It can be
    cached, although not in the memory block where the characters are
    stored, since we want a ``String`` to share storage with its
    slices.  Since the body of the ``String`` must already store the
    ``String``\ 's *byte length*, caching the ``length`` would
    increase the footprint of the top-level String object.  Finally,
    even if ``length`` were provided, doing things with ``String``
    that depend on a specific numeric ``length`` is error-prone.

:Cocoa:
  .. parsed-literal::

     \- (NSUInteger)\ **length**
     \- (unichar)\ **characterAtIndex:**\ (NSUInteger)index;

:Swift: *not directly provided*, but similar functionality is
  available:

  .. parsed-literal::

       for j in 0...\ **s.bytes.length** {
         doSomethingWith(**s.bytes[j]**)
       }

---------

:Cocoa:
  .. parsed-literal::
     \- (NSRange)\ **rangeOfComposedCharacterSequenceAtIndex:**\ (NSUInteger)index;
     \- (NSRange)\ **rangeOfComposedCharacterSequencesForRange:**\ (NSRange)range;

:Swift:
  .. parsed-literal::
    typealias IndexType = ...
    func **indices**\ () -> Range<IndexType>
    **subscript**\ (i: IndexType) -> Character

  .. Admonition:: Usage

     .. parsed-literal::

       for i in someString.indices() {
         doSomethingWith(\ **someString[i]**\ )
       }

       var (i, j) = **someString.indices().bounds**
       while (i != j) {
         doSomethingElseWith(\ **someString[i]**\ )
         ++i
       }


Slicing
~~~~~~~

:Cocoa:
  .. parsed-literal::
     \- (void)\ **getCharacters:**\ (unichar \*)\ **buffer range:**\ (NSRange)aRange;

:Swift:
  .. parsed-literal::
    typealias IndexType = ...
    **subscript**\ (r: Range<IndexType>) -> Character

Indexing
~~~~~~~~

:Cocoa:
  .. parsed-literal::
     \- (NSString \*)\ **substringToIndex:**\ (NSUInteger)to;
     \- (NSString \*)\ **substringFromIndex:**\ (NSUInteger)from;
     \- (NSString \*)\ **substringWithRange:**\ (NSRange)range;

:Swift:
  .. parsed-literal::
    **subscript**\ (range : Range<IndexType>) -> String

  .. _slicing:

  .. Admonition:: Example

    .. parsed-literal::
        s[beginning...ending] // [s substringWithRange: NSMakeRange(beginning, ending)]
        s[beginning...]       // [s substringFromIndex: beginning]
        s[...ending]          // [s substringToIndex: ending]

    :Note: Swift may need additional interfaces to support
           ``index...`` and ``...index`` notations.  This part of the
           ``Container`` protocol design isn't worked out yet.

Comparison
~~~~~~~~~~~~

:Cocoa:
  .. parsed-literal::
     \- (BOOL)\ **isEqualToString:**\ (NSString \*)aString;
     \- (NSComparisonResult)\ **compare:**\ (NSString \*)string;

:Swift:
  .. parsed-literal::
     func **==** (lhs: String, rhs: String) -> Bool
     func **!=** (lhs: String, rhs: String) -> Bool
     func **<**  (lhs: String, rhs: String) -> Bool
     func **>**  (lhs: String, rhs: String) -> Bool
     func **<=** (lhs: String, rhs: String) -> Bool
     func **>=** (lhs: String, rhs: String) -> Bool

``NSString`` comparison is "literal" by default.  As the documentation
says of ``isEqualToString``,

  "Ö" represented as the composed character sequence "O" and umlaut
  would not compare equal to "Ö" represented as one Unicode character.

By contrast, Swift string's primary comparison interface uses
Unicode's default collation_ algorithm, and is thus always
"Unicode-correct."  Unlike comparisons that depend on locale, it is
also stable across changes in system state.  However, *just like*
``NSString``\ 's ``isEqualToString`` and ``compare`` methods, it
should not be expected to yield ideal (or even "proper") results in
all contexts.

---------

:Cocoa:
  .. parsed-literal::
     \- (NSComparisonResult)\ **compare:**\ (NSString \*)string \ **options:**\ (NSStringCompareOptions)mask;
     \- (NSComparisonResult)\ **compare:**\ (NSString \*)string \ **options:**\ (NSStringCompareOptions)mask \ **range:**\ (NSRange)compareRange;
     \- (NSComparisonResult)\ **caseInsensitiveCompare:**\ (NSString \*)string;

:Swift: *various compositions of primitive operations* / TBD_

* As noted above__, instead of passing sub-range arguments, we expect
  Swift users to compose slicing_ with whole-string operations.

  __ range_

* Other details of these interfaces are distinguished by an
  ``NSStringCompareOptions`` mask, of which
  ``caseInsensitiveCompare:`` is essentially a special case:

  :``NSCaseInsensitiveSearch``: Whether a direct interface is needed
     at all in Swift, and if so, its form, are TBD_.  However, we
     should consider following the lead of Python 3, wherein case
     conversion also `normalizes letterforms`__.  Then one can combine
     ``String.toLower()`` with default comparison to get a
     case-insensitive comparison::

       { $0.toLower() == $1.toLower() }

     __ http://stackoverflow.com/a/11573384/125349

  :``NSLiteralSearch``: Though it is the default for ``NSString``,
     this option is essentially only useful as a performance
     optimization when the string content is known to meet certain
     restrictions (i.e. is known to be pure ASCII).  When such
     optimization is absolutely necessary, Swift standard library
     algorithms can be used directly on the ``String``\ 's UTF8 code
     units.  However, Swift will also perform these optimizations
     automatically (at the cost of a single test/branch) in many
     cases, because each ``String`` stores a bit indicating whether
     its content is known to be ASCII.

  :``NSBackwardsSearch``: It's unclear from the docs how this option
     interacts with other ``NSString`` options, if at all, but basic
     cases can be handled in Swift by ``s1.endsWith(s2)``.

  :``NSAnchoredSearch``: Not applicable to whole-string comparisons
  :``NSNumericSearch``: While it's legitimate to defer this
                        functionality to Cocoa, it's (probably--see
                        <rdar://problem/14724804>) locale-independent and
                        easy enough to implement in Swift.  TBD_
  :``NSDiacriticInsensitiveSearch``: Ditto; TBD_
  :``NSWidthInsensitiveSearch``: Ditto; TBD_
  :``NSForcedOrderingSearch``: Ditto; TBD_.  Also see
                               <rdar://problem/14724888>
  :``NSRegularExpressionSearch``: We can defer this functionality to
                                  Cocoa, or dispatch directly to ICU
                                  as an optimization.  It's unlikely
                                  that we'll be building Swift its own
                                  regexp engine for 1.0.

---------

:Cocoa:
  .. parsed-literal::
     \- (NSComparisonResult)\ **localizedCompare:**\ (NSString \*)string;
     \- (NSComparisonResult)\ **localizedCaseInsensitiveCompare:**\ (NSString \*)string;
     \- (NSComparisonResult)\ **localizedStandardCompare:**\ (NSString \*)string;
     \- (NSComparisonResult)\ **compare:**\ (NSString \*)string \ **options:**\ (NSStringCompareOptions)mask \ **range:**\ (NSRange)compareRange \ **locale:**\ (id)locale;

:Swift: As these all depend on locale, they are TBD_

Searching
~~~~~~~~~

.. Sidebar:: Rationale

   Modern languages (Java, C#, Python, Ruby...) have standardized on
   variants of ``startsWith``/\ ``endsWith``.  There's no reason Swift
   should deviate from de-facto industry standards here.

:Cocoa:
  .. parsed-literal::
     \- (BOOL)\ **hasPrefix:**\ (NSString \*)aString;
     \- (BOOL)\ **hasSuffix:**\ (NSString \*)aString;

:Swift:
  .. parsed-literal::
     func **startsWith**\ (_ prefix: String)
     func **endsWith**\ (_ suffix: String)

----

:Cocoa:
  .. parsed-literal::
     \- (NSRange)\ **rangeOfString:**\ (NSString \*)aString;

:Swift:
  .. parsed-literal::
       func **find**\ (_ sought: String) -> Range<String.IndexType>

  .. Note:: Most other languages provide something like
            ``s1.indexOf(s2)``, which returns only the starting index of
            the first match.  This is far less useful than the range of
            the match, and is always available via
            ``s1.find(s2).bounds.0``

----

:Cocoa:
  .. parsed-literal::
     \- (NSRange)\ **rangeOfCharacterFromSet:**\ (NSCharacterSet \*)aSet;

.. sidebar:: Naming

   The Swift function is just an algorithm that comes from conformance
   to the ``Container`` protocol, which explains why it doesn't have a
   ``String``\ -specific name.

:Swift:
  .. parsed-literal::
       func **find**\ (_ match: (Character) -> Bool) -> Range<String.IndexType>

  .. Admonition:: Usage Example

     The ``NSString`` semantics can be achieved as follows:

     .. parsed-literal::

        someString.find( {someCharSet.contains($0)} )

-----

:Cocoa:
  .. parsed-literal::
     \- (NSRange)\ **rangeOfString:**\ (NSString \*)aString \ **options:**\ (NSStringCompareOptions)mask;
     \- (NSRange)\ **rangeOfString:**\ (NSString \*)aString \ **options:**\ (NSStringCompareOptions)mask \ **range:**\ (NSRange)searchRange;
     \- (NSRange)\ **rangeOfString:**\ (NSString \*)aString \ **options:**\ (NSStringCompareOptions)mask \ **range:**\ (NSRange)searchRange \ **locale:**\ (NSLocale \*)locale;

     \- (NSRange)\ **rangeOfCharacterFromSet:**\ (NSCharacterSet \*)aSet \ **options:**\ (NSStringCompareOptions)mask;
     \- (NSRange)\ **rangeOfCharacterFromSet:**\ (NSCharacterSet \*)aSet \ **options:**\ (NSStringCompareOptions)mask \ **range:**\ (NSRange)searchRange;

  These functions

:Swift: *various compositions of primitive operations* / TBD_

Building
~~~~~~~~

:Cocoa:
  .. parsed-literal::
     \- (NSString \*)\ **stringByAppendingString:**\ (NSString \*)aString;

.. sidebar:: ``append``

   the ``append`` method is a consequence of ``String``\ 's
   conformance to ``TextOutputStream``.  See the *Swift
   formatting proposal* for details.

:Swift:
  .. parsed-literal::
        func **+** (lhs: String, rhs: String) -> String
        func [infix, assignment] **+=** (lhs: [inout] String, rhs: String)
        func **append**\ (_ suffix: String)


Dynamic Formatting
~~~~~~~~~~~~~~~~~~

:Cocoa:
  .. parsed-literal::
     \- (NSString \*)\ **stringByAppendingFormat:**\ (NSString \*)format, ... NS_FORMAT_FUNCTION(1,2);

:Swift: *Not directly provided*\ --see the *Swift formatting proposal*

Extracting Numeric Values
~~~~~~~~~~~~~~~~~~~~~~~~~

:Cocoa:
  .. parsed-literal::
     \- (double)doubleValue;
     \- (float)floatValue;
     \- (int)intValue;
     \- (NSInteger)integerValue;
     \- (long long)longLongValue;
     \- (BOOL)boolValue;

:Swift: Not in ``String``\ --It is up to other types to provide their
   conversions to and from String.  See also this `rationale`__

   __ extending_

Splitting
~~~~~~~~~

:Cocoa:
  .. parsed-literal::
     \- (NSArray \*)\ **componentsSeparatedByString:**\ (NSString \*)separator;
     \- (NSArray \*)\ **componentsSeparatedByCharactersInSet:**\ (NSCharacterSet \*)separator;

:Swift:
  .. parsed-literal::
     func split(_ maxSplit: Int = Int.max()) -> [String]
     func split(_ separator: Character, maxSplit: Int = Int.max()) -> [String]

  The semantics of these functions were taken from Python, which seems
  to be a fairly good representative of what modern languages are
  currently doing.  The first overload splits on all whitespace
  characters; the second only on specific characters.  The universe of
  possible splitting functions is quite broad, so the particulars of
  this interface are **wide open for discussion**.  In Swift right
  now, these methods (on ``CodePoints``) are implemented in terms of a
  generic algorithm:

  .. parsed-literal::

    func **split**\ <Seq: Sliceable, IsSeparator: Predicate
        where IsSeparator.Arguments == Seq.Element
    >(_ seq: Seq, isSeparator: IsSeparator, maxSplit: Int = Int.max(),
      allowEmptySlices: Bool = false) -> [Seq]

Splitting
~~~~~~~~~

:Cocoa:
  .. parsed-literal::
     \- (NSString \*)\ **commonPrefixWithString:**\ (NSString \*)aString \ **options:**\ (NSStringCompareOptions)mask;

:Swift:
  .. parsed-literal::
     func **commonPrefix**\ (_ other: String) -> String

Upper/Lowercase
~~~~~~~~~~~~~~~

:Cocoa:
  .. parsed-literal::
     \- (NSString \*)\ **uppercaseString**;
     \- (NSString \*)\ **uppercaseStringWithLocale:**\ (NSLocale \*)locale;
     \- (NSString \*)\ **lowercaseString**;
     \- (NSString \*)\ **lowercaseStringWithLocale:**\ (NSLocale \*)locale;

.. sidebar:: Naming

   Other languages have overwhelmingly settled on ``upper()`` or
   ``toUpper()`` for this functionality

:Swift:
  .. parsed-literal::
     func **toUpper**\ () -> String
     func **toLower**\ () -> String

Capitalization
~~~~~~~~~~~~~~

:Cocoa:
  .. parsed-literal::
     \- (NSString \*)\ **capitalizedString**;
     \- (NSString \*)\ **capitalizedStringWithLocale:**\ (NSLocale \*)locale;

:Swift:
  **TBD**

.. Note:: ``NSString`` capitalizes the first letter of each substring
          separated by spaces, tabs, or line terminators, which is in
          no sense "Unicode-correct."  In most other languages that
          support a ``capitalize`` method, it operates only on the
          first character of the string, and capitalization-by-word is
          named something like "``title``."  If Swift ``String``
          supports capitalization by word, it should be
          Unicode-correct, but how we sort this particular area out is
          still **TBD**.

---------

:Cocoa:
  .. parsed-literal::
     \- (NSString \*)\ **stringByTrimmingCharactersInSet:**\ (NSCharacterSet \*)set;

:Swift:
  .. parsed-literal::
       trim **trim**\ (match: (Character) -> Bool) -> String

  .. Admonition:: Usage Example

     The ``NSString`` semantics can be achieved as follows:

     .. parsed-literal::

        someString.trim( {someCharSet.contains($0)} )

---------

:Cocoa:
  .. parsed-literal::
     \- (NSString \*)\ **stringByPaddingToLength:**\ (NSUInteger)newLength \ **withString:**\ (NSString \*)padString \ **startingAtIndex:**\ (NSUInteger)padIndex;

:Swift:
  .. parsed-literal:: *Not provided*.  It's not clear whether this is
                      useful at all for non-ASCII strings, and

---------

:Cocoa:
  .. parsed-literal::
     \- (void)\ **getLineStart:**\ (NSUInteger \*)startPtr \ **end:**\ (NSUInteger \*)lineEndPtr \ **contentsEnd:**\ (NSUInteger \*)contentsEndPtr \ **forRange:**\ (NSRange)range;

:Swift:
  .. parsed-literal::
        **TBD**

---------

:Cocoa:
  .. parsed-literal::
     \- (NSRange)\ **lineRangeForRange:**\ (NSRange)range;

:Swift:
  .. parsed-literal::
        **TBD**

---------

:Cocoa:
  .. parsed-literal::
     \- (void)\ **getParagraphStart:**\ (NSUInteger \*)startPtr \ **end:**\ (NSUInteger \*)parEndPtr \ **contentsEnd:**\ (NSUInteger \*)contentsEndPtr \ **forRange:**\ (NSRange)range;

:Swift:
  .. parsed-literal::
        **TBD**

---------

:Cocoa:
  .. parsed-literal::
     \- (NSRange)\ **paragraphRangeForRange:**\ (NSRange)range;

:Swift:
  .. parsed-literal::
        **TBD**

---------

:Cocoa:
  .. parsed-literal::
     \- (void)\ **enumerateSubstringsInRange:**\ (NSRange)range \ **options:**\ (NSStringEnumerationOptions)opts \ **usingBlock:**\ (void (^)(NSString \*substring, NSRange substringRange, NSRange enclosingRange, BOOL \*stop))block;

:Swift:
  .. parsed-literal::
        **TBD**

---------

:Cocoa:
  .. parsed-literal::
     \- (void)\ **enumerateLinesUsingBlock:**\ (void (^)(NSString \*line, BOOL \*stop))block;

:Swift:
  .. parsed-literal::
        **TBD**

---------

:Cocoa:
  .. parsed-literal::
     \- (NSString \*)description;

:Swift:
  .. parsed-literal::
        **TBD**

---------

:Cocoa:
  .. parsed-literal::
     \- (NSUInteger)hash;

:Swift:
  .. parsed-literal::
        **TBD**

---------

:Cocoa:
  .. parsed-literal::
     \- (NSStringEncoding)fastestEncoding;

:Swift:
  .. parsed-literal::
        **TBD**

---------

:Cocoa:
  .. parsed-literal::
     \- (NSStringEncoding)smallestEncoding;

:Swift:
  .. parsed-literal::
        **TBD**

---------

:Cocoa:
  .. parsed-literal::
     \- (NSData \*)\ **dataUsingEncoding:**\ (NSStringEncoding)encoding \ **allowLossyConversion:**\ (BOOL)lossy;

:Swift:
  .. parsed-literal::
        **TBD**

---------

:Cocoa:
  .. parsed-literal::
     \- (NSData \*)\ **dataUsingEncoding:**\ (NSStringEncoding)encoding;

:Swift:
  .. parsed-literal::
        **TBD**

- (BOOL)\ **canBeConvertedToEncoding:**\ (NSStringEncoding)encoding;


---------

:Cocoa:
  .. parsed-literal::
     \- (__strong const char \*)\ **cStringUsingEncoding:**\ (NSStringEncoding)encoding NS_RETURNS_INNER_POINTER;

:Swift:
  .. parsed-literal::
        **TBD**

---------

:Cocoa:
  .. parsed-literal::
     \- (BOOL)\ **getCString:**\ (char \*)buffer \ **maxLength:**\ (NSUInteger)maxBufferCount \ **encoding:**\ (NSStringEncoding)encoding;

:Swift:
  .. parsed-literal::
        **TBD**

---------

:Cocoa:
  .. parsed-literal::
     \- (BOOL)\ **getBytes:**\ (void \*)buffer \ **maxLength:**\ (NSUInteger)maxBufferCount \ **usedLength:**\ (NSUInteger \*)usedBufferCount \ **encoding:**\ (NSStringEncoding)encoding \ **options:**\ (NSStringEncodingConversionOptions)options \ **range:**\ (NSRange)range \ **remainingRange:**\ (NSRangePointer)leftover;

:Swift:
  .. parsed-literal::
        **TBD**

---------

:Cocoa:
  .. parsed-literal::
     \- (NSUInteger)\ **maximumLengthOfBytesUsingEncoding:**\ (NSStringEncoding)enc;

:Swift:
  .. parsed-literal::
        **TBD**

---------

:Cocoa:
  .. parsed-literal::
     \- (NSUInteger)\ **lengthOfBytesUsingEncoding:**\ (NSStringEncoding)enc;

:Swift:
  .. parsed-literal::
        **TBD**

---------

:Cocoa:
  .. parsed-literal::
     \- (NSString \*)decomposedStringWithCanonicalMapping;

:Swift:
  .. parsed-literal::
        **TBD**

---------

:Cocoa:
  .. parsed-literal::
     \- (NSString \*)precomposedStringWithCanonicalMapping;

:Swift:
  .. parsed-literal::
        **TBD**

---------

:Cocoa:
  .. parsed-literal::
     \- (NSString \*)decomposedStringWithCompatibilityMapping;

:Swift:
  .. parsed-literal::
        **TBD**

---------

:Cocoa:
  .. parsed-literal::
     \- (NSString \*)precomposedStringWithCompatibilityMapping;

:Swift:
  .. parsed-literal::
        **TBD**

---------

:Cocoa:
  .. parsed-literal::
     \- (NSString \*)\ **stringByFoldingWithOptions:**\ (NSStringCompareOptions)options \ **locale:**\ (NSLocale \*)locale;

:Swift:
  .. parsed-literal::
        **TBD**

---------

:Cocoa:
  .. parsed-literal::
     \- (NSString \*)\ **stringByReplacingOccurrencesOfString:**\ (NSString \*)target \ **withString:**\ (NSString \*)replacement \ **options:**\ (NSStringCompareOptions)options \ **range:**\ (NSRange)searchRange;

:Swift:
  .. parsed-literal::
        **TBD**

---------

:Cocoa:
  .. parsed-literal::
     \- (NSString \*)\ **stringByReplacingOccurrencesOfString:**\ (NSString \*)target \ **withString:**\ (NSString \*)replacement;

:Swift:
  .. parsed-literal::
        **TBD**

---------

:Cocoa:
  .. parsed-literal::
     \- (NSString \*)\ **stringByReplacingCharactersInRange:**\ (NSRange)range \ **withString:**\ (NSString \*)replacement;


---------

:Cocoa:
  .. parsed-literal::
     \- (__strong const char \*)UTF8String NS_RETURNS_INNER_POINTER;

:Swift:
  .. parsed-literal::
        **TBD**

---------

:Cocoa:
  .. parsed-literal::
     \+ (NSStringEncoding)defaultCStringEncoding;

:Swift:
  .. parsed-literal::
        **TBD**

---------

:Cocoa:
  .. parsed-literal::
     \+ (const NSStringEncoding \*)availableStringEncodings;

:Swift:
  .. parsed-literal::
        **TBD**

---------

:Cocoa:
  .. parsed-literal::
     \+ (NSString \*)\ **localizedNameOfStringEncoding:**\ (NSStringEncoding)encoding;

Constructors
~~~~~~~~~~~~

:Cocoa:
  .. parsed-literal::
     \- (instancetype)init;

---------

:Cocoa:
  .. parsed-literal::
     \- (instancetype)\ **initWithString:**\ (NSString \*)aString;

---------

:Cocoa:
  .. parsed-literal::
    \+ (instancetype)string;

---------

:Cocoa:
  .. parsed-literal::
    \+ (instancetype)\ **stringWithString:**\ (NSString \*)string;

Not available (too error prone)

---------

:Cocoa:
  .. parsed-literal::
     \- (instancetype)\ **initWithCharactersNoCopy:**\ (unichar \*)characters \ **length:**\ (NSUInteger)length \ **freeWhenDone:**\ (BOOL)freeBuffer;

:Swift:
  .. parsed-literal::
        **TBD**

---------

:Cocoa:
  .. parsed-literal::
     \- (instancetype)\ **initWithCharacters:**\ (const unichar \*)characters \ **length:**\ (NSUInteger)length;

:Swift:
  .. parsed-literal::
        **TBD**

---------

:Cocoa:
  .. parsed-literal::
     \- (instancetype)\ **initWithUTF8String:**\ (const char \*)nullTerminatedCString;

:Swift:
  .. parsed-literal::
        **TBD**

---------

:Cocoa:
  .. parsed-literal::
     \- (instancetype)\ **initWithFormat:**\ (NSString \*)format, ... NS_FORMAT_FUNCTION(1,2);

:Swift:
  .. parsed-literal::
        **TBD**

---------

:Cocoa:
  .. parsed-literal::
     \- (instancetype)\ **initWithFormat:**\ (NSString \*)format \ **arguments:**\ (va_list)argList NS_FORMAT_FUNCTION(1,0);

:Swift:
  .. parsed-literal::
        **TBD**

---------

:Cocoa:
  .. parsed-literal::
     \- (instancetype)\ **initWithFormat:**\ (NSString \*)format \ **locale:**\ (id)locale, ... NS_FORMAT_FUNCTION(1,3);

:Swift:
  .. parsed-literal::
        **TBD**

---------

:Cocoa:
  .. parsed-literal::
     \- (instancetype)\ **initWithFormat:**\ (NSString \*)format \ **locale:**\ (id)locale \ **arguments:**\ (va_list)argList NS_FORMAT_FUNCTION(1,0);

:Swift:
  .. parsed-literal::
        **TBD**

---------

:Cocoa:
  .. parsed-literal::
     \- (instancetype)\ **initWithData:**\ (NSData \*)data \ **encoding:**\ (NSStringEncoding)encoding;

:Swift:
  .. parsed-literal::
        **TBD**

---------

:Cocoa:
  .. parsed-literal::
     \- (instancetype)\ **initWithBytes:**\ (const void \*)bytes \ **length:**\ (NSUInteger)len \ **encoding:**\ (NSStringEncoding)encoding;

:Swift:
  .. parsed-literal::
        **TBD**

---------

:Cocoa:
  .. parsed-literal::
     \- (instancetype)\ **initWithBytesNoCopy:**\ (void \*)bytes \ **length:**\ (NSUInteger)len \ **encoding:**\ (NSStringEncoding)encoding \ **freeWhenDone:**\ (BOOL)freeBuffer;

:Swift:
  .. parsed-literal::
        **TBD**

---------

:Cocoa:
  .. parsed-literal::
     \+ (instancetype)\ **stringWithCharacters:**\ (const unichar \*)characters \ **length:**\ (NSUInteger)length;

:Swift:
  .. parsed-literal::
        **TBD**

---------

:Cocoa:
  .. parsed-literal::
     \+ (instancetype)\ **stringWithUTF8String:**\ (const char \*)nullTerminatedCString;

:Swift:
  .. parsed-literal::
        **TBD**

---------

:Cocoa:
  .. parsed-literal::
     \+ (instancetype)\ **stringWithFormat:**\ (NSString \*)format, ... NS_FORMAT_FUNCTION(1,2);

:Swift:
  .. parsed-literal::
        **TBD**

---------

:Cocoa:
  .. parsed-literal::
     \+ (instancetype)\ **localizedStringWithFormat:**\ (NSString \*)format, ... NS_FORMAT_FUNCTION(1,2);

:Swift:
  .. parsed-literal::
        **TBD**

---------

:Cocoa:
  .. parsed-literal::
     \- (instancetype)\ **initWithCString:**\ (const char \*)nullTerminatedCString \ **encoding:**\ (NSStringEncoding)encoding;

:Swift:
  .. parsed-literal::
        **TBD**

---------

:Cocoa:
  .. parsed-literal::
     \+ (instancetype)\ **stringWithCString:**\ (const char \*)cString \ **encoding:**\ (NSStringEncoding)enc;


Linguistic Analysis
~~~~~~~~~~~~~~~~~~~

:Cocoa:
  .. parsed-literal::
     \- (NSArray \*)\ **linguisticTagsInRange:**\ (NSRange)range \ **scheme:**\ (NSString \*)tagScheme \ **options:**\ (NSLinguisticTaggerOptions)opts \ **orthography:**\ (NSOrthography \*)orthography \ **tokenRanges:**\ (NSArray \*\*)tokenRanges;
     \- (void)\ **enumerateLinguisticTagsInRange:**\ (NSRange)range \ **scheme:**\ (NSString \*)tagScheme \ **options:**\ (NSLinguisticTaggerOptions)opts \ **orthography:**\ (NSOrthography \*)orthography \ **usingBlock:**\ (void (^)(NSString \*tag, NSRange tokenRange, NSRange sentenceRange, BOOL \*stop))block;

:Swift:
  .. parsed-literal::
        **TBD**

Unavailable on Swift Strings
----------------------------

URL Handling
~~~~~~~~~~~~

.. parsed-literal::

    \- (instancetype)\ **initWithContentsOfURL:**\ (NSURL \*)url \ **encoding:**\ (NSStringEncoding)enc \ **error:**\ (NSError \*\*)error;
    \+ (instancetype)\ **stringWithContentsOfURL:**\ (NSURL \*)url \ **encoding:**\ (NSStringEncoding)enc \ **error:**\ (NSError \*\*)error;
    \- (instancetype)\ **initWithContentsOfURL:**\ (NSURL \*)url \ **usedEncoding:**\ (NSStringEncoding \*)enc \ **error:**\ (NSError \*\*)error;
    \+ (instancetype)\ **stringWithContentsOfURL:**\ (NSURL \*)url \ **usedEncoding:**\ (NSStringEncoding \*)enc \ **error:**\ (NSError \*\*)error;
    \- (BOOL)\ **writeToURL:**\ (NSURL \*)url \ **atomically:**\ (BOOL)useAuxiliaryFile \ **encoding:**\ (NSStringEncoding)enc \ **error:**\ (NSError \*\*)error;
    \- (NSString \*)\ **stringByAddingPercentEncodingWithAllowedCharacters:**\ (NSCharacterSet \*)allowedCharacters;
    \- (NSString \*)stringByRemovingPercentEncoding;
    \- (NSString \*)\ **stringByAddingPercentEscapesUsingEncoding:**\ (NSStringEncoding)enc;
    \- (NSString \*)\ **stringByReplacingPercentEscapesUsingEncoding:**\ (NSStringEncoding)enc;

See: class File

.. parsed-literal::

    \- (instancetype)\ **initWithContentsOfFile:**\ (NSString \*)path \ **encoding:**\ (NSStringEncoding)enc \ **error:**\ (NSError \*\*)error;
    \+ (instancetype)\ **stringWithContentsOfFile:**\ (NSString \*)path \ **encoding:**\ (NSStringEncoding)enc \ **error:**\ (NSError \*\*)error;
    \- (instancetype)\ **initWithContentsOfFile:**\ (NSString \*)path \ **usedEncoding:**\ (NSStringEncoding \*)enc \ **error:**\ (NSError \*\*)error;
    \+ (instancetype)\ **stringWithContentsOfFile:**\ (NSString \*)path \ **usedEncoding:**\ (NSStringEncoding \*)enc \ **error:**\ (NSError \*\*)error;
    \- (BOOL)\ **writeToFile:**\ (NSString \*)path \ **atomically:**\ (BOOL)useAuxiliaryFile \ **encoding:**\ (NSStringEncoding)enc \ **error:**\ (NSError \*\*)error;

Path Handling
~~~~~~~~~~~~~

.. parsed-literal::

    \+ (NSString \*)\ **pathWithComponents:**\ (NSArray \*)components;
    \- (NSArray \*)pathComponents;
    \- (BOOL)isAbsolutePath;
    \- (NSString \*)lastPathComponent;
    \- (NSString \*)stringByDeletingLastPathComponent;
    \- (NSString \*)\ **stringByAppendingPathComponent:**\ (NSString \*)str;
    \- (NSString \*)pathExtension;
    \- (NSString \*)stringByDeletingPathExtension;
    \- (NSString \*)\ **stringByAppendingPathExtension:**\ (NSString \*)str;
    \- (NSString \*)stringByAbbreviatingWithTildeInPath;
    \- (NSString \*)stringByExpandingTildeInPath;
    \- (NSString \*)stringByStandardizingPath;
    \- (NSString \*)stringByResolvingSymlinksInPath;
    \- (NSArray \*)\ **stringsByAppendingPaths:**\ (NSArray \*)paths;
    \- (NSUInteger)\ **completePathIntoString:**\ (NSString \*\*)outputName \ **caseSensitive:**\ (BOOL)flag \ **matchesIntoArray:**\ (NSArray \*\*)outputArray \ **filterTypes:**\ (NSArray \*)filterTypes;
    \- (__strong const char \*)fileSystemRepresentation NS_RETURNS_INNER_POINTER;
    \- (BOOL)\ **getFileSystemRepresentation:**\ (char \*)cname \ **maxLength:**\ (NSUInteger)max;

Property Lists
~~~~~~~~~~~~~~

Property lists are a feature of Cocoa.

.. parsed-literal::

    \- (id)propertyList;
    \- (NSDictionary \*)propertyListFromStringsFileFormat;
    Not applicable. Swift does not provide GUI support.

    \- (NSSize)\ **sizeWithAttributes:**\ (NSDictionary \*)attrs;
    \- (void)\ **drawAtPoint:**\ (NSPoint)point \ **withAttributes:**\ (NSDictionary \*)attrs;
    \- (void)\ **drawInRect:**\ (NSRect)rect \ **withAttributes:**\ (NSDictionary \*)attrs;
    \- (void)\ **drawWithRect:**\ (NSRect)rect \ **options:**\ (NSStringDrawingOptions)options \ **attributes:**\ (NSDictionary \*)attributes;
    \- (NSRect)\ **boundingRectWithSize:**\ (NSSize)size \ **options:**\ (NSStringDrawingOptions)options \ **attributes:**\ (NSDictionary \*)attributes;
    \- (NSArray \*)\ **writableTypesForPasteboard:**\ (NSPasteboard \*)pasteboard;
    \- (NSPasteboardWritingOptions)\ **writingOptionsForType:**\ (NSString \*)type \ **pasteboard:**\ (NSPasteboard \*)pasteboard;
    \- (id)\ **pasteboardPropertyListForType:**\ (NSString \*)type;
    \+ (NSArray \*)\ **readableTypesForPasteboard:**\ (NSPasteboard \*)pasteboard;
    \+ (NSPasteboardReadingOptions)\ **readingOptionsForType:**\ (NSString \*)type \ **pasteboard:**\ (NSPasteboard \*)pasteboard;
    \- (id)\ **initWithPasteboardPropertyList:**\ (id)propertyList \ **ofType:**\ (NSString \*)type;

Deprecated APIs
~~~~~~~~~~~~~~~

Already deprecated in Cocoa.

.. parsed-literal::

    \- (const char \*)cString;
    \- (const char \*)lossyCString;
    \- (NSUInteger)cStringLength;
    \- (void)\ **getCString:**\ (char \*)bytes;
    \- (void)\ **getCString:**\ (char \*)bytes \ **maxLength:**\ (NSUInteger)maxLength;
    \- (void)\ **getCString:**\ (char \*)bytes \ **maxLength:**\ (NSUInteger)maxLength \ **range:**\ (NSRange)aRange \ **remainingRange:**\ (NSRangePointer)leftoverRange;
    \- (BOOL)\ **writeToFile:**\ (NSString \*)path \ **atomically:**\ (BOOL)useAuxiliaryFile;
    \- (BOOL)\ **writeToURL:**\ (NSURL \*)url \ **atomically:**\ (BOOL)atomically;
    \- (id)\ **initWithContentsOfFile:**\ (NSString \*)path;
    \- (id)\ **initWithContentsOfURL:**\ (NSURL \*)url;
    \+ (id)\ **stringWithContentsOfFile:**\ (NSString \*)path;
    \+ (id)\ **stringWithContentsOfURL:**\ (NSURL \*)url;
    \- (id)\ **initWithCStringNoCopy:**\ (char \*)bytes \ **length:**\ (NSUInteger)length \ **freeWhenDone:**\ (BOOL)freeBuffer;
    \- (id)\ **initWithCString:**\ (const char \*)bytes \ **length:**\ (NSUInteger)length;
    \- (id)\ **initWithCString:**\ (const char \*)bytes;
    \+ (id)\ **stringWithCString:**\ (const char \*)bytes \ **length:**\ (NSUInteger)length;
    \+ (id)\ **stringWithCString:**\ (const char \*)bytes;
    \- (void)\ **getCharacters:**\ (unichar \*)buffer;


--------------

Why YAGNI
---------

* Retroactive Modeling
* Derivation
* ...

.. [#agnostic] Unicode specifies default ("un-tailored")
   locale-independent collation_ and segmentation_ algorithms that
   make reasonable sense in most contexts.  Using these algorithms
   allows strings to be naturally compared and combined, generating
   the expected results when the content is ASCII

.. [#canonical] Technically, ``==`` checks for `Unicode canonical
                equivalence`__

                __ http://www.unicode.org/reports/tr15/tr15-18.html#Introduction

.. [#locales] We have some specific ideas for locale-sensitive
              interfaces, but details are still TBD and wide open for
              discussion.

.. [#re_sort] Collections that automatically re-sort based on locale
   changes are out of scope for the core Swift language

.. [#char] The type currently called ``Char`` in Swift represents a
   Unicode code point.  This document refers to it as ``CodePoint``,
   in anticipation of renaming.


.. _segmentation: http://www.unicode.org/reports/tr29/#GB1

.. _collation: http://www.unicode.org/reports/tr10/


.. [#code_points] When the user writes a string literal, she
   specifies a particular sequence of code points.  We guarantee that
   those code points are stored without change in the resulting
   ``String``.  The user can explicitly request normalization, and
   Swift can use a bit to remember whether a given string buffer has
   been normalized, thus speeding up comparison operations.

.. [#elements] Since ``String`` is locale-agnostic_, its elements are
   determined using Unicode's default, "un-tailored" segmentation_
   algorithm.

