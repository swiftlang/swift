.. @raise litre.TestsAreMissing

===================
Swift String Design
===================

Note:: This document represents the intended design of ``String``, not
its current implementation state.

.. contents:: Index

DaveZ's Proposed Document Structure
-----------------------------------

1. High level design rationale

Prose: words and thoughts like this: "deep reverence and respect for
the lessons learned from many languages and libraries". You don't
literally need to use the word "reverence" or "respect", but they do
help set the tone.

2. Examples and tutorials
3. Reference manual
4. High level comparison to NSString
5. Side by side comparisons with NSString
Take advantage of the back that you can reference back to sections 1,
2, and 3 in this outline. :-)

Chris' Outline of Important Points to Cover
-------------------------------------------

* Why having a builtin String is good

* The user model for Cocoa

  - Bridging
  - Extensions on String
  - Implicit Conversions
  - API?

* How it works?

  - Runtime Implementation Detail
  - How does Bridging Work
  - Measured Performance Cost
  - You can still use NSString

------------------------------------------------------------------------


General Principles
------------------

.. sidebar:: Rationale

   Making ``String`` a value type eliminates the need for defensive
   copying and a separate mutable string type.

   Unicode-awareness ensures that straightforward code written without
   attention to Unicode will not be badly broken in the presence of
   non-ASCII characters.  

   Locale-unawareness ensures that casual sorting or hashing of
   ``String``\ s is remains valid across changes in global system
   state.  Collections that automatically re-sort based on locale
   changes are firmly out-of-scope for Swift and in-scope for Cocoa.

   Encoding conversion is a neatly separable problem from other
   ``String`` issues, and—at least initially—we intend to refer users
   to Cocoa for that functionality.  When and if Swift itself acquires
   encoding support, it can go in a separate module with its own
   interfaces, e.g., ``utf16.encode(someString)``, thereby avoiding
   bloating the ``String`` interface.

   Strings are not indexable with ``Int`` because |Character|_\ s
   consume an arbitrary number of bytes, so they can't be randomly
   addressed with reasonable efficiency.

   .. _extending:

   The library uses restraint in extending ``String`` because
   ``String`` is a “vocabulary type” to and from which most other
   types are convertible.  Making these conversions members of
   ``String`` would quickly lead to an intolerably broad ``String``
   interface with intolerably slow code completion.

   The design of ``NSString`` is *very* different from the string
   designs of most modern programming languages, which all tend to be
   very similar to one another.  Although existing ``NSString`` users
   are a critical constituency today, current trends indicate that
   most of our *future* target audience will not be ``NSString``
   users. Absent compelling justification, it's important to make the
   Swift programming environment as familiar as possible for them.

* ``String`` is a mutable **value type**, just like ``Int``: each copy
  of a ``String`` is logically distinct from the original, and a
  ``String``’s value can be altered.

* ``String``\ 's primary interface is **Unicode-aware**.  For
  example::

    var x = "\u006E\u0303"   // Decomposed "ñ"
    var y = "\u00F1"         // Composed "ñ"
    assert(x == y)           // OK, passes

.. _locale-unaware:

* ``String``\ 's primary interface is **Locale-unaware**.  For any
  pair of strings ``s1`` and ``s2``, “``s1 == s2``” yields the same
  result regardless of the global locale. [#unaware]_

* The type currently called ``Char`` in Swift represents a Unicode
  code point.  This document refers to it as ``CodePoint``, in
  anticipation of renaming.

.. |Character| replace:: ``Character``
.. _Character:

* A new type called ``Character`` represents a Unicode `extended
  grapheme cluster`__ as defined by some Unicode segmentation
  algorithm.

  __ http://www.unicode.org/glossary/#extended_grapheme_cluster

* ``String`` elements are |Character|_\ s as defined by Unicode's
  `default segmentation`__ algorithm, rather than code units (as in
  Cocoa) or code points. [#elements]_ Secondary interfaces are
  provided for those who need to manipulate a ``String``\ 's
  ``CodePoint``\ s or its bytes.  For example::

    s                         // locale-independent Characters
    s.bytes                   // UInt8s (utf-8)
    s.codePoints              // CodePoints

  __ http://www.unicode.org/reports/tr29/#Default_Grapheme_Cluster_Table

* No ``String`` interfaces deal directly with encoding.

* ``String`` implements generic ``Indexable`` and ``Sliceable``
  protocols using a type conforming to ``BidirectionalIndex``, rather
  than ``Int``, as indices.

* When the user writes a string literal, she specifies a particular
  sequence of code points.  We guarantee that those code points are
  stored without change in the resulting ``String``.  The user
  can explicitly request normalization. [#normalizationbit]_

* The standard library does not extend ``String`` for each type with
  which it needs to interoperate.  Instead, types conforming to
  ``Printable`` have a ::

    func toString() -> String

  method, and types conforming to ``Parseable`` have a ::

    ``static func parse(s: String) -> Self``

* ``String``\ 's interface design is primaily inspired by convention
  among “modern” programming languages such as Python, C#, Ruby, and
  Go.

Deviations from Unicode
-----------------------

Unicode is a pretty good standard.  Any deviation from what Unicode
specifies requires careful justification.  So far, we have found two
possible points of deviation for Swift ``String``:

1. The `Unicode Text Segmentation Specification`_ says, “`do not
   break between CR and LF`__.”  However, breaking between CR and LF
   may necessary if we wish ``String`` to “behave normally” for users
   of pure ASCII.  This point is still open for discussion.

   __ http://www.unicode.org/reports/tr29/#GB2

2. The `Unicode Text Segmentation Specification`_ says,
   “`do not break between regional indicator symbols`__.”  However, it also
   says “(Sequences of more than two RI characters should be separated
   by other characters, such as U+200B ZWSP).”  Although the
   parenthesized note probably has less official weight than the other
   admonition, breaking pairs of RI characters seems like the right
   thing for us to do given that Cocoa already forms strings with
   several adjacent pairs of RI characters, and the Unicode spec *can*
   be read as outlawing such strings anyway.

   __ http://www.unicode.org/reports/tr29/#GB8

.. _Unicode Text Segmentation Specification: http://www.unicode.org/reports/tr29


Qualitative Comparison with ``NSString``
-----------------------------------------

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

  * A status of “*Remove*” below indicates a feature whose removal is
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
    ``String``\ 's *byte length*, cacheing the ``length`` would
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

       for j in 0...\ **s.utf8.length** {
         doSomethingWith(**s.utf8[j]**)
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

       var (i,j) = **someString.indices().bounds**
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
        s[beginning...ending] // [s substringWithRange: NSMakeRange( beginning, ending )]
        s[beginning...]       // [s substringFromIndex: beginning]
        s[...ending]          // [s substringToIndex: ending]

    :Note: Swift may need additional interfaces to support
           ``index...`` and ``...index`` notations.  This part of the
           ``Indexable`` protocol design isn't worked out yet.

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

``NSString`` comparison is “literal” by default.  As the documentation
says of ``isEqualToString``,

  “Ö” represented as the composed character sequence “O” and umlaut
  would not compare equal to “Ö” represented as one Unicode character.

By contrast, Swift string's primary comparison interface uses
Unicode's default collation_ algorithm, and is thus always
“Unicode-correct.”  Unlike comparisons that depend on locale, it is
also stable across changes in system state.  However, *just like*
``NSString``\ 's ``isEqualToString`` and ``compare`` methods, it
should not be expected to yield ideal (or even “proper”) results in
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
                        functionality to Cocoa, it's (probably—see
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

   Modern languages (Java, C#, Python, Ruby…) have standardized on
   variants of ``startsWith``/\ ``endsWith``.  There's no reason Swift
   should deviate from de-facto industry standards here.

:Cocoa: 
  .. parsed-literal::
     \- (BOOL)\ **hasPrefix:**\ (NSString \*)aString;
     \- (BOOL)\ **hasSuffix:**\ (NSString \*)aString;

:Swift: 
  .. parsed-literal::
     func **startsWith**\ (prefix: String)
     func **endsWith**\ (suffix: String)

----

:Cocoa: 
  .. parsed-literal::
     \- (NSRange)\ **rangeOfString:**\ (NSString \*)aString;

:Swift:
  .. parsed-literal::
       func **findRange**\ (sought: String) -> Range<String.IndexType>

  .. Note:: Most other languages provide something like
            ``s1.indexOf(s2)``, which returns only the starting index of
            the first match.  This is far less useful than the range of
            the match, and is always available via
            ``s1.findRangeOf(s2).bounds.0``

----

:Cocoa: 
  .. parsed-literal::
     \- (NSRange)\ **rangeOfCharacterFromSet:**\ (NSCharacterSet \*)aSet;

.. sidebar:: Naming

   The Swift function is just an algorithm that comes from conformance
   to the ``Indexable`` protocol, which explains why it doesn't have a
   ``String``\ -specific name.

:Swift:
  .. parsed-literal::
       func **findRange**\ (match: (Character)->Bool) -> Range<String.IndexType>

  .. Admonition:: Usage Example

     The ``NSString`` semantics can be acheived as follows:

     .. parsed-literal::

        someString.findRange( {someCharSet.contains($0)} )

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
   conformance to ``OutputStream``.  See the *Swift
   formatting proposal* for details.

:Swift:
  .. parsed-literal::
        func **+** (lhs: String, rhs: String) -> String
        func [infix,assignment] **+=** (lhs: [byref] String, rhs: String)
        func **append**\ (suffix: String)


Dynamic Formatting
~~~~~~~~~~~~~~~~~~

:Cocoa: 
  .. parsed-literal::
     \- (NSString \*)\ **stringByAppendingFormat:**\ (NSString \*)format, ... NS_FORMAT_FUNCTION(1,2);

:Swift: *Not directly provided*\ —see the *Swift formatting proposal*

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

:Swift: Not in ``String``\ —It is up to other types to provide their
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
     func split(maxSplit: Int = Int.max()) -> String[]
     func split(separator: Character, maxSplit: Int = Int.max()) -> String[]

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
    >(seq: Seq, isSeparator: IsSeparator, maxSplit: Int = Int.max(),
      allowEmptySlices: Bool = false  ) -> Seq[]

Splitting
~~~~~~~~~

:Cocoa: 
  .. parsed-literal::
     \- (NSString \*)\ **commonPrefixWithString:**\ (NSString \*)aString \ **options:**\ (NSStringCompareOptions)mask;

:Swift:
  .. parsed-literal::
     func **commonPrefix**\ (other: String) -> String

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
          no sense “Unicode-correct.”  In most other languages that
          support a ``capitalize`` method, it operates only on the
          first character of the string, and capitalization-by-word is
          named something like “``title``.”  If Swift ``String``
          supports capitalization by word, it should be
          Unicode-correct, but how we sort this particular area out is
          still **TBD**.

---------

:Cocoa: 
  .. parsed-literal::
     \- (NSString \*)\ **stringByTrimmingCharactersInSet:**\ (NSCharacterSet \*)set;

:Swift:
  .. parsed-literal::
       trim **trim**\ (match: (Character)->Bool) -> String

  .. Admonition:: Usage Example

     The ``NSString`` semantics can be acheived as follows:

     .. parsed-literal::

        someString.trim( {someCharSet.contains($0)} )

---------

:Cocoa: 
  .. parsed-literal::
     \- (NSString \*)\ **stringByPaddingToLength:**\ (NSUInteger)newLength \ **withString:**\ (NSString \*)padString \ **startingAtIndex:**\ (NSUInteger)padIndex;

:Swift:
  .. parsed-literal::
        **TBD**

.. Note:: It's not clear whether this is useful for non-ASCII strings.

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

.. [#unaware] Unicode specifies default (“un-tailored”)
   locale-independent collation_ and segmentation_ algorithms that
   make reasonable sense in most contexts.  Using these algorithms
   allows strings to be naturally compared and combined, generating
   the expected results when the content is ASCII

.. _segmentation: http://www.unicode.org/reports/tr29/#GB1

.. _collation: http://www.unicode.org/reports/tr10/


.. [#normalizationbit] We can use a bit to remember whether a given
   string buffer has been normalized.

.. [#elements] Since ``String`` is locale-unaware_, its elements are
   determined using Unicode's default, “un-tailored” segmentation_
   algorithm.
