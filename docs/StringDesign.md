orphan

:   

<style> 

.repl, .emph, .look {color:rgb(47,175,187)}
.emph {font-weight:bold}

pre, .pre { font-family: Monaco, monospace; font-size:90% }

pre.literal-block {
  overflow: hidden;
}

span.look, span.look1 {
  position: relative;
  border-bottom: .2em dotted rgb(255,165,165);
}

span.aside { font-family: sans-serif; white-space: normal; }

span.look + span.aside, span.look1 + span.aside { display: none; }

span.look:hover, span.look1:hover {
  background-color:greenyellow;
}

span.look:hover {
  color:rgb(23,87,94);
}

/* Main speech bubble*/
span.look:hover + span.aside, span.look1:hover + span.aside{
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
span.look:hover + span.aside:after, span.look1:hover + span.aside:after {
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
.. role:: look1 .. role:: aside .. role:: emph

Swift String Design
===================

> **This Document**
>
> -   contains interactive HTML commentary that does not currently
>     appear in printed output. Hover your mouse over elements with a
>     dotted pink underline to view the hidden commentary.
> -   represents the intended design of Swift strings, not their current
>     implementation state.
> -   is being delivered in installments. Content still to come is
>     outlined in <span class="repl">Coming Installments</span>\_.

> **warning**
>
> This document was used in planning Swift 1.0; it has not been kept

> up to date and does not describe the current or planned behavior of
> Swift.

Introduction
------------

Like all things Swift, our approach to strings begins with a deep
respect for the lessons learned from many languages and libraries,
especially Objective-C and Cocoa.

### Goals

`String` should:

-   honor industry standards such as Unicode
-   when handling non-ASCII text, deliver “reasonably correct” results
    to users thinking only in terms of ASCII
-   when handling ASCII text, provide “expected behavior” to users
    thinking only in terms of ASCII
-   be hard to use incorrectly
-   be easy to use correctly
-   provide near-optimal efficiency for 99% of use cases
-   provide a foundation upon which proper locale-sensitive operations
    can be built

### Non-Goals

`String` need not:

-   have behavior appropriate to all locales and contexts
-   be an appropriate type (or base type) for all text storage
    applications

Overview By Example
-------------------

In this section, we'll walk through some basic examples of Swift string
usage while discovering its essential properties.

### `String` is a [First-Class Type](http://en.wikipedia.org/wiki/First-class_citizen)

Unlike, say, C's `char*`, the meaning of a swift string is always
unambiguous.

### Strings are **Efficient**

The implementation of `String` takes advantage of state-of-the-art
optimizations, including:

-   Storing short strings without heap allocation
-   Sharing allocated buffers among copies and slices
-   In-place modification of uniquely-owned buffers

As a result, copying\_ and [slicing](sliceable_) strings, in particular,
can be viewed by most programmers as being “almost free.”

### Strings are **Mutable**

> **Why Mention It?**
>
> The ability to change a string's value might not be worth noting
> except that *some languages make all strings immutable*, as a way of
> working around problems that Swift has defined away—by making strings
> pure values (see below).

> (swift) extension String {
>
> :   
>
>     func addEcho() {
>
>     :   self += self
>
>     }
>
> > }
>
> (swift) :look1:<span
> class="repl">s.addEcho()</span>s is modified in place (swift) s <span
> class="repl">// s: String =</span> "YoYo"

### Strings are **Value Types**

Distinct string variables have independent values: when you pass someone
a string they get a copy of the value, and when someone passes you a
string *you own it*. Nobody can change a string value “behind your
back.”

> (swift) class Cave {
>
> :   // Utter something in the cave func say(msg: String) -&gt; String
>     { :look1:<span
>     class="repl">msg.addEcho()</span>Modifying a parameter is safe because the callee sees a copy of the argument
>     self.lastSound = msg :look1:<span
>     class="repl">return self.lastSound</span>Returning a stored value is safe because the caller sees a copy of the value
>     }
>
>     var lastSound: String // a Cave remembers the last sound made
>
> > }
>
> (swift) var c = Cave() <span
> class="repl">// c: Cave = &lt;Cave instance&gt;</span> (swift) s =
> "Hey" (swift) var t = :look1:<span
> class="repl">c.say(s)</span>this call can't change s… <span
> class="repl">// t: String = "HeyHey"</span> (swift) s <span
> class="repl">// s: String =</span> <span
> class="look">"Hey"</span>…and it doesn't. (swift) :look1:<span
> class="repl">t.addEcho()</span>this call can't change c.lastSound…
> (swift) \[s, c.lastSound, t\] <span
> class="repl">// r0: String\[\] = \["Hey",</span> <span
> class="look">"HeyHey"</span>…and it doesn't.<span
> class="repl">, "HeyHeyHeyHey"\]</span>

### Strings are **Unicode-Aware**

> **Deviations from Unicode**
>
> Any deviation from what Unicode specifies requires careful
> justification. So far, we have found two possible points of deviation
> for Swift `String`:
>
> 1.  The [Unicode Text Segmentation
>     Specification](http://www.unicode.org/reports/tr29) says, “[do not
>     break between CR and
>     LF](http://www.unicode.org/glossary/#grapheme_cluster).” However,
>     breaking extended grapheme clusters between CR and LF may
>     necessary if we wish `String` to “behave normally” for users of
>     pure ASCII. This point is still open for discussion.
>
>     \_\_ <http://www.unicode.org/reports/tr29/#GB2>
>
> 2.  The [Unicode Text Segmentation
>     Specification](http://www.unicode.org/reports/tr29) says, “[do not
>     break between regional indicator
>     symbols](http://useless-factor.blogspot.com/2007/08/unicode-implementers-guide-part-4.html).”
>     However, it also says “(Sequences of more than two RI characters
>     should be separated by other characters, such as U+200B ZWSP).”
>     Although the parenthesized note probably has less official weight
>     than the other admonition, breaking pairs of RI characters seems
>     like the right thing for us to do given that Cocoa already forms
>     strings with several adjacent pairs of RI characters, and the
>     Unicode spec *can* be read as outlawing such strings anyway.
>
>     \_\_ <http://www.unicode.org/reports/tr29/#GB8>
>
Swift applies Unicode algorithms wherever possible. For example,
distinct sequences of code points are treated as equal if they represent
the same character: [^1]

> (swift) var n1 = ":look1:<span
> class="repl">\\\\u006E\\\\u0303</span>Multiple code points, but only one Character"
> <span class="repl">// n1 : String =</span> **"ñ"** (swift) var n2 =
> "\\u00F1" <span class="repl">// n2 : String =</span> **"ñ"** (swift)
> n1 == n2 <span class="repl">// r0 : Bool =</span> **true**

Note that individual code points are still observable by explicit
request:

> (swift) n1.codePoints == n2.codePoints <span
> class="repl">// r0 : Bool =</span> **false**

### Strings are **Locale-Agnostic**

Strings neither carry their own locale information, nor provide
behaviors that depend on a global locale setting. Thus, for any pair of
strings `s1` and `s2`, “`s1 == s2`” yields the same result regardless of
system state. Strings *do* provide a suitable foundation on which to
build locale-aware interfaces.[^2]

### Strings are **Containers**

> **String Indices**
>
> `String` implements the `Container` protocol, but **cannot be indexed
> by integers**. Instead, `String.IndexType` is a library type
> conforming to the `BidirectionalIndex` protocol.
>
> This might seem surprising at first, but code that indexes strings
> with arbitrary integers is seldom Unicode-correct in the first place,
> and Swift provides alternative interfaces that encourage
> Unicode-correct code. For example, instead of `s[0] == 'S'` you'd
> write `s.startsWith("S")`.

### Strings are Composed of `Character`s

`Character`, the element type of `String`, represents a **grapheme
cluster**, as specified by a default or tailored Unicode segmentation
algorithm. This term is [precisely
defined](http://www.unicode.org/reports/tr29/#Default_Grapheme_Cluster_Table)
by the Unicode specification, but it roughly means [what the user thinks
of when she hears
“character”](http://www.unicode.org/glossary/#code_unit). For example,
the pair of code points “LATIN SMALL LETTER N, COMBINING TILDE” forms a
single grapheme cluster, “ñ”.

Access to lower-level elements is still possible by explicit request:

Strings Support Flexible Segmentation
-------------------------------------

The `Character`s enumerated when simply looping over elements of a Swift
string are [extended grapheme clusters](length_) as determined by
Unicode's <span class="repl">Default Grapheme Cluster Boundary
Specification</span>\_\_. [^3]

This segmentation offers naïve users of English, Chinese, French, and
probably a few other languages what we think of as the “expected
results.” However, not every
[script](http://www.unicode.org/glossary/#script) can be segmented
uniformly for all purposes. For example, searching and collation require
different segmentations in order to handle Indic scripts correctly. To
that end, strings support properties for more-specific segmentations:

> **note**
>
> The following example needs a more interesting string in
>
> :   order to demonstrate anything interesting. Hopefully Aki has some
>     advice for us.
>
Also, each such segmentation provides a unique `IndexType`, allowing a
string to be indexed directly with different indexing schemes:

    |swift| var i = s.searchCharacters.startIndex
    `// r2 : UInt8 = UInt8(83)`

### Strings are **Sliceable**

### Strings are **Encoded as UTF-8**

> **Encoding Conversion**
>
> Conversion to and from other encodings is out-of-scope for `String`
> itself, but could be provided, e.g., by an `Encoding` module.

Coming Installments
-------------------

-   Reference Manual
-   Rationales
-   Cocoa Bridging Strategy
-   Comparisons with NSString
    -   High Level
    -   Member-by-member

Reference Manual
----------------

-   s.bytes
-   s.indices
-   s\[i\]
-   s\[start...end\]
-   s == t, s != t
-   s &lt; t, s &gt; t, s &lt;= t, s &gt;= t
-   s.hash()
-   s.startsWith(), s.endsWith()
-   s + t, s += t, s.append(t)
-   s.split(), s.split(n), s.split(sep, n)
-   s.strip(), s.stripStart(), s.stripEnd()
-   s.commonPrefix(t), s.mismatch(t)
-   s.toUpper(), s.toLower()
-   s.trim(predicate)
-   s.replace(old, new, count)
-   s.join(sequenceOfStrings)

Cocoa Bridging Strategy
-----------------------

Rationales
----------

### Why a Built-In String Type?

> **DaveZ Sez**
>
> In the "why a built-in string type" section, I think the main
> narrative is that two string types is bad, but that we have two string
> types in Objective-C for historically good reasons. To get one string
> type, we need to merge the high-level features of Objective-C with the
> performance of C, all while not having the respective bad the bad
> semantics of either (reference semantics and "anarchy"
> memory-management respectively). Furthermore, I'd write "value
> semantics" in place of "C++ semantics". I know that is what you meant,
> but we need to tread carefully in the final document.

`NSString` and `NSMutableString`—the string types provided by Cocoa—are
full-featured classes with high-level functionality for writing
fully-localized applications. They have served Apple programmers well;
so, why does Swift have its own string type?

-   ObjCMessageSend
-   Error Prone Mutability Reference semantics don't line up with how
    people think about strings
-   2 is too many string types. two APIs duplication of effort
    documentation Complexity adds decisions for users etc.
-   ObjC needed to innovate because C strings suck O(N) length no
    localization no memory management no specified encoding
-   C strings had to stay around for performance reasons and
    interoperability

Want performance of C, sane semantics of C++ strings, and high-level
goodness of ObjC.

> The design of `NSString` is *very* different from the string designs
> of most modern programming languages, which all tend to be very
> similar to one another. Although existing `NSString` users are a
> critical constituency today, current trends indicate that most of our
> *future* target audience will not be `NSString` users. Absent
> compelling justification, it's important to make the Swift programming
> environment as familiar as possible for them.

### How Would You Design It?

> **DaveZ Sez**
>
> In the "how would you design it" section, the main narrative is
> twofold: how does it "feel" and how efficient is it? The former is
> about feeling built in, which we can easily argue that both C strings
> or Cocoa strings fail at for their respective semantic (and often
> memory management related) reasons. Additionally, the "feel" should be
> modern, which is where the Cocoa framework and the Unicode standard
> body do better than C. Nevertheless, we can still do better than
> Objective-C and your strong work at helping people reason about
> grapheme clusters instead of code points (or worse, units) is
> wonderful and it feels right to developers. The second part of the
> narrative is about being efficient, which is where arguing for UTF8 is
> the non-obvious but "right" answer for the reasons we have discussed.

-   It'd be an independent *value* so you don't have to micromanage
    sharing and mutation
-   It'd be UTF-8 because:
    -   UTF-8 has been the clear winner\_\_ among Unicode encodings
        since at least 2008; Swift should interoperate smoothly and
        efficiently with the rest of the world's systems

        \_\_ <http://www.artima.com/weblogs/viewpost.jsp?thread=230157>

    -   UTF-8 is a fairly efficient storage format, especially for ASCII
        but also for the most common non-ASCII code points.
    -   This\_\_ posting elaborates on some other nice qualities of
        UTF-8:

        1.  All ASCII files are already UTF-8 files
        2.  ASCII bytes always represent themselves in UTF-8 files. They
            never appear as part of other UTF-8 sequences
        3.  ASCII code points are always represented as themselves in
            UTF-8 files. They cannot be hidden inside multibyte UTF-8
            sequences
        4.  UTF-8 is self-synchronizing
        5.  CodePoint substring search is just byte string search
        6.  Most programs that handle 8-bit files safely can handle
            UTF-8 safely
        7.  UTF-8 sequences sort in code point order.
        8.  UTF-8 has no “byte order.”

        \_\_
        <http://research.swtch.com/2010/03/utf-8-bits-bytes-and-benefits.html>

-   It would be efficient, taking advantage of state-of-the-art
    optimizations, including:
    -   Storing short strings without heap allocation
    -   Sharing allocated buffers among copies and slices
    -   In-place modification of uniquely-owned buffers

Comparisons with `NSString`
---------------------------

### High-Level Comparison with `NSString`

> **DaveZ Sez**
>
> I think the main message of the API breadth subsection is that URLs,
> paths, etc would be modeled as formal types in Swift (i.e. not as
> extensions on String). Second, I'd speculate less on what Foundation
> could do (like extending String) and instead focus on the fact that
> NSString still exists as an escape hatch for those that feel that they
> need or want it. Furthermore, I'd move up the "element access"
> discussion above the "escape hatch" discussion (which should be last
> in the comparison with NSString discussion).

#### API Breadth

The `NSString` interface clearly shows the effects of 20 years of
evolution through accretion. It is broad, with functionality addressing
encodings, paths, URLs, localization, and more. By contrast, the
interface to Swift's `String` is much narrower.

Of course, there's a reason for every `NSString` method, and the full
breadth of `NSString` functionality must remain accessible to the
Cocoa/Swift programmer. Fortunately, there are many ways to address this
need. For example:

-   The `Foundation` module can extend `String` with the methods of
    `NSString`. The extent to which we provide an identical-feeling
    interface and/or correct any `NSString` misfeatures is still TBD and
    wide open for discussion.
-   We can create a new modular interface in pure Swift, including a
    `Locale` module that addresses localized string operations, an
    `Encoding` module that addresses character encoding schemes, a
    `Regex` module that provides regular expression functionality, etc.
    Again, the specifics are TBD.
-   When all else fails, users can convert their Swift `String`s to
    `NSString`s when they want to access `NSString`-specific
    functionality:

For Swift version 1.0, we err on the side of keeping the string
interface small, coherent, and sufficient for implementing higher-level
functionality.

#### Element Access

`NSString` exposes UTF-16 <span class="repl">code units</span>\_\_ as
the primary element on which indexing, slicing, and iteration operate.
Swift's UTF-8 code units are only available as a secondary interface.

`NSString` is indexable and sliceable using `Int`s, and so exposes a
`length` attribute. Swift's `String` is indexable and sliceable using an
abstract `BidirectionalIndex` type, and <span class="repl">does not
expose its length</span>\_\_.

#### Sub-Strings

Creating substrings in Swift is very fast. Therefore, Cocoa APIs that
operate on a substring given as an `NSRange` are replaced with Swift
APIs that just operate on `String`s. One can use range-based
subscripting to achieve the same effect. For example:
`[str doFoo:arg withRange:subrange]` becomes `str[subrange].doFoo(arg)`.

### `NSString` Member-by-Member Comparison

Notes

:   -   The following are from public headers from public frameworks,
        which are AppKit and Foundation (verified).
    -   Deprecated Cocoa APIs are not considered
    -   A status of “*Remove*” below indicates a feature whose removal
        is anticipated. Rationale is provided for these cases.

#### Indexing

------------------------------------------------------------------------

> **Why doesn't `String` support `.length`?**
>
> In Swift, by convention, `x.length` is used to represent the number of
> elements in a container, and since `String` is a container of abstract
> `Character`\_s, `length` would have to count those.
>
> This meaning of `length` is unimplementable in O(1). It can be cached,
> although not in the memory block where the characters are stored,
> since we want a `String` to share storage with its slices. Since the
> body of the `String` must already store the `String`'s *byte length*,
> caching the `length` would increase the footprint of the top-level
> String object. Finally, even if `length` were provided, doing things
> with `String` that depend on a specific numeric `length` is
> error-prone.

Cocoa

:   

Swift

:   *not directly provided*, but similar functionality is available:

------------------------------------------------------------------------

Cocoa

:   

Swift

:   typealias IndexType = ... func **indices**() -&gt;
    > Range&lt;IndexType&gt; **subscript**(i: IndexType) -&gt; Character

    > **Usage**

#### Slicing

Cocoa

:   

Swift

:   typealias IndexType = ... **subscript**(r: Range&lt;IndexType&gt;)
    > -&gt; Character

#### Indexing

Cocoa

:   

Swift

:   **subscript**(range : Range&lt;IndexType&gt;) -&gt; String

    > **Example**
    >
    > Note
    >
    > :   Swift may need additional interfaces to support `index...` and
    >     `...index` notations. This part of the `Container` protocol
    >     design isn't worked out yet.
    >
#### Comparison

Cocoa

:   

Swift

:   

`NSString` comparison is “literal” by default. As the documentation says
of `isEqualToString`,

> “Ö” represented as the composed character sequence “O” and umlaut
> would not compare equal to “Ö” represented as one Unicode character.

By contrast, Swift string's primary comparison interface uses Unicode's
default [collation](http://www.unicode.org/reports/tr10/) algorithm, and
is thus always “Unicode-correct.” Unlike comparisons that depend on
locale, it is also stable across changes in system state. However, *just
like* `NSString`'s `isEqualToString` and `compare` methods, it should
not be expected to yield ideal (or even “proper”) results in all
contexts.

------------------------------------------------------------------------

Cocoa

:   

Swift

:   *various compositions of primitive operations* / TBD\_

-   As noted above\_\_, instead of passing sub-range arguments, we
    expect Swift users to compose slicing\_ with
    whole-string operations.

    \_\_ range\_

-   Other details of these interfaces are distinguished by an
    `NSStringCompareOptions` mask, of which `caseInsensitiveCompare:` is
    essentially a special case:

    `NSCaseInsensitiveSearch`

    :   Whether a direct interface is needed at all in Swift, and if so,
        its form, are TBD\_. However, we should consider following the
        lead of Python 3, wherein case conversion also <span
        class="repl">normalizes letterforms</span>\_\_. Then one can
        combine `String.toLower()` with default comparison to get a
        case-insensitive comparison:

            { $0.toLower() == $1.toLower() }

        \_\_ <http://stackoverflow.com/a/11573384/125349>

    `NSLiteralSearch`

    :   Though it is the default for `NSString`, this option is
        essentially only useful as a performance optimization when the
        string content is known to meet certain restrictions (i.e. is
        known to be pure ASCII). When such optimization is absolutely
        necessary, Swift standard library algorithms can be used
        directly on the `String`'s UTF8 code units. However, Swift will
        also perform these optimizations automatically (at the cost of a
        single test/branch) in many cases, because each `String` stores
        a bit indicating whether its content is known to be ASCII.

    `NSBackwardsSearch`

    :   It's unclear from the docs how this option interacts with other
        `NSString` options, if at all, but basic cases can be handled in
        Swift by `s1.endsWith(s2)`.

    `NSAnchoredSearch`

    :   Not applicable to whole-string comparisons

    `NSNumericSearch`

    :   While it's legitimate to defer this functionality to Cocoa, it's
        (probably—see &lt;rdar://problem/14724804&gt;)
        locale-independent and easy enough to implement in Swift. TBD\_

    `NSDiacriticInsensitiveSearch`

    :   Ditto; TBD\_

    `NSWidthInsensitiveSearch`

    :   Ditto; TBD\_

    `NSForcedOrderingSearch`

    :   Ditto; TBD\_. Also see &lt;rdar://problem/14724888&gt;

    `NSRegularExpressionSearch`

    :   We can defer this functionality to Cocoa, or dispatch directly
        to ICU as an optimization. It's unlikely that we'll be building
        Swift its own regexp engine for 1.0.

------------------------------------------------------------------------

Cocoa

:   

Swift

:   As these all depend on locale, they are TBD\_

#### Searching

> **Rationale**
>
> Modern languages (Java, C\#, Python, Ruby…) have standardized on
> variants of `startsWith`/`endsWith`. There's no reason Swift should
> deviate from de-facto industry standards here.

Cocoa

:   

Swift

:   

------------------------------------------------------------------------

Cocoa

:   

Swift

:   **note**
    >
    > Most other languages provide something like
    >
    > :   `s1.indexOf(s2)`, which returns only the starting index of the
    >     first match. This is far less useful than the range of the
    >     match, and is always available via `s1.find(s2).bounds.0`
    >
------------------------------------------------------------------------

Cocoa

:   

> **Naming**
>
> The Swift function is just an algorithm that comes from conformance to
> the `Container` protocol, which explains why it doesn't have a
> `String`-specific name.

Swift

:   **Usage Example**
    >
    > The `NSString` semantics can be achieved as follows:

------------------------------------------------------------------------

Cocoa

:   These functions

Swift

:   *various compositions of primitive operations* / TBD\_

#### Building

Cocoa

:   

> **`append`**
>
> the `append` method is a consequence of `String`'s conformance to
> `OutputStream`. See the *Swift formatting proposal* for details.

Swift

:   

#### Dynamic Formatting

Cocoa

:   

Swift

:   *Not directly provided*—see the *Swift formatting proposal*

#### Extracting Numeric Values

Cocoa

:   

Swift

:   Not in `String`—It is up to other types to provide their conversions
    to and from String. See also this <span
    class="repl">rationale</span>\_\_

    \_\_ extending\_

#### Splitting

Cocoa

:   

Swift

:   The semantics of these functions were taken from Python, which seems
    to be a fairly good representative of what modern languages are
    currently doing. The first overload splits on all whitespace
    characters; the second only on specific characters. The universe of
    possible splitting functions is quite broad, so the particulars of
    this interface are **wide open for discussion**. In Swift right now,
    these methods (on `CodePoints`) are implemented in terms of a
    generic algorithm:

#### Splitting

Cocoa

:   

Swift

:   

#### Upper/Lowercase

Cocoa

:   

> **Naming**
>
> Other languages have overwhelmingly settled on `upper()` or
> `toUpper()` for this functionality

Swift

:   

#### Capitalization

Cocoa

:   

Swift

:   **TBD**

> **note**
>
> `NSString` capitalizes the first letter of each substring
>
> :   separated by spaces, tabs, or line terminators, which is in no
>     sense “Unicode-correct.” In most other languages that support a
>     `capitalize` method, it operates only on the first character of
>     the string, and capitalization-by-word is named something like
>     “`title`.” If Swift `String` supports capitalization by word, it
>     should be Unicode-correct, but how we sort this particular area
>     out is still **TBD**.
>
------------------------------------------------------------------------

Cocoa

:   

Swift

:   **Usage Example**
    >
    > The `NSString` semantics can be achieved as follows:

------------------------------------------------------------------------

Cocoa

:   

Swift

:   

------------------------------------------------------------------------

Cocoa

:   

Swift

:   

------------------------------------------------------------------------

Cocoa

:   

Swift

:   

------------------------------------------------------------------------

Cocoa

:   

Swift

:   

------------------------------------------------------------------------

Cocoa

:   

Swift

:   

------------------------------------------------------------------------

Cocoa

:   

Swift

:   

------------------------------------------------------------------------

Cocoa

:   

Swift

:   

------------------------------------------------------------------------

Cocoa

:   

Swift

:   

------------------------------------------------------------------------

Cocoa

:   

Swift

:   

------------------------------------------------------------------------

Cocoa

:   

Swift

:   

------------------------------------------------------------------------

Cocoa

:   

Swift

:   

------------------------------------------------------------------------

Cocoa

:   

Swift

:   

------------------------------------------------------------------------

Cocoa

:   

Swift

:   

-   (BOOL)**canBeConvertedToEncoding:**(NSStringEncoding)encoding;

------------------------------------------------------------------------

Cocoa

:   

Swift

:   

------------------------------------------------------------------------

Cocoa

:   

Swift

:   

------------------------------------------------------------------------

Cocoa

:   

Swift

:   

------------------------------------------------------------------------

Cocoa

:   

Swift

:   

------------------------------------------------------------------------

Cocoa

:   

Swift

:   

------------------------------------------------------------------------

Cocoa

:   

Swift

:   

------------------------------------------------------------------------

Cocoa

:   

Swift

:   

------------------------------------------------------------------------

Cocoa

:   

Swift

:   

------------------------------------------------------------------------

Cocoa

:   

Swift

:   

------------------------------------------------------------------------

Cocoa

:   

Swift

:   

------------------------------------------------------------------------

Cocoa

:   

Swift

:   

------------------------------------------------------------------------

Cocoa

:   

Swift

:   

------------------------------------------------------------------------

Cocoa

:   

------------------------------------------------------------------------

Cocoa

:   

Swift

:   

------------------------------------------------------------------------

Cocoa

:   

Swift

:   

------------------------------------------------------------------------

Cocoa

:   

Swift

:   

------------------------------------------------------------------------

Cocoa

:   

#### Constructors

Cocoa

:   

------------------------------------------------------------------------

Cocoa

:   

------------------------------------------------------------------------

Cocoa

:   + (instancetype)string;

------------------------------------------------------------------------

Cocoa

:   + (instancetype)**stringWithString:**(NSString \*)string;

Not available (too error prone)

------------------------------------------------------------------------

Cocoa

:   

Swift

:   

------------------------------------------------------------------------

Cocoa

:   

Swift

:   

------------------------------------------------------------------------

Cocoa

:   

Swift

:   

------------------------------------------------------------------------

Cocoa

:   

Swift

:   

------------------------------------------------------------------------

Cocoa

:   

Swift

:   

------------------------------------------------------------------------

Cocoa

:   

Swift

:   

------------------------------------------------------------------------

Cocoa

:   

Swift

:   

------------------------------------------------------------------------

Cocoa

:   

Swift

:   

------------------------------------------------------------------------

Cocoa

:   

Swift

:   

------------------------------------------------------------------------

Cocoa

:   

Swift

:   

------------------------------------------------------------------------

Cocoa

:   

Swift

:   

------------------------------------------------------------------------

Cocoa

:   

Swift

:   

------------------------------------------------------------------------

Cocoa

:   

Swift

:   

------------------------------------------------------------------------

Cocoa

:   

Swift

:   

------------------------------------------------------------------------

Cocoa

:   

Swift

:   

------------------------------------------------------------------------

Cocoa

:   

#### Linguistic Analysis

Cocoa

:   

Swift

:   

### Unavailable on Swift Strings

#### URL Handling

See: class File

#### Path Handling

#### Property Lists

Property lists are a feature of Cocoa.

#### Deprecated APIs

Already deprecated in Cocoa.

------------------------------------------------------------------------

### Why YAGNI

-   Retroactive Modeling
-   Derivation
-   ...

[^1]: Technically, `==` checks for [Unicode canonical
    equivalence](http://www.unicode.org/glossary/#extended_grapheme_cluster)

    \_\_ <http://www.unicode.org/reports/tr15/tr15-18.html#Introduction>

[^2]: We have some specific ideas for locale-sensitive interfaces, but
    details are still TBD and wide open for discussion.

[^3]: The type currently called `Char` in Swift represents a Unicode
    code point. This document refers to it as `CodePoint`, in
    anticipation of renaming.
