// We need to require macOS because swiftSyntax currently doesn't build on Linux
// REQUIRES: OS=macosx
// RUN: %target-swift-ide-test -syntax-coloring -source-filename %s | %FileCheck %s -check-prefixes CHECK,CHECK-OLD
// RUN: %swift-swiftsyntax-test -classify-syntax -source-file %s | %FileCheck %s --check-prefixes CHECK,CHECK-NEW
// XFAIL: broken_std_regex

// CHECK: <comment-block>/* foo is the best */</comment-block>
/* foo is the best */
func foo(n: Float) {}

///- returns: single-line, no space
// CHECK-OLD: ///- <doc-comment-field>returns</doc-comment-field>: single-line, no space
// CHECK-NEW: <doc-comment-line>///- returns: single-line, no space</doc-comment-line>

/// - returns: single-line, 1 space
// CHECK-OLD: /// - <doc-comment-field>returns</doc-comment-field>: single-line, 1 space
// CHECK-NEW: <doc-comment-line>/// - returns: single-line, 1 space</doc-comment-line>

///  - returns: single-line, 2 spaces
// CHECK-OLD: ///  - <doc-comment-field>returns</doc-comment-field>: single-line, 2 spaces
// CHECK-NEW: <doc-comment-line>///  - returns: single-line, 2 spaces</doc-comment-line>

///       - returns: single-line, more spaces
// CHECK-OLD: ///       - <doc-comment-field>returns</doc-comment-field>: single-line, more spaces
// CHECK-NEW: <doc-comment-line>///       - returns: single-line, more spaces</doc-comment-line>

// CHECK: <kw>protocol</kw> Prot
protocol Prot {}

func f(x: Int) -> Int {
  // CHECK: <comment-line>// string interpolation is the best</comment-line>
  // string interpolation is the best
  "This is string \(x) interpolation"
}

// FIXME: blah.
//    FIXME:   blah blah
// Something something, FIXME: blah

// CHECK-OLD: <comment-line>// <comment-marker>FIXME: blah.</comment-marker></comment-line>
// CHECK-OLD: <comment-line>//    <comment-marker>FIXME:   blah blah</comment-marker></comment-line>
// CHECK-OLD: <comment-line>// Something something, <comment-marker>FIXME: blah</comment-marker></comment-line>
// CHECK-NEW: <comment-line>// FIXME: blah.</comment-line>
// CHECK-NEW: <comment-line>//    FIXME:   blah blah</comment-line>
// CHECK-NEW: <comment-line>// Something something, FIXME: blah</comment-line>



/* FIXME: blah*/

// CHECK-OLD: <comment-block>/* <comment-marker>FIXME: blah*/</comment-marker></comment-block>
// CHECK-NEW: <comment-block>/* FIXME: blah*/</comment-block>

/*
 * FIXME: blah
 * Blah, blah.
 */

// CHECK: <comment-block>/*
// CHECK-OLD:  * <comment-marker>FIXME: blah</comment-marker>
// CHECK-NEW:  * FIXME: blah
// CHECK:  * Blah, blah.
// CHECK:  */</comment-block>

// TODO: blah.
// TTODO: blah.
// MARK: blah.

// CHECK-OLD: <comment-line>// <comment-marker>TODO: blah.</comment-marker></comment-line>
// CHECK-OLD: <comment-line>// T<comment-marker>TODO: blah.</comment-marker></comment-line>
// CHECK-OLD: <comment-line>// <comment-marker>MARK: blah.</comment-marker></comment-line>
// CHECK-NEW: <comment-line>// TODO: blah.</comment-line>
// CHECK-NEW: <comment-line>// TTODO: blah.</comment-line>
// CHECK-NEW: <comment-line>// MARK: blah.</comment-line>

// CHECK: <kw>func</kw> test5() -> <type>Int</type> {
func test5() -> Int {
  // CHECK-OLD: <comment-line>// <comment-marker>TODO: something, something.</comment-marker></comment-line>
  // CHECK-NEW: <comment-line>// TODO: something, something.</comment-line>
  // TODO: something, something.
  // CHECK: <kw>return</kw> <int>0</int>
  return 0
}

// http://whatever.com?ee=2&yy=1 and radar://123456
/* http://whatever.com FIXME: see in http://whatever.com/fixme
  http://whatever.com */

// CHECK-OLD: <comment-line>// <comment-url>http://whatever.com?ee=2&yy=1</comment-url> and <comment-url>radar://123456</comment-url></comment-line>
// CHECK-OLD: <comment-block>/* <comment-url>http://whatever.com</comment-url> <comment-marker>FIXME: see in <comment-url>http://whatever.com/fixme</comment-url></comment-marker>
// CHECK-OLD:  <comment-url>http://whatever.com</comment-url> */</comment-block>
// CHECK-NEW: <comment-line>// http://whatever.com?ee=2&yy=1 and radar://123456</comment-line>
// CHECK-NEW: <comment-block>/* http://whatever.com FIXME: see in http://whatever.com/fixme
// CHECK-NEW:   http://whatever.com */</comment-block>

// CHECK-OLD: <comment-line>// <comment-url>http://whatever.com/what-ever</comment-url></comment-line>
// CHECK-NEW: <comment-line>// http://whatever.com/what-ever</comment-line>
// http://whatever.com/what-ever

/// Brief.
///
/// Simple case.
///
/// - parameter x: A number
/// - parameter y: Another number
/// - PaRamEteR z-hyphen-q: Another number
/// - parameter : A strange number...
/// - parameternope1: Another number
/// - parameter nope2
/// - parameter: nope3
/// -parameter nope4: Another number
/// * parameter nope5: Another number
///  - parameter nope6: Another number
///  - Parameters: nope7
/// - seealso: yes
///   - seealso: yes
/// - seealso:
/// -seealso: nope
/// - seealso : nope
/// - seealso nope
/// - returns: `x + y`
func foo(x: Int, y: Int) -> Int { return x + y }
// CHECK-OLD: <doc-comment-line>/// Brief.
// CHECK-OLD: </doc-comment-line><doc-comment-line>///
// CHECK-OLD: </doc-comment-line><doc-comment-line>/// Simple case.
// CHECK-OLD: </doc-comment-line><doc-comment-line>///
// CHECK-OLD: </doc-comment-line><doc-comment-line>/// - <doc-comment-field>parameter</doc-comment-field> x: A number
// CHECK-OLD: </doc-comment-line><doc-comment-line>/// - <doc-comment-field>parameter</doc-comment-field> y: Another number
// CHECK-OLD: </doc-comment-line><doc-comment-line>/// - <doc-comment-field>PaRamEteR</doc-comment-field> z-hyphen-q: Another number
// CHECK-OLD: </doc-comment-line><doc-comment-line>/// - <doc-comment-field>parameter</doc-comment-field> : A strange number...
// CHECK-OLD: </doc-comment-line><doc-comment-line>/// - parameternope1: Another number
// CHECK-OLD: </doc-comment-line><doc-comment-line>/// - parameter nope2
// CHECK-OLD: </doc-comment-line><doc-comment-line>/// - parameter: nope3
// CHECK-OLD: </doc-comment-line><doc-comment-line>/// -parameter nope4: Another number
// CHECK-OLD: </doc-comment-line><doc-comment-line>/// * parameter nope5: Another number
// CHECK-OLD: </doc-comment-line><doc-comment-line>///  - parameter nope6: Another number
// CHECK-OLD: </doc-comment-line><doc-comment-line>///  - Parameters: nope7
// CHECK-OLD: </doc-comment-line><doc-comment-line>/// - <doc-comment-field>seealso</doc-comment-field>: yes
// CHECK-OLD: </doc-comment-line><doc-comment-line>///   - <doc-comment-field>seealso</doc-comment-field>: yes
// CHECK-OLD: </doc-comment-line><doc-comment-line>/// - <doc-comment-field>seealso</doc-comment-field>:
// CHECK-OLD: </doc-comment-line><doc-comment-line>/// -seealso: nope
// CHECK-OLD: </doc-comment-line><doc-comment-line>/// - seealso : nope
// CHECK-OLD: </doc-comment-line><doc-comment-line>/// - seealso nope
// CHECK-OLD: </doc-comment-line><doc-comment-line>/// - <doc-comment-field>returns</doc-comment-field>: `x + y`
// CHECK-OLD: </doc-comment-line><kw>func</kw> foo(x: <type>Int</type>, y: <type>Int</type>) -> <type>Int</type> { <kw>return</kw> x + y }
// CHECK-NEW: <doc-comment-line>/// Brief.</doc-comment-line>
// CHECK-NEW: <doc-comment-line>///</doc-comment-line>
// CHECK-NEW: <doc-comment-line>/// Simple case.</doc-comment-line>
// CHECK-NEW: <doc-comment-line>///</doc-comment-line>
// CHECK-NEW: <doc-comment-line>/// - parameter x: A number</doc-comment-line>
// CHECK-NEW: <doc-comment-line>/// - parameter y: Another number</doc-comment-line>
// CHECK-NEW: <doc-comment-line>/// - PaRamEteR z-hyphen-q: Another number</doc-comment-line>
// CHECK-NEW: <doc-comment-line>/// - parameter : A strange number...</doc-comment-line>
// CHECK-NEW: <doc-comment-line>/// - parameternope1: Another number</doc-comment-line>
// CHECK-NEW: <doc-comment-line>/// - parameter nope2</doc-comment-line>
// CHECK-NEW: <doc-comment-line>/// - parameter: nope3</doc-comment-line>
// CHECK-NEW: <doc-comment-line>/// -parameter nope4: Another number</doc-comment-line>
// CHECK-NEW: <doc-comment-line>/// * parameter nope5: Another number</doc-comment-line>
// CHECK-NEW: <doc-comment-line>///  - parameter nope6: Another number</doc-comment-line>
// CHECK-NEW: <doc-comment-line>///  - Parameters: nope7</doc-comment-line>
// CHECK-NEW: <doc-comment-line>/// - seealso: yes</doc-comment-line>
// CHECK-NEW: <doc-comment-line>///   - seealso: yes</doc-comment-line>
// CHECK-NEW: <doc-comment-line>/// - seealso:</doc-comment-line>
// CHECK-NEW: <doc-comment-line>/// -seealso: nope</doc-comment-line>
// CHECK-NEW: <doc-comment-line>/// - seealso : nope</doc-comment-line>
// CHECK-NEW: <doc-comment-line>/// - seealso nope</doc-comment-line>
// CHECK-NEW: <doc-comment-line>/// - returns: `x + y`</doc-comment-line>
// CHECK-NEW: <kw>func</kw> foo(x: <type>Int</type>, y: <type>Int</type>) -> <type>Int</type> { <kw>return</kw> x + y }


/// Brief.
///
/// Simple case.
///
/// - Parameters:
///   - x: A number
///   - y: Another number
///
///- note: NOTE1
///
/// - NOTE: NOTE2
///   - note: Not a Note field (not at top level)
/// - returns: `x + y`
func bar(x: Int, y: Int) -> Int { return x + y }
// CHECK-OLD: <doc-comment-line>/// Brief.
// CHECK-OLD: </doc-comment-line><doc-comment-line>///
// CHECK-OLD: </doc-comment-line><doc-comment-line>/// Simple case.
// CHECK-OLD: </doc-comment-line><doc-comment-line>///
// CHECK-OLD: </doc-comment-line><doc-comment-line>/// - <doc-comment-field>Parameters</doc-comment-field>:
// CHECK-OLD: </doc-comment-line><doc-comment-line>/// - x: A number
// CHECK-OLD: </doc-comment-line><doc-comment-line>/// - y: Another number
// CHECK-OLD: </doc-comment-line><doc-comment-line>/// - <doc-comment-field>returns</doc-comment-field>: `x + y`
// CHECK-OLD: </doc-comment-line><kw>func</kw> bar(x: <type>Int</type>, y: <type>Int</type>) -> <type>Int</type> { <kw>return</kw> x + y }
// CHECK-NEW: <doc-comment-line>/// Brief.</doc-comment-line>
// CHECK-NEW: <doc-comment-line>///</doc-comment-line>
// CHECK-NEW: <doc-comment-line>/// Simple case.</doc-comment-line>
// CHECK-NEW: <doc-comment-line>///</doc-comment-line>
// CHECK-NEW: <doc-comment-line>/// - Parameters:</doc-comment-line>
// CHECK-NEW: <doc-comment-line>///   - x: A number</doc-comment-line>
// CHECK-NEW: <doc-comment-line>///   - y: Another number</doc-comment-line>
// CHECK-NEW: <doc-comment-line>///</doc-comment-line>
// CHECK-NEW: <doc-comment-line>///- note: NOTE1</doc-comment-line>
// CHECK-NEW: <doc-comment-line>///</doc-comment-line>
// CHECK-NEW: <doc-comment-line>/// - NOTE: NOTE2</doc-comment-line>
// CHECK-NEW: <doc-comment-line>///   - note: Not a Note field (not at top level)</doc-comment-line>
// CHECK-NEW: <doc-comment-line>/// - returns: `x + y`</doc-comment-line>
// CHECK-NEW: <kw>func</kw> bar(x: <type>Int</type>, y: <type>Int</type>) -> <type>Int</type> { <kw>return</kw> x + y }

/**
  Does pretty much nothing.

  Not a parameter list: improper indentation.
    - Parameters: sdfadsf

  - WARNING: - WARNING: Should only have one field

  - $$$: Not a field.

  Empty field, OK:
*/
func baz() {}
// CHECK: <doc-comment-block>/**
// CHECK:   Does pretty much nothing.
// CHECK:   Not a parameter list: improper indentation.
// CHECK:     - Parameters: sdfadsf
// CHECK-OLD:   - <doc-comment-field>WARNING</doc-comment-field>: - WARNING: Should only have one field
// CHECK-NEW:   - WARNING: - WARNING: Should only have one field
// CHECK:   - $$$: Not a field.
// CHECK:   Empty field, OK:
// CHECK: */</doc-comment-block>
// CHECK: <kw>func</kw> baz() {}

/***/
func emptyDocBlockComment() {}
// CHECK: <doc-comment-block>/***/</doc-comment-block>
// CHECK: <kw>func</kw> emptyDocBlockComment() {}

/**
*/
func emptyDocBlockComment2() {}
// CHECK: <doc-comment-block>/**
// CHECK: */
// CHECK: <kw>func</kw> emptyDocBlockComment2() {}

/**          */
func emptyDocBlockComment3() {}
// CHECK: <doc-comment-block>/**          */
// CHECK: <kw>func</kw> emptyDocBlockComment3() {}


/**/
func malformedBlockComment(f : () throws -> ()) rethrows {}
// CHECK: <doc-comment-block>/**/</doc-comment-block>

// CHECK-OLD: <kw>func</kw> malformedBlockComment(f : () <kw>throws</kw> -> ()) <attr-builtin>rethrows</attr-builtin> {}
// CHECK-NEW: <kw>func</kw> malformedBlockComment(f : () <kw>throws</kw> -> ()) <kw>rethrows</kw> {}

//: playground doc comment line
func playgroundCommentLine(f : () throws -> ()) rethrows {}
// CHECK: <comment-line>//: playground doc comment line</comment-line>

/*:
  playground doc comment multi-line
*/
func playgroundCommentMultiLine(f : () throws -> ()) rethrows {}
// CHECK: <comment-block>/*:
// CHECK: playground doc comment multi-line
// CHECK: */</comment-block>

/// [strict weak ordering](http://en.wikipedia.org/wiki/Strict_weak_order#Strict_weak_orderings)
// CHECK-OLD: <doc-comment-line>/// [strict weak ordering](<comment-url>http://en.wikipedia.org/wiki/Strict_weak_order#Strict_weak_orderings</comment-url>
// CHECK-NEW: <doc-comment-line>/// [strict weak ordering](http://en.wikipedia.org/wiki/Strict_weak_order#Strict_weak_orderings)</doc-comment-line>

/** aaa

 - returns: something
 */
// CHECK-OLD:  - <doc-comment-field>returns</doc-comment-field>: something
// CHECK-NEW:  - returns: something
let blah = 0

// Keep this as the last test
/**
  Trailing off ...
func unterminatedBlockComment() {}
// CHECK: <comment-line>// Keep this as the last test</comment-line>
// CHECK: <doc-comment-block>/**
// CHECK:  Trailing off ...
// CHECK:  func unterminatedBlockComment() {}
// CHECK:  </doc-comment-block>
