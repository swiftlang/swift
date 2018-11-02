// RUN: %target-swift-ide-test -syntax-coloring -source-filename %s | %FileCheck %s
// RUN: %target-swift-ide-test -syntax-coloring -typecheck -source-filename %s | %FileCheck %s -check-prefix CHECK -check-prefix CHECK-OLD
// RUN: %target-swift-ide-test -syntax-coloring -force-libsyntax-based-processing -source-filename %s | %FileCheck %s --check-prefix CHECK -check-prefix CHECK-NEW
// XFAIL: broken_std_regex

#line 17 "abc.swift"
// CHECK: <kw>#line</kw> <int>17</int> <str>"abc.swift"</str>

@available(iOS 8.0, OSX 10.10, *)
// CHECK: <attr-builtin>@available</attr-builtin>(<kw>iOS</kw> <float>8.0</float>, <kw>OSX</kw> <float>10.10</float>, *)
func foo() {
// CHECK: <kw>if</kw> <kw>#available</kw> (<kw>OSX</kw> <float>10.10</float>, <kw>iOS</kw> <float>8.01</float>, *) {<kw>let</kw> <kw>_</kw> = <str>"iOS"</str>}
  if #available (OSX 10.10, iOS 8.01, *) {let _ = "iOS"}
}

enum List<T> {
  case Nil
  // rdar://21927124
  // CHECK-OLD: <attr-builtin>indirect</attr-builtin> <kw>case</kw> Cons(T, List)
  // CHECK-NEW: <attr-builtin>indirect</attr-builtin> <kw>case</kw> Cons(<type>T</type>, <type>List</type>)
  indirect case Cons(T, List)
}

// CHECK: <kw>struct</kw> S {
struct S {
  // CHECK: <kw>var</kw> x : <type>Int</type>
  var x : Int
  // CHECK: <kw>var</kw> y : <type>Int</type>.<type>Int</type>
  var y : Int.Int
  // CHECK: <kw>var</kw> a, b : <type>Int</type>
  var a, b : Int
}

enum EnumWithDerivedEquatableConformance : Int {
// CHECK-LABEL: <kw>enum</kw> EnumWithDerivedEquatableConformance : {{(<type>)}}Int{{(</type>)?}} {
  case CaseA
// CHECK-NEXT: <kw>case</kw> CaseA
  case CaseB, CaseC
// CHECK-NEXT: <kw>case</kw> CaseB, CaseC
  case CaseD = 30, CaseE
// CHECK-NEXT: <kw>case</kw> CaseD = <int>30</int>, CaseE
}
// CHECK-NEXT: }

// CHECK: <kw>class</kw> MyCls {
class MyCls {
    // CHECK: <kw>var</kw> www : <type>Int</type>
    var www : Int

    // CHECK: <kw>func</kw> foo(x: <type>Int</type>) {}
    func foo(x: Int) {}
    // CHECK: <kw>var</kw> aaa : <type>Int</type> {
    var aaa : Int {
      // CHECK: <kw>get</kw> {}
      get {}
      // CHECK: <kw>set</kw> {}
      set {}
    }
    // CHECK: <kw>var</kw> bbb : <type>Int</type> {
    var bbb : Int {
      // CHECK: <kw>set</kw> {
      set {
       // CHECK: <kw>var</kw> tmp : <type>Int</type>
        var tmp : Int
      }
      // CHECK: <kw>get</kw> {
      get {
       // CHECK: <kw>var</kw> tmp : <type>Int</type>
       var tmp : Int
      }
    }

    // CHECK: <kw>subscript</kw> (i : <type>Int</type>, j : <type>Int</type>) -> <type>Int</type> {
    subscript (i : Int, j : Int) -> Int {
      // CHECK: <kw>get</kw> {
      get {
        // CHECK: <kw>return</kw> i + j
        return i + j
      }
      // CHECK: <kw>set</kw>(v) {
      set(v) {
        // CHECK: v + i - j
        v + i - j
      }
    }

    // CHECK: <kw>func</kw> multi(<kw>_</kw> name: <type>Int</type>, otherpart x: <type>Int</type>) {}
    func multi(_ name: Int, otherpart x: Int) {}
}

// CHECK-LABEL: <kw>class</kw> Attributes {
class Attributes {
// CHECK: <attr-builtin>@IBOutlet</attr-builtin> <kw>var</kw> v0: <type>Int</type>
  @IBOutlet var v0: Int

// CHECK-OLD: <attr-builtin>@IBOutlet</attr-builtin> <attr-id>@IBOutlet</attr-id> <kw>var</kw> v1: <type>String</type>
// CHECK-NEW: <attr-builtin>@IBOutlet</attr-builtin> <attr-builtin>@IBOutlet</attr-builtin> <kw>var</kw> v1: <type>String</type>
  @IBOutlet @IBOutlet var v1: String

// CHECK: <attr-builtin>@objc</attr-builtin> <attr-builtin>@IBOutlet</attr-builtin> <kw>var</kw> v2: <type>String</type>
  @objc @IBOutlet var v2: String

// CHECK: <attr-builtin>@IBOutlet</attr-builtin> <attr-builtin>@objc</attr-builtin> <kw>var</kw> v3: <type>String</type>
  @IBOutlet @objc var v3: String

// CHECK: <attr-builtin>@available</attr-builtin>(*, unavailable) <kw>func</kw> f1() {}
  @available(*, unavailable) func f1() {}

// CHECK: <attr-builtin>@available</attr-builtin>(*, unavailable) <attr-builtin>@IBAction</attr-builtin> <kw>func</kw> f2() {}
  @available(*, unavailable) @IBAction func f2() {}

// CHECK: <attr-builtin>@IBAction</attr-builtin> <attr-builtin>@available</attr-builtin>(*, unavailable) <kw>func</kw> f3() {}
  @IBAction @available(*, unavailable) func f3() {}

// CHECK: <attr-builtin>mutating</attr-builtin> <kw>func</kw> func_mutating_1() {}
  mutating func func_mutating_1() {}

// CHECK: <attr-builtin>nonmutating</attr-builtin> <kw>func</kw> func_mutating_2() {}
  nonmutating func func_mutating_2() {}
}

func stringLikeLiterals() {
// CHECK: <kw>var</kw> us1: <type>UnicodeScalar</type> = <str>"a"</str>
  var us1: UnicodeScalar = "a"
// CHECK: <kw>var</kw> us2: <type>UnicodeScalar</type> = <str>"ы"</str>
  var us2: UnicodeScalar = "ы"

// CHECK: <kw>var</kw> ch1: <type>Character</type> = <str>"a"</str>
  var ch1: Character = "a"
// CHECK: <kw>var</kw> ch2: <type>Character</type> = <str>"あ"</str>
  var ch2: Character = "あ"

// CHECK: <kw>var</kw> s1 = <str>"abc абвгд あいうえお"</str>
  var s1 = "abc абвгд あいうえお"
}

// CHECK: <kw>var</kw> globComp : <type>Int</type>
var globComp : Int {
  // CHECK: <kw>get</kw> {
  get {
    // CHECK: <kw>return</kw> <int>0</int>
    return 0
  }
}

// CHECK: <comment-block>/* foo is the best */</comment-block>
/* foo is the best */
// CHECK: <kw>func</kw> foo(n: <type>Float</type>) -> <type>Int</type> {
func foo(n: Float) -> Int {
    // CHECK: <kw>var</kw> fnComp : <type>Int</type>
    var fnComp : Int {
      // CHECK: <kw>get</kw> {
      get {
        // CHECK: <kw>var</kw> a: <type>Int</type>
        // CHECK: <kw>return</kw> <int>0</int>
        var a: Int
        return 0
      }
    }
    // CHECK: <kw>var</kw> q = {{(<type>)?}}MyCls{{(</type>)?}}()
    var q = MyCls()
    // CHECK: <kw>var</kw> ee = <str>"yoo"</str>;
    var ee = "yoo";
    // CHECK: <kw>return</kw> <int>100009</int>
    return 100009
}

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
protocol Prot {
  // CHECK: <kw>typealias</kw> Blarg
  typealias Blarg
  // CHECK: <kw>func</kw> protMeth(x: <type>Int</type>)
  func protMeth(x: Int)
  // CHECK: <kw>var</kw> protocolProperty1: <type>Int</type> { <kw>get</kw> }
  var protocolProperty1: Int { get }
  // CHECK: <kw>var</kw> protocolProperty2: <type>Int</type> { <kw>get</kw> <kw>set</kw> }
  var protocolProperty2: Int { get set }
}

// CHECK: <attr-builtin>infix</attr-builtin> <kw>operator</kw> *-* : FunnyPrecedence{{$}}
infix operator *-* : FunnyPrecedence

// CHECK: <kw>precedencegroup</kw> FunnyPrecedence
// CHECK-NEXT: <kw>associativity</kw>: left{{$}}
// CHECK-NEXT: <kw>higherThan</kw>: MultiplicationPrecedence
precedencegroup FunnyPrecedence {
  associativity: left
  higherThan: MultiplicationPrecedence
}

// CHECK: <kw>func</kw> *-*(l: <type>Int</type>, r: <type>Int</type>) -> <type>Int</type> { <kw>return</kw> l }{{$}}
func *-*(l: Int, r: Int) -> Int { return l }

// CHECK: <attr-builtin>infix</attr-builtin> <kw>operator</kw> *-+* : FunnyPrecedence
infix operator *-+* : FunnyPrecedence

// CHECK: <kw>func</kw> *-+*(l: <type>Int</type>, r: <type>Int</type>) -> <type>Int</type> { <kw>return</kw> l }{{$}}
func *-+*(l: Int, r: Int) -> Int { return l }

// CHECK: <attr-builtin>infix</attr-builtin> <kw>operator</kw> *--*{{$}}
infix operator *--*

// CHECK: <kw>func</kw> *--*(l: <type>Int</type>, r: <type>Int</type>) -> <type>Int</type> { <kw>return</kw> l }{{$}}
func *--*(l: Int, r: Int) -> Int { return l }

// CHECK: <kw>protocol</kw> Prot2 : <type>Prot</type> {}
protocol Prot2 : Prot {}

// CHECK: <kw>class</kw> SubCls : <type>MyCls</type>, <type>Prot</type> {}
class SubCls : MyCls, Prot {}

// CHECK: <kw>func</kw> genFn<T : <type>Prot</type> <kw>where</kw> <type>T</type>.<type>Blarg</type> : <type>Prot2</type>>(<kw>_</kw>: <type>T</type>) -> <type>Int</type> {}{{$}}
func genFn<T : Prot where T.Blarg : Prot2>(_: T) -> Int {}

func f(x: Int) -> Int {
  // CHECK: <comment-line>// string interpolation is the best</comment-line>
  // string interpolation is the best
  // CHECK: <str>"This is string </str>\<anchor>(</anchor>genFn({(a:<type>Int</type> -> <type>Int</type>) <kw>in</kw> a})<anchor>)</anchor><str> interpolation"</str>
  "This is string \(genFn({(a:Int -> Int) in a})) interpolation"

  // CHECK: <str>"This is unterminated</str>
  "This is unterminated

  // CHECK: <str>"This is unterminated with ignored \(interpolation) in it</str>
  "This is unterminated with ignored \(interpolation) in it

  // CHECK: <str>"This is terminated with invalid \(interpolation" + "in it"</str>
  "This is terminated with invalid \(interpolation" + "in it"

  // CHECK: <str>"""
  // CHECK-NEXT: This is a multiline string.
  // CHECK-NEXT: """</str>
  """
  This is a multiline string.
"""

  // CHECK: <str>"""
  // CHECK-NEXT: This is a multiline</str>\<anchor>(</anchor> <str>"interpolated"</str> <anchor>)</anchor><str>string
  // CHECK-NEXT: </str>\<anchor>(</anchor>
  // CHECK-NEXT: <str>"""
  // CHECK-NEXT: inner
  // CHECK-NEXT: """</str>
  // CHECK-NEXT: <anchor>)</anchor><str>
  // CHECK-NEXT: """</str>
  """
      This is a multiline\( "interpolated" )string
   \(
   """
    inner
   """
   )
   """

  // CHECK-OLD: <str>"</str>\<anchor>(</anchor><int>1</int><anchor>)</anchor>\<anchor>(</anchor><int>1</int><anchor>)</anchor><str>"</str>
  // CHECK-NEW: <str>"</str>\<anchor>(</anchor><int>1</int><anchor>)</anchor><str></str>\<anchor>(</anchor><int>1</int><anchor>)</anchor><str>"</str>
  "\(1)\(1)"
}

// CHECK: <kw>func</kw> bar(x: <type>Int</type>) -> (<type>Int</type>, <type>Float</type>) {
func bar(x: Int) -> (Int, Float) {
  // CHECK: foo({{(<type>)?}}Float{{(</type>)?}}())
  foo(Float())
}

class GenC<T1,T2> {}

func test() {
  // CHECK: {{(<type>)?}}GenC{{(</type>)?}}<<type>Int</type>, <type>Float</type>>()
  var x = GenC<Int, Float>()
}

// CHECK: <kw>typealias</kw> MyInt = <type>Int</type>
typealias MyInt = Int

func test2(x: Int) {
  // CHECK: <str>"</str>\<anchor>(</anchor>x<anchor>)</anchor><str>"</str>
  "\(x)"
}

// CHECK: <kw>class</kw> Observers {
class Observers {
  // CHECK: <kw>var</kw> p1 : <type>Int</type> {
  var p1 : Int {
    // CHECK: <kw>willSet</kw>(newValue) {}
    willSet(newValue) {}
    // CHECK: <kw>didSet</kw> {}
    didSet {}
  }
  // CHECK: <kw>var</kw> p2 : <type>Int</type> {
  var p2 : Int {
    // CHECK: <kw>didSet</kw> {}
    didSet {}
    // CHECK: <kw>willSet</kw> {}
    willSet {}
  }
}

// CHECK: <kw>func</kw> test3(o: <type>AnyObject</type>) {
func test3(o: AnyObject) {
  // CHECK: <kw>let</kw> x = o <kw>as</kw>! <type>MyCls</type>
  let x = o as! MyCls
}

// CHECK: <kw>func</kw> test4(<kw>inout</kw> a: <type>Int</type>) {{{$}}
func test4(inout a: Int) {
  // CHECK-OLD: <kw>if</kw> <kw>#available</kw> (<kw>OSX</kw> >= <float>10.10</float>, <kw>iOS</kw> >= <float>8.01</float>) {<kw>let</kw> OSX = <str>"iOS"</str>}}{{$}}
  // CHECK-NEW: <kw>if</kw> <kw>#available</kw> (OSX >= <float>10.10</float>, iOS >= <float>8.01</float>) {<kw>let</kw> OSX = <str>"iOS"</str>}}{{$}}
  if #available (OSX >= 10.10, iOS >= 8.01) {let OSX = "iOS"}}

// CHECK: <kw>func</kw> test4b(a: <kw>inout</kw> <type>Int</type>) {{{$}}
func test4b(a: inout Int) {
}

// CHECK: <kw>class</kw> MySubClass : <type>MyCls</type> {
class MySubClass : MyCls {
    // CHECK: <attr-builtin>override</attr-builtin> <kw>func</kw> foo(x: <type>Int</type>) {}
    override func foo(x: Int) {}

    // CHECK: <attr-builtin>convenience</attr-builtin> <kw>init</kw>(a: <type>Int</type>) {}
    convenience init(a: Int) {}
}

// CHECK: <kw>var</kw> g1 = { (x: <type>Int</type>) -> <type>Int</type> <kw>in</kw> <kw>return</kw> <int>0</int> }
var g1 = { (x: Int) -> Int in return 0 }

// CHECK: <attr-builtin>infix</attr-builtin> <kw>operator</kw> ~~ {
infix operator ~~ {}
// CHECK: <attr-builtin>prefix</attr-builtin> <kw>operator</kw> *~~ {
prefix operator *~~ {}
// CHECK: <attr-builtin>postfix</attr-builtin> <kw>operator</kw> ~~* {
postfix operator ~~* {}

func test_defer() {
  defer {
    // CHECK: <kw>let</kw> x : <type>Int</type> = <int>0</int>
    let x : Int = 0
  }
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

func test6<T : Prot>(x: T) {}
// CHECK: <kw>func</kw> test6<T : <type>Prot</type>>(x: <type>T</type>) {}{{$}}

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

// CHECK: <kw>func</kw> <placeholder><#test1#></placeholder> () {}
func <#test1#> () {}

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

func funcTakingFor(for internalName: Int) {}
// CHECK: <kw>func</kw> funcTakingFor(for internalName: <type>Int</type>) {}

func funcTakingIn(in internalName: Int) {}
// CHECK: <kw>func</kw> funcTakingIn(in internalName: <type>Int</type>) {}

_ = 123
// CHECK: <int>123</int>
_ = -123
// CHECK-OLD: <int>-123</int>
// CHECK-NEW: -<int>123</int>
_ = -1
// CHECK-OLD: <int>-1</int>
// CHECK-NEW: -<int>1</int>
_ = -0x123
// CHECK-OLD: <int>-0x123</int>
// CHECK-NEW: -<int>0x123</int>
_ = -3.1e-5
// CHECK-OLD: <float>-3.1e-5</float>
// CHECK-NEW: <float>3.1e-5</float>

/** aaa

 - returns: something
 */
// CHECK-OLD:  - <doc-comment-field>returns</doc-comment-field>: something
// CHECK-NEW:  - returns: something

let filename = #file
// CHECK: <kw>let</kw> filename = <kw>#file</kw>
let line = #line
// CHECK: <kw>let</kw> line = <kw>#line</kw>
let column = #column
// CHECK: <kw>let</kw> column = <kw>#column</kw>
let function = #function
// CHECK: <kw>let</kw> function = <kw>#function</kw>

let image = #imageLiteral(resourceName: "cloud.png")
// CHECK-OLD: <kw>let</kw> image = <object-literal>#imageLiteral(resourceName: "cloud.png")</object-literal>
// CHECK-NEW: <kw>let</kw> image = <object-literal>#imageLiteral</object-literal>(resourceName: <str>"cloud.png"</str>)
let file = #fileLiteral(resourceName: "cloud.png")
// CHECK-OLD: <kw>let</kw> file = <object-literal>#fileLiteral(resourceName: "cloud.png")</object-literal>
// CHECK-NEW: <kw>let</kw> file = <object-literal>#fileLiteral</object-literal>(resourceName: <str>"cloud.png"</str>)
let black = #colorLiteral(red: 0, green: 0, blue: 0, alpha: 1)
// CHECK-OLD: <kw>let</kw> black = <object-literal>#colorLiteral(red: 0, green: 0, blue: 0, alpha: 1)</object-literal>
// CHECK-NEW: <kw>let</kw> black = <object-literal>#colorLiteral</object-literal>(red: <int>0</int>, green: <int>0</int>, blue: <int>0</int>, alpha: <int>1</int>)

let rgb = [#colorLiteral(red: 1, green: 0, blue: 0, alpha: 1),
           #colorLiteral(red: 0, green: 1, blue: 0, alpha: 1),
           #colorLiteral(red: 0, green: 0, blue: 1, alpha: 1)]
// CHECK-OLD: <kw>let</kw> rgb = [<object-literal>#colorLiteral(red: 1, green: 0, blue: 0, alpha: 1)</object-literal>,
// CHECK-OLD:                     <object-literal>#colorLiteral(red: 0, green: 1, blue: 0, alpha: 1)</object-literal>,
// CHECK-OLD:                     <object-literal>#colorLiteral(red: 0, green: 0, blue: 1, alpha: 1)</object-literal>]
// CHECK-NEW: <kw>let</kw> rgb = [<object-literal>#colorLiteral</object-literal>(red: <int>1</int>, green: <int>0</int>, blue: <int>0</int>, alpha: <int>1</int>),
// CHECK-NEW:                     <object-literal>#colorLiteral</object-literal>(red: <int>0</int>, green: <int>1</int>, blue: <int>0</int>, alpha: <int>1</int>),
// CHECK-NEW:                     <object-literal>#colorLiteral</object-literal>(red: <int>0</int>, green: <int>0</int>, blue: <int>1</int>, alpha: <int>1</int>)]

"--\"\(x) --"
// CHECK: <str>"--\"</str>\<anchor>(</anchor>x<anchor>)</anchor><str> --"</str>

func keywordAsLabel1(in: Int) {}
// CHECK: <kw>func</kw> keywordAsLabel1(in: <type>Int</type>) {}
func keywordAsLabel2(for: Int) {}
// CHECK: <kw>func</kw> keywordAsLabel2(for: <type>Int</type>) {}
func keywordAsLabel3(if: Int, for: Int) {}
// CHECK: <kw>func</kw> keywordAsLabel3(if: <type>Int</type>, for: <type>Int</type>) {}
func keywordAsLabel4(_: Int) {}
// CHECK: <kw>func</kw> keywordAsLabel4(<kw>_</kw>: <type>Int</type>) {}
func keywordAsLabel5(_: Int, for: Int) {}
// CHECK: <kw>func</kw> keywordAsLabel5(<kw>_</kw>: <type>Int</type>, for: <type>Int</type>) {}
func keywordAsLabel6(if func: Int) {}
// CHECK: <kw>func</kw> keywordAsLabel6(if func: <type>Int</type>) {}

func foo1() {
// CHECK: <kw>func</kw> foo1() {
  keywordAsLabel1(in: 1)
// CHECK: keywordAsLabel1(in: <int>1</int>)
  keywordAsLabel2(for: 1)
// CHECK: keywordAsLabel2(for: <int>1</int>)
  keywordAsLabel3(if: 1, for: 2)
// CHECK: keywordAsLabel3(if: <int>1</int>, for: <int>2</int>)
  keywordAsLabel5(1, for: 2)
// CHECK: keywordAsLabel5(<int>1</int>, for: <int>2</int>)

  _ = (if: 0, for: 2)
// CHECK: <kw>_</kw> = (if: <int>0</int>, for: <int>2</int>)
  _ = (_: 0, _: 2)
// CHECK: <kw>_</kw> = (<kw>_</kw>: <int>0</int>, <kw>_</kw>: <int>2</int>)
}

func foo2(O1 : Int?, O2: Int?, O3: Int?) {
  guard let _ = O1, var _ = O2, let _ = O3 else { }
// CHECK:  <kw>guard</kw> <kw>let</kw> <kw>_</kw> = O1, <kw>var</kw> <kw>_</kw> = O2, <kw>let</kw> <kw>_</kw> = O3 <kw>else</kw> { }
  if let _ = O1, var _ = O2, let _ = O3 {}
// CHECK: <kw>if</kw> <kw>let</kw> <kw>_</kw> = O1, <kw>var</kw> <kw>_</kw> = O2, <kw>let</kw> <kw>_</kw> = O3 {}
}

func keywordInCaseAndLocalArgLabel(_ for: Int, for in: Int, class _: Int) {
// CHECK:  <kw>func</kw> keywordInCaseAndLocalArgLabel(<kw>_</kw> for: <type>Int</type>, for in: <type>Int</type>, class <kw>_</kw>: <type>Int</type>) {
  switch(`for`, `in`) {
  case (let x, let y):
// CHECK: <kw>case</kw> (<kw>let</kw> x, <kw>let</kw> y):
    print(x, y)
  }
}

#if os(macOS)
#endif
// CHECK: <#kw>#if</#kw> <#id>os</#id>(<#id>macOS</#id>)

// Keep this as the last test
/**
  Trailing off ...
func unterminatedBlockComment() {}
// CHECK: <comment-line>// Keep this as the last test</comment-line>
// CHECK: <doc-comment-block>/**
// CHECK:  Trailing off ...
// CHECK:  func unterminatedBlockComment() {}
// CHECK:  </doc-comment-block>
