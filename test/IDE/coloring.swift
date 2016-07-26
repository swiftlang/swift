// RUN: %target-swift-ide-test -syntax-coloring -source-filename %s | FileCheck %s
// RUN: %target-swift-ide-test -syntax-coloring -typecheck -source-filename %s | FileCheck %s
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
  // CHECK: <attr-builtin>indirect</attr-builtin> <kw>case</kw> Cons(T, List)
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

// CHECK: <attr-builtin>@IBOutlet</attr-builtin> <attr-id>@IBOutlet</attr-id> <kw>var</kw> {{(<attr-builtin>)?}}v1{{(</attr-builtin>)?}}: <type>String</type>
  @IBOutlet @IBOutlet var v1: String

// CHECK: <attr-builtin>@objc</attr-builtin> <attr-builtin>@IBOutlet</attr-builtin> <kw>var</kw> {{(<attr-builtin>)?}}v2{{(</attr-builtin>)?}}: <type>String</type>
  @objc @IBOutlet var v2: String

// CHECK: <attr-builtin>@IBOutlet</attr-builtin> <attr-builtin>@objc</attr-builtin> <kw>var</kw> {{(<attr-builtin>)?}}v3{{(</attr-builtin>)?}}: <type>String</type>
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
// CHECK: ///- <doc-comment-field>returns</doc-comment-field>: single-line, no space

/// - returns: single-line, 1 space
// CHECK: /// - <doc-comment-field>returns</doc-comment-field>: single-line, 1 space

///  - returns: single-line, 2 spaces
// CHECK: ///  - <doc-comment-field>returns</doc-comment-field>: single-line, 2 spaces

///       - returns: single-line, more spaces
// CHECK: ///       - <doc-comment-field>returns</doc-comment-field>: single-line, more spaces

// CHECK: <kw>protocol</kw> Prot {
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
  // CHECK: <kw>if</kw> <kw>#available</kw> (<kw>OSX</kw> >= <float>10.10</float>, <kw>iOS</kw> >= <float>8.01</float>) {<kw>let</kw> OSX = <str>"iOS"</str>}}{{$}}
  if #available (OSX >= 10.10, iOS >= 8.01) {let OSX = "iOS"}}

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

// CHECK: <comment-line>// <comment-marker>FIXME: blah.</comment-marker></comment-line>
// CHECK: <comment-line>//    <comment-marker>FIXME:   blah blah</comment-marker></comment-line>
// CHECK: <comment-line>// Something something, <comment-marker>FIXME: blah</comment-marker></comment-line>

/* FIXME: blah*/

// CHECK: <comment-block>/* <comment-marker>FIXME: blah*/</comment-marker></comment-block>

/*
 * FIXME: blah
 * Blah, blah.
 */

// CHECK: <comment-block>/*
// CHECK:  * <comment-marker>FIXME: blah</comment-marker>
// CHECK:  * Blah, blah.
// CHECK:  */</comment-block>

// TODO: blah.
// TTODO: blah.
// MARK: blah.

// CHECK: <comment-line>// <comment-marker>TODO: blah.</comment-marker></comment-line>
// CHECK: <comment-line>// T<comment-marker>TODO: blah.</comment-marker></comment-line>
// CHECK: <comment-line>// <comment-marker>MARK: blah.</comment-marker></comment-line>

// CHECK: <kw>func</kw> test5() -> <type>Int</type> {
func test5() -> Int {
  // CHECK: <comment-line>// <comment-marker>TODO: something, something.</comment-marker></comment-line>
  // TODO: something, something.
  // CHECK: <kw>return</kw> <int>0</int>
  return 0
}

func test6<T : Prot>(x: T) {}
// CHECK: <kw>func</kw> test6<T : <type>Prot</type>>(x: <type>T</type>) {}{{$}}

// http://whatever.com?ee=2&yy=1 and radar://123456
/* http://whatever.com FIXME: see in http://whatever.com/fixme
  http://whatever.com */

// CHECK: <comment-line>// <comment-url>http://whatever.com?ee=2&yy=1</comment-url> and <comment-url>radar://123456</comment-url></comment-line>
// CHECK: <comment-block>/* <comment-url>http://whatever.com</comment-url> <comment-marker>FIXME: see in <comment-url>http://whatever.com/fixme</comment-url></comment-marker>
// CHECK:  <comment-url>http://whatever.com</comment-url> */</comment-block>

// CHECK: <comment-line>// <comment-url>http://whatever.com/what-ever</comment-url></comment-line>
// http://whatever.com/what-ever

// CHECK: <kw>func</kw> <placeholder><#test1#></placeholder> () {}
func <#test1#> () {}

/// Brief.
///
/// Simple case.
///
/// - parameter x: A number
/// - parameter y: Another number
/// - returns: `x + y`
func foo(x: Int, y: Int) -> Int { return x + y }
// CHECK: <doc-comment-line>/// Brief.
// CHECK: </doc-comment-line><doc-comment-line>///
// CHECK: </doc-comment-line><doc-comment-line>/// Simple case.
// CHECK: </doc-comment-line><doc-comment-line>///
// CHECK: </doc-comment-line><doc-comment-line>/// - <doc-comment-field>parameter</doc-comment-field> x: A number
// CHECK: </doc-comment-line><doc-comment-line>/// - <doc-comment-field>parameter</doc-comment-field> y: Another number
// CHECK: </doc-comment-line><doc-comment-line>/// - <doc-comment-field>returns</doc-comment-field>: `x + y`
// CHECK: </doc-comment-line><kw>func</kw> foo(x: <type>Int</type>, y: <type>Int</type>) -> <type>Int</type> { <kw>return</kw> x + y }


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
// CHECK: <doc-comment-line>/// Brief.
// CHECK: </doc-comment-line><doc-comment-line>///
// CHECK: </doc-comment-line><doc-comment-line>/// Simple case.
// CHECK: </doc-comment-line><doc-comment-line>///
// CHECK: </doc-comment-line><doc-comment-line>/// - <doc-comment-field>Parameters</doc-comment-field>:
// CHECK: </doc-comment-line><doc-comment-line>/// - x: A number
// CHECK: </doc-comment-line><doc-comment-line>/// - y: Another number
// CHECK: </doc-comment-line><doc-comment-line>/// - <doc-comment-field>returns</doc-comment-field>: `x + y`
// CHECK: </doc-comment-line><kw>func</kw> bar(x: <type>Int</type>, y: <type>Int</type>) -> <type>Int</type> { <kw>return</kw> x + y }

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
// CHECK:   - <doc-comment-field>WARNING</doc-comment-field>: - WARNING: Should only have one field
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
// CHECK: <kw>func</kw> malformedBlockComment(f : () <kw>throws</kw> -> ()) <attr-builtin>rethrows</attr-builtin> {}

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
// CHECK: <doc-comment-line>/// [strict weak ordering](<comment-url>http://en.wikipedia.org/wiki/Strict_weak_order#Strict_weak_orderings</comment-url>

func funcTakingFor(for internalName: Int) {}
// CHECK: <kw>func</kw> funcTakingFor(for internalName: <type>Int</type>) {}

func funcTakingIn(in internalName: Int) {}
// CHECK: <kw>func</kw> funcTakingIn(in internalName: <type>Int</type>) {}

_ = 123
// CHECK: <int>123</int>
_ = -123
// CHECK: <int>-123</int>
_ = -1
// CHECK: <int>-1</int>
_ = -0x123
// CHECK: <int>-0x123</int>
_ = -3.1e-5
// CHECK: <float>-3.1e-5</float>

/** aaa

 - returns: something
 */
// CHECK:  - <doc-comment-field>returns</doc-comment-field>: something

let filename = #file
// CHECK: <kw>let</kw> filename = <kw>#file</kw>
let line = #line
// CHECK: <kw>let</kw> line = <kw>#line</kw>
let column = #column
// CHECK: <kw>let</kw> column = <kw>#column</kw>
let function = #function
// CHECK: <kw>let</kw> function = <kw>#function</kw>

let image = #imageLiteral(resourceName: "cloud.png")
// CHECK: <kw>let</kw> image = <object-literal>#imageLiteral(resourceName: "cloud.png")</object-literal>
let file = #fileLiteral(resourceName: "cloud.png")
// CHECK: <kw>let</kw> file = <object-literal>#fileLiteral(resourceName: "cloud.png")</object-literal>
let black = #colorLiteral(red: 0, green: 0, blue: 0, alpha: 1)
// CHECK: <kw>let</kw> black = <object-literal>#colorLiteral(red: 0, green: 0, blue: 0, alpha: 1)</object-literal>

"--\"\(x) --"
// CHECK: <str>"--\"</str>\<anchor>(</anchor>x<anchor>)</anchor><str> --"</str>

func keywordAsLabel1(in: Int) {}
// CHECK: <kw>func</kw> keywordAsLabel1(in: <type>Int</type>) {}
func keywordAsLabel2(for: Int) {}
// CHECK: <kw>func</kw> keywordAsLabel2(for: <type>Int</type>) {}

func foo1() {
// CHECK: <kw>func</kw> foo1() {
  keywordAsLabel1(in: 1)
// CHECK: keywordAsLabel1(in: <int>1</int>)
  keywordAsLabel2(for: 1)
// CHECK: keywordAsLabel2(for: <int>1</int>)
}

// Keep this as the last test
/**
  Trailing off ...
func unterminatedBlockComment() {}
// CHECK: <comment-line>// Keep this as the last test</comment-line>
// CHECK: <doc-comment-block>/**
// CHECK:  Trailing off ...
// CHECK:  func unterminatedBlockComment() {}
// CHECK:  </doc-comment-block>
