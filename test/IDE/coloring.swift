// RUN: %target-swift-ide-test -syntax-coloring -source-filename %s | FileCheck %s
// RUN: %target-swift-ide-test -syntax-coloring -typecheck -source-filename %s | FileCheck %s

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

// CHECK: <attr-builtin>@noreturn</attr-builtin> <kw>func</kw> f0() {}
  @noreturn func f0() {}

// CHECK: <attr-builtin>@availability(*, unavailable)</attr-builtin> <kw>func</kw> f1() {}
  @availability(*, unavailable) func f1() {}

// CHECK: <attr-builtin>@availability(*, unavailable)</attr-builtin> <attr-builtin>@IBAction</attr-builtin> <kw>func</kw> f2() {}
  @availability(*, unavailable) @IBAction func f2() {}

// CHECK: <attr-builtin>@IBAction</attr-builtin> <attr-builtin>@availability(*, unavailable)</attr-builtin> <kw>func</kw> f3() {}
  @IBAction @availability(*, unavailable) func f3() {}

// CHECK: <attr-builtin>@IBAction</attr-builtin> <attr-builtin>@availability(*, unavailable)</attr-builtin> <attr-builtin>@noreturn</attr-builtin> <kw>func</kw> f4() {}
  @IBAction @availability(*, unavailable) @noreturn func f4() {}

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

// CHECK: <attr-builtin>infix</attr-builtin> <kw>operator</kw> *-* { <kw>associativity</kw> left <kw>precedence</kw> <int>140</int> }{{$}}
infix operator *-* { associativity left precedence 140 }

// CHECK: <kw>func</kw> *-*(l: <type>Int</type>, r: <type>Int</type>) -> <type>Int</type> { <kw>return</kw> l }{{$}}
func *-*(l: Int, r: Int) -> Int { return l }

// CHECK: <attr-builtin>infix</attr-builtin> <kw>operator</kw> *-+* { <kw>associativity</kw> left }{{$}}
infix operator *-+* { associativity left }

// CHECK: <kw>func</kw> *-+*(l: <type>Int</type>, r: <type>Int</type>) -> <type>Int</type> { <kw>return</kw> l }{{$}}
func *-+*(l: Int, r: Int) -> Int { return l }

// CHECK: <attr-builtin>infix</attr-builtin> <kw>operator</kw> *--* {}{{$}}
infix operator *--* {}

// CHECK: <kw>func</kw> *--*(l: <type>Int</type>, r: <type>Int</type>) -> <type>Int</type> { <kw>return</kw> l }{{$}}
func *--*(l: Int, r: Int) -> Int { return l }

// CHECK: <kw>protocol</kw> Prot2 : <type>Prot</type> {}
protocol Prot2 : Prot {}

// CHECK: <kw>class</kw> SubCls : <type>MyCls</type>, <type>Prot</type> {}
class SubCls : MyCls, Prot {}

// CHECK: <kw>func</kw> genFn<T : Prot <kw>where</kw> T.Blarg : Prot2>(<kw>_</kw>: <type>T</type>) -> <type>Int</type> {}
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
  // CHECK: <kw>if</kw> <#kw>#available</#kw> (<kw>OSX</kw> >= <float>10.10</float>, <kw>iOS</kw> >= <float>8.01</float>) {<kw>let</kw> OSX = <str>"iOS"</str>}}{{$}}
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
/// TTODO: blah.
// MARK: blah.

// CHECK: <comment-line>// <comment-marker>TODO: blah.</comment-marker></comment-line>
// CHECK: <comment-line>/// T<comment-marker>TODO: blah.</comment-marker></comment-line>
// CHECK: <comment-line>// <comment-marker>MARK: blah.</comment-marker></comment-line>

// CHECK: <kw>func</kw> test5() -> <type>Int</type> {
func test5() -> Int {
  // CHECK: <comment-line>// <comment-marker>TODO: something, something.</comment-marker></comment-line>
  // TODO: something, something.
  // CHECK: <kw>return</kw> <int>0</int>
  return 0
}

// http://whatever.com?ee=2&yy=1 and radar://123456
/* http://whatever.com FIXME: see in http://whatever.com/fixme
  http://whatever.com */

// CHECK: <comment-line>// <comment-url>http://whatever.com?ee=2&yy=1</comment-url> and <comment-url>radar://123456</comment-url></comment-line>
// CHECK: <comment-block>/* <comment-url>http://whatever.com</comment-url> <comment-marker>FIXME: see in <comment-url>http://whatever.com/fixme</comment-url></comment-marker>
// CHECK:  <comment-url>http://whatever.com</comment-url> */</comment-block>

// CHECK: <comment-line>// <comment-url>http://whatever.com/what-ever</comment-url></comment-line>
// http://whatever.com/what-ever
