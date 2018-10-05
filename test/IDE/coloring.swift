// RUN: %target-swift-ide-test -syntax-coloring -source-filename %s | %FileCheck %s
// RUN: %target-swift-ide-test -syntax-coloring -typecheck -source-filename %s | %FileCheck %s

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

// CHECK: <attr-builtin>@IBOutlet</attr-builtin> <attr-id>@IBOutlet</attr-id> <kw>var</kw> v1: <type>String</type>
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

  // CHECK: <str>"</str>\<anchor>(</anchor><int>1</int><anchor>)</anchor>\<anchor>(</anchor><int>1</int><anchor>)</anchor><str>"</str>
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
  // CHECK: <kw>_</kw> = o <kw>is</kw> <type>MyCls</type> ? o <kw>as</kw> <type>MyCls</type> : o <kw>as</kw>! <type>MyCls</type> <kw>as</kw> <type>MyCls</type> + <int>1</int>
  _ = o is MyCls ? o as MyCls : o as! MyCls as MyCls + 1
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

func test6<T : Prot>(x: T) {}
// CHECK: <kw>func</kw> test6<T : <type>Prot</type>>(x: <type>T</type>) {}{{$}}

// CHECK: <kw>func</kw> <placeholder><#test1#></placeholder> () {}
func <#test1#> () {}

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

// CHECK: <kw>class</kw> Ownership {
class Ownership {
  // CHECK: <attr-builtin>weak</attr-builtin> <kw>var</kw> w
  weak var w
  // CHECK: <attr-builtin>unowned</attr-builtin> <kw>var</kw> u
  unowned var u
  // CHECK: <attr-builtin>unowned(unsafe)</attr-builtin> <kw>var</kw> uu
  unowned(unsafe) var uu
}
// CHECK: <kw>let</kw> closure = { [<attr-builtin>weak</attr-builtin> x=bindtox, <attr-builtin>unowned</attr-builtin> y=bindtoy, <attr-builtin>unowned(unsafe)</attr-builtin> z=bindtoz] <kw>in</kw> }
let closure = { [weak x=bindtox, unowned y=bindtoy, unowned(unsafe) z=bindtoz] in }

protocol FakeClassRestrictedProtocol : `class` {}
// CHECK: <kw>protocol</kw> FakeClassRestrictedProtocol : <type>`class`</type> {}
// FIXME: rdar://42801404: OLD and NEW should be the same '<type>`class`</type>'.
