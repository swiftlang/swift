// RUN: %target-typecheck-verify-swift -swift-version 4

////
// Members of structs
////

struct X {
  func f0(_ i: Int) -> X { }

  func f1(_ i: Int) { } // expected-note {{found this candidate}}

  mutating func f1(_ f: Float) { } // expected-note {{found this candidate}}

  func f2<T>(_ x: T) -> T { }
}

struct Y<T> {
  func f0(_: T) -> T {}
  func f1<U>(_ x: U, y: T) -> (T, U) {}
}

var i : Int
var x : X
var yf : Y<Float>

func g0(_: (inout X) -> (Float) -> ()) {}

_ = x.f0(i)
x.f0(i).f1(i)

// FIXME: Is this a bug in Swift 4 mode?
g0(X.f1)
// expected-error@-1 {{ambiguous reference to member 'f1'}}

_ = x.f0(x.f2(1))
_ = x.f0(1).f2(i)
_ = yf.f0(1)
_ = yf.f1(i, y: 1)

// Members referenced from inside the struct
struct Z {
  var i : Int
  func getI() -> Int { return i }
  mutating func incI() {}

  func curried(_ x: Int) -> (Int) -> Int { return { y in x + y } }

  subscript (k : Int) -> Int {
    get {
      return i + k
    }
    mutating
    set {
      i -= k
    }
  }
}

struct GZ<T> {
  var i : T
  func getI() -> T { return i }

  func f1<U>(_ a: T, b: U) -> (T, U) { 
    return (a, b)
  }
  
  func f2() {
    var f : Float
    var t = f1(i, b: f)
    f = t.1

    var zi = Z.i; // expected-error{{instance member 'i' cannot be used on type 'Z'}}
    var zj = Z.asdfasdf  // expected-error {{type 'Z' has no member 'asdfasdf'}}
  }
}

var z = Z(i: 0)
var getI = z.getI
var incI = z.incI // expected-error{{partial application of 'mutating'}}
var zi = z.getI()
var zcurried1 = z.curried
var zcurried2 = z.curried(0)
var zcurriedFull = z.curried(0)(1)

////
// Members of modules
////

// Module
Swift.print(3, terminator: "")

var format : String
_ = format._splitFirstIf({ $0.isASCII })

////
// Unqualified references
////

////
// Members of literals
////

// FIXME: Crappy diagnostic
"foo".lower() // expected-error{{value of type 'String' has no member 'lower'}}
var tmp = "foo".debugDescription

////
// Members of enums
////

enum W {
  case Omega

  func foo(_ x: Int) {}
  func curried(_ x: Int) -> (Int) -> () {}
}

var w = W.Omega
var foo = w.foo
var fooFull : () = w.foo(0)
var wcurried1 = w.curried
var wcurried2 = w.curried(0)
var wcurriedFull : () = w.curried(0)(1)

// Member of enum type
func enumMetatypeMember(_ opt: Int?) {
  opt.none // expected-error{{enum element 'none' cannot be referenced as an instance member}}
}

////
// Nested types
////

// Reference a Type member. <rdar://problem/15034920>
class G<T> {
  class In {
    class func foo() {}
  }
}

func goo() {
  G<Int>.In.foo()
}

////
// Misc ambiguities
////

// <rdar://problem/15537772>
struct DefaultArgs {
  static func f(_ a: Int = 0) -> DefaultArgs {
    return DefaultArgs()
  }
  init() {
    self = .f()
  }
}

class InstanceOrClassMethod {
  func method() -> Bool { return true }
  class func method(_ other: InstanceOrClassMethod) -> Bool { return false }
}

func testPreferClassMethodToCurriedInstanceMethod(_ obj: InstanceOrClassMethod) {
  let result = InstanceOrClassMethod.method(obj)
  let _: Bool = result // no-warning
  let _: () -> Bool = InstanceOrClassMethod.method(obj)
}

protocol Numeric {
  static func +(x: Self, y: Self) -> Self
}

func acceptBinaryFunc<T>(_ x: T, _ fn: (T, T) -> T) { }

func testNumeric<T : Numeric>(_ x: T) {
  acceptBinaryFunc(x, +)
}

/* FIXME: We can't check this directly, but it can happen with
multiple modules.

class PropertyOrMethod {
  func member() -> Int { return 0 }
  let member = false

  class func methodOnClass(_ obj: PropertyOrMethod) -> Int { return 0 }
  let methodOnClass = false
}

func testPreferPropertyToMethod(_ obj: PropertyOrMethod) {
  let result = obj.member
  let resultChecked: Bool = result
  let called = obj.member()
  let calledChecked: Int = called
  let curried = obj.member as () -> Int

  let methodOnClass = PropertyOrMethod.methodOnClass
  let methodOnClassChecked: (PropertyOrMethod) -> Int = methodOnClass
}
*/

struct Foo { var foo: Int }

protocol ExtendedWithMutatingMethods { }
extension ExtendedWithMutatingMethods {
  mutating func mutatingMethod() {}
  var mutableProperty: Foo {
    get { }
    set { }
  }
  var nonmutatingProperty: Foo {
    get { }
    nonmutating set { }
  }
  var mutatingGetProperty: Foo {
    mutating get { }
    set { }
  }
}

class ClassExtendedWithMutatingMethods: ExtendedWithMutatingMethods {}
class SubclassExtendedWithMutatingMethods: ClassExtendedWithMutatingMethods {}

func testClassExtendedWithMutatingMethods(_ c: ClassExtendedWithMutatingMethods, // expected-note* {{}}
                                     sub: SubclassExtendedWithMutatingMethods) { // expected-note* {{}}
  c.mutatingMethod() // expected-error{{cannot use mutating member on immutable value: 'c' is a 'let' constant}}
  c.mutableProperty = Foo(foo: 0) // expected-error{{cannot assign to property}}
  c.mutableProperty.foo = 0 // expected-error{{cannot assign to property}}
  c.nonmutatingProperty = Foo(foo: 0)
  c.nonmutatingProperty.foo = 0
  c.mutatingGetProperty = Foo(foo: 0) // expected-error{{cannot use mutating}}
  c.mutatingGetProperty.foo = 0 // expected-error{{cannot use mutating}}
  _ = c.mutableProperty
  _ = c.mutableProperty.foo
  _ = c.nonmutatingProperty
  _ = c.nonmutatingProperty.foo
  // FIXME: diagnostic nondeterministically says "member" or "getter"
  _ = c.mutatingGetProperty // expected-error{{cannot use mutating}}
  _ = c.mutatingGetProperty.foo // expected-error{{cannot use mutating}}

  sub.mutatingMethod() // expected-error{{cannot use mutating member on immutable value: 'sub' is a 'let' constant}}
  sub.mutableProperty = Foo(foo: 0) // expected-error{{cannot assign to property}}
  sub.mutableProperty.foo = 0 // expected-error{{cannot assign to property}}
  sub.nonmutatingProperty = Foo(foo: 0)
  sub.nonmutatingProperty.foo = 0
  sub.mutatingGetProperty = Foo(foo: 0) // expected-error{{cannot use mutating}}
  sub.mutatingGetProperty.foo = 0 // expected-error{{cannot use mutating}}
  _ = sub.mutableProperty
  _ = sub.mutableProperty.foo
  _ = sub.nonmutatingProperty
  _ = sub.nonmutatingProperty.foo
  _ = sub.mutatingGetProperty // expected-error{{cannot use mutating}}
  _ = sub.mutatingGetProperty.foo // expected-error{{cannot use mutating}}

  var mutableC = c
  mutableC.mutatingMethod()
  mutableC.mutableProperty = Foo(foo: 0)
  mutableC.mutableProperty.foo = 0
  mutableC.nonmutatingProperty = Foo(foo: 0)
  mutableC.nonmutatingProperty.foo = 0
  mutableC.mutatingGetProperty = Foo(foo: 0)
  mutableC.mutatingGetProperty.foo = 0
  _ = mutableC.mutableProperty
  _ = mutableC.mutableProperty.foo
  _ = mutableC.nonmutatingProperty
  _ = mutableC.nonmutatingProperty.foo
  _ = mutableC.mutatingGetProperty
  _ = mutableC.mutatingGetProperty.foo

  var mutableSub = sub
  mutableSub.mutatingMethod()
  mutableSub.mutableProperty = Foo(foo: 0)
  mutableSub.mutableProperty.foo = 0
  mutableSub.nonmutatingProperty = Foo(foo: 0)
  mutableSub.nonmutatingProperty.foo = 0
  _ = mutableSub.mutableProperty
  _ = mutableSub.mutableProperty.foo
  _ = mutableSub.nonmutatingProperty
  _ = mutableSub.nonmutatingProperty.foo
  _ = mutableSub.mutatingGetProperty
  _ = mutableSub.mutatingGetProperty.foo
}

// <rdar://problem/18879585> QoI: error message for attempted access to instance properties in static methods are bad.
enum LedModules: Int {
  case WS2811_1x_5V
}

extension LedModules {
  static var watts: Double {
    return [0.30][self.rawValue] // expected-error {{instance member 'rawValue' cannot be used on type 'LedModules'}}
  }
}


// <rdar://problem/15117741> QoI: calling a static function on an instance produces a non-helpful diagnostic
class r15117741S {
  static func g() {}
}
func test15117741(_ s: r15117741S) {
  s.g() // expected-error {{static member 'g' cannot be used on instance of type 'r15117741S'}}
}


// <rdar://problem/22491394> References to unavailable decls sometimes diagnosed as ambiguous
struct UnavailMember {
  @available(*, unavailable)
  static var XYZ : X { get {} } // expected-note {{'XYZ' has been explicitly marked unavailable here}}
}

let _ : [UnavailMember] = [.XYZ] // expected-error {{'XYZ' is unavailable}}
let _ : [UnavailMember] = [.ABC] // expected-error {{type 'UnavailMember' has no member 'ABC'}}


// <rdar://problem/22490787> QoI: Poor error message iterating over property with non-sequence type that defines an Iterator type alias
struct S22490787 {
  typealias Iterator = AnyIterator<Int>
}

func f22490787() {
  var path: S22490787 = S22490787()
  
  for p in path {  // expected-error {{type 'S22490787' does not conform to protocol 'Sequence'}}
  }
}

// <rdar://problem/23942743> [QoI] Bad diagnostic when errors inside enum constructor
enum r23942743 {
  case Tomato(cloud: String)
}
let _ = .Tomato(cloud: .none)  // expected-error {{reference to member 'Tomato' cannot be resolved without a contextual type}}



// SR-650: REGRESSION: Assertion failed: (baseTy && "Couldn't find appropriate context"), function getMemberSubstitutions
enum SomeErrorType {
  case StandaloneError
  case UnderlyingError(String)

  static func someErrorFromString(_ str: String) -> SomeErrorType? {
    if str == "standalone" { return .StandaloneError }
    if str == "underlying" { return .UnderlyingError }  // expected-error {{member 'UnderlyingError' expects argument of type 'String'}}
    return nil
  }
}

// SR-2193: QoI: better diagnostic when a decl exists, but is not a type

enum SR_2193_Error: Error {
  case Boom
}

do {
  throw SR_2193_Error.Boom
} catch let e as SR_2193_Error.Boom { // expected-error {{enum element 'Boom' is not a member type of 'SR_2193_Error'}}
}

// rdar://problem/25341015
extension Sequence {
  func r25341015_1() -> Int {
    return max(1, 2) // expected-error {{use of 'max' refers to instance method 'max(by:)' rather than global function 'max' in module 'Swift'}} expected-note {{use 'Swift.' to reference the global function in module 'Swift'}}
  }
}

class C_25341015 {
  static func baz(_ x: Int, _ y: Int) {} // expected-note {{'baz' declared here}}
  func baz() {}
  func qux() {
    baz(1, 2) // expected-error {{use of 'baz' refers to instance method 'baz()' rather than static method 'baz' in class 'C_25341015'}} expected-note {{use 'C_25341015.' to reference the static method}}
  }
}

struct S_25341015 {
  static func foo(_ x: Int, y: Int) {} // expected-note {{'foo(_:y:)' declared here}}

  func foo(z: Int) {}
  func bar() {
    foo(1, y: 2) // expected-error {{use of 'foo' refers to instance method 'foo(z:)' rather than static method 'foo(_:y:)' in struct 'S_25341015'}} expected-note {{use 'S_25341015.' to reference the static method}}
  }
}

func r25341015() {
  func baz(_ x: Int, _ y: Int) {}
  class Bar {
    func baz() {}
    func qux() {
      baz(1, 2) // expected-error {{argument passed to call that takes no arguments}}
    }
  }
}

func r25341015_local(x: Int, y: Int) {}
func r25341015_inner() {
  func r25341015_local() {}
  r25341015_local(x: 1, y: 2) // expected-error {{argument passed to call that takes no arguments}}
}
