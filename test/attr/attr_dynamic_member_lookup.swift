// RUN: %target-typecheck-verify-swift

var global = 42

@dynamicMemberLookup
struct Gettable {
  subscript(dynamicMember member: StaticString) -> Int {
    return 42
  }
}

@dynamicMemberLookup
struct Settable {
  subscript(dynamicMember member: StaticString) -> Int {
    get {return 42}
    set {}
  }
}

@dynamicMemberLookup
struct MutGettable {
  subscript(dynamicMember member: StaticString) -> Int {
    mutating get {
      return 42
    }
  }
}

@dynamicMemberLookup
struct NonMutSettable {
  subscript(dynamicMember member: StaticString) -> Int {
    get { return 42 }
    nonmutating set {}
  }
}

func test(a: Gettable, b: Settable, c: MutGettable, d: NonMutSettable) {
  global = a.wyverns
  a.flavor = global // expected-error {{cannot assign to property: 'a' is a 'let' constant}}

  global = b.flavor
  b.universal = global // expected-error {{cannot assign to property: 'b' is a 'let' constant}}
  b.thing += 1 // expected-error {{left side of mutating operator isn't mutable: 'b' is a 'let' constant}}

  var bm = b
  global = bm.flavor
  bm.universal = global
  bm.thing += 1

  var cm = c
  global = c.dragons // expected-error {{cannot use mutating getter on immutable value: 'c' is a 'let' constant}}
  global = c[dynamicMember: "dragons"] // expected-error {{cannot use mutating getter on immutable value: 'c' is a 'let' constant}}
  global = cm.dragons
  c.woof = global // expected-error {{cannot use mutating getter on immutable value: 'c' is a 'let' constant}}

  var dm = d
  global = d.dragons  // ok
  global = dm.dragons // ok
  d.woof = global     // ok
  dm.woof = global    // ok
}

func testIUO(a: Gettable!, b: Settable!) {
  global = a.wyverns
  a.flavor = global // expected-error {{cannot assign through dynamic lookup property: 'a' is a 'let' constant}}

  global = b.flavor
  b.universal = global // expected-error {{cannot assign through dynamic lookup property: 'b' is a 'let' constant}}

  var bm: Settable! = b

  global = bm.flavor
  bm.universal = global
}

//===----------------------------------------------------------------------===//
// Returning a function
//===----------------------------------------------------------------------===//

@dynamicMemberLookup
struct FnTest {
  subscript(dynamicMember member: StaticString) -> (Int) -> () {
    return { x in () }
  }
}
func testFunction(x: FnTest) {
  x.phunky(12)
}
func testFunctionIUO(x: FnTest!) {
  x.flavor(12)
}

//===----------------------------------------------------------------------===//
// Explicitly declared members take precedence
//===----------------------------------------------------------------------===//

@dynamicMemberLookup
struct Dog {
  public var name = "Kaylee"

  subscript(dynamicMember member: String) -> String {
    return "Zoey"
  }
}

func testDog(person: Dog) -> String {
  return person.name + person.otherName
}

//===----------------------------------------------------------------------===//
// Returning an IUO
//===----------------------------------------------------------------------===//

@dynamicMemberLookup
struct IUOResult {
  subscript(dynamicMember member: StaticString) -> Int! {
    get { return 42 }
    nonmutating set {}
  }
}

func testIUOResult(x: IUOResult) {
  x.foo?.negate() // Test mutating writeback.

  let _: Int = x.bar // Test implicitly forced optional
  let b = x.bar
  // expected-note@-1{{short-circuit}}
  // expected-note@-2{{coalesce}}
  // expected-note@-3{{force-unwrap}}

  let _: Int = b // expected-error {{value of optional type 'Int?' must be unwrapped to a value of type 'Int'}}
  // expected-note@-1{{coalesce}}
  // expected-note@-2{{force-unwrap}}
}

//===----------------------------------------------------------------------===//
// Error cases
//===----------------------------------------------------------------------===//

// Subscript index must be ExpressibleByStringLiteral.
@dynamicMemberLookup
struct Invalid1 {
  // expected-error @+1 {{@dynamicMemberLookup attribute requires 'Invalid1' to have a 'subscript(dynamicMember:)' method with an 'ExpressibleByStringLiteral' parameter}}
  subscript(dynamicMember member: Int) -> Int {
    return 42
  }
}

// Subscript may not be variadic.
@dynamicMemberLookup
struct Invalid2 {
  // expected-error @+1 {{@dynamicMemberLookup attribute requires 'Invalid2' to have a 'subscript(dynamicMember:)' method with an 'ExpressibleByStringLiteral' parameter}}
  subscript(dynamicMember member: String...) -> Int {
    return 42
  }
}

// References to overloads are resolved just like normal subscript lookup:
// they are either contextually disambiguated or are invalid.
@dynamicMemberLookup
struct Ambiguity {
  subscript(dynamicMember member: String) -> Int {
    return 42
  }
  subscript(dynamicMember member: String) -> Float {
    return 42
  }
}

func testAmbiguity(a: Ambiguity) {
  let _: Int = a.flexibility
  let _: Float = a.dynamism
  _ = a.dynamism // expected-error {{ambiguous use of 'subscript(dynamicMember:)'}}
}

// expected-error @+1 {{'@dynamicMemberLookup' attribute cannot be applied to this declaration}}
@dynamicMemberLookup
extension Int {
  subscript(dynamicMember member: String) -> Int {
    fatalError()
  }
}

// expected-error @+1 {{'@dynamicMemberLookup' attribute cannot be applied to this declaration}}
@dynamicMemberLookup
func NotAllowedOnFunc() {}

// @dynamicMemberLookup cannot be declared on a base class and fulfilled with a
// derived class.

// expected-error @+1 {{@dynamicMemberLookup attribute requires 'InvalidBase' to have a 'subscript(dynamicMember:)' method with an 'ExpressibleByStringLiteral' parameter}}
@dynamicMemberLookup
class InvalidBase {}

class InvalidDerived : InvalidBase { subscript(dynamicMember: String) -> Int { get {}} }

// expected-error @+1 {{value of type 'InvalidDerived' has no member 'dynamicallyLookedUp'}}
_ = InvalidDerived().dynamicallyLookedUp

//===----------------------------------------------------------------------===//
// Existentials
//===----------------------------------------------------------------------===//

@dynamicMemberLookup
protocol DynamicProtocol {
  subscript(dynamicMember member: StaticString) -> DynamicProtocol {
    get nonmutating set
  }
}

struct MyDynamicStruct : DynamicProtocol {
  subscript(dynamicMember member: StaticString) -> DynamicProtocol {
    get { fatalError() }
    nonmutating set {}
  }
}

func testMutableExistential(a: DynamicProtocol,
                            b: MyDynamicStruct) -> DynamicProtocol {
  a.x.y = b
  b.x.y = b
  return a.foo.bar.baz
}

// Verify protocol compositions and protocol refinements work.
protocol SubDynamicProtocol : DynamicProtocol {}
typealias ProtocolComp = AnyObject & DynamicProtocol

func testMutableExistential2(a: AnyObject & DynamicProtocol,
                             b: SubDynamicProtocol,
                             c: ProtocolComp & AnyObject) {
  a.x.y = b
  b.x.y = b
  c.x.y = b
}

@dynamicMemberLookup
protocol ProtoExt {}
extension ProtoExt {
  subscript(dynamicMember member: String) -> String {
    get {}
  }
}

extension String: ProtoExt {}

func testProtoExt() -> String {
  let str = "test"
  return str.sdfsdfsdf
}

//===----------------------------------------------------------------------===//
// JSON example
//===----------------------------------------------------------------------===//

@dynamicMemberLookup
enum JSON {
  case IntValue(Int)
  case StringValue(String)
  case ArrayValue(Array<JSON>)
  case DictionaryValue(Dictionary<String, JSON>)

  var stringValue: String? {
    if case .StringValue(let str) = self {
      return str
    }
    return nil
  }
  subscript(index: Int) -> JSON? {
    if case .ArrayValue(let arr) = self {
      return index < arr.count ? arr[index] : nil
    }
    return nil
  }
  subscript(key: String) -> JSON? {
    if case .DictionaryValue(let dict) = self {
      return dict[key]
    }
    return nil
  }

  subscript(dynamicMember member: String) -> JSON? {
    if case .DictionaryValue(let dict) = self {
      return dict[member]
    }
    return nil
  }
}
func testJsonExample(x: JSON) -> String? {
  _ = x.name?.first
  return x.name?.first?.stringValue
}

//===----------------------------------------------------------------------===//
// Class inheritance tests
//===----------------------------------------------------------------------===//

@dynamicMemberLookup
class BaseClass {
  subscript(dynamicMember member: String) -> Int {
    return 42
  }
}
class DerivedClass : BaseClass {}

func testDerivedClass(x: BaseClass, y: DerivedClass) -> Int {
  return x.life - y.the + x.universe - y.and + x.everything
}

// Test that derived classes can add a setter.
class DerivedClassWithSetter : BaseClass {
  override subscript(dynamicMember member: String) -> Int {
    get { return super[dynamicMember: member] }
    set {}
  }
}

func testOverrideSubscript(a: BaseClass, b: DerivedClassWithSetter) {
  let x = a.frotz + b.garbalaz
  b.baranozo = x
  a.balboza = 12 // expected-error {{cannot assign to property}}
}

//===----------------------------------------------------------------------===//
// Generics
//===----------------------------------------------------------------------===//

@dynamicMemberLookup
struct SettableGeneric1<T> {
  subscript(dynamicMember member: StaticString) -> T? {
    get {}
    nonmutating set {}
  }
}

func testGenericType<T>(a: SettableGeneric1<T>, b: T) -> T? {
  a.dfasdf = b
  return a.dfsdffff
}

func testConcreteGenericType(a: SettableGeneric1<Int>) -> Int? {
  a.dfasdf = 42
  return a.dfsdffff
}

@dynamicMemberLookup
struct SettableGeneric2<T> {
  subscript<U: ExpressibleByStringLiteral>(dynamicMember member: U) -> T {
    get {}
    nonmutating set {}
  }
}

func testGenericType2<T>(a: SettableGeneric2<T>, b: T) -> T? {
  a[dynamicMember: "fasdf"] = b
  a.dfasdf = b
  return a.dfsdffff
}

func testConcreteGenericType2(a: SettableGeneric2<Int>) -> Int? {
  a.dfasdf = 42
  return a.dfsdffff
}

// SR-8077 test case.
// `subscript(dynamicMember:)` works as a `@dynamicMemberLookup` protocol
// requirement.
@dynamicMemberLookup
protocol GenericProtocol {
  associatedtype S: ExpressibleByStringLiteral
  associatedtype T
  subscript(dynamicMember member: S) -> T { get }
}

@dynamicMemberLookup
class GenericClass<S: ExpressibleByStringLiteral, T> {
  let t: T
  init(_ t: T) { self.t = t }
  subscript(dynamicMember member: S) -> T { return t }
}

func testGenerics<S, T, P: GenericProtocol>(
  a: P,
  b: AnyObject & GenericClass<S, T>
) where P.S == S, P.T == T {
  let _: T = a.wew
  let _: T = b.lad
}

//===----------------------------------------------------------------------===//
// Keypaths
//===----------------------------------------------------------------------===//

@dynamicMemberLookup
class C {
  subscript(dynamicMember member: String) -> Int { return 7 }
}
_ = \C.[dynamicMember: "hi"]
_ = \C.testLookup
