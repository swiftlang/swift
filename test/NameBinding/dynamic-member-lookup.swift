// RUN: %target-swift-frontend -typecheck -verify %s
var global = 42

func testXXXXX(a : PyVal, b : MyType) -> PyVal {
  a[dynamicMember: "x"] = b
  a.x = b
}

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

func test_function(b: Settable) {
  var bm = b
  bm.flavor = global
}

func test(a: Gettable, b: Settable, c: MutGettable, d: NonMutSettable) {
  global = a.wyverns
  a.flavor = global    // expected-error {{cannot assign to property: 'a' is a 'let' constant}}
  
  global = b.flavor
  b.universal = global // expected-error {{cannot assign to property: 'b' is a 'let' constant}}
  b.thing += 1         // expected-error {{left side of mutating operator isn't mutable: 'b' is a 'let' constant}}

  var bm = b
  global = bm.flavor
  bm.universal = global
  bm.thing += 1
  
  var cm = c
  global = c.dragons  // expected-error {{cannot use mutating getter on immutable value: 'c' is a 'let' constant}}
  global = c[dynamicMember: "dragons"] // expected-error {{cannot use mutating getter on immutable value: 'c' is a 'let' constant}}
  global = cm.dragons
  c.woof = global // expected-error {{cannot use mutating getter on immutable value: 'c' is a 'let' constant}}
  
  var dm = d
  global = d.dragons  // ok
  global = dm.dragons // ok
  d.woof = global     // ok
  dm.woof = global    // ok
}


func test_iuo(a : Gettable!, b : Settable!) {
  global = a.wyverns
  a.flavor = global  // expected-error {{cannot assign through dynamic lookup property: subscript is get-only}}
  
  global = b.flavor
  b.universal = global // expected-error {{cannot assign through dynamic lookup property: 'b' is a 'let' constant}}
  
  var bm : Settable! = b
  
  global = bm.flavor
  bm.universal = global
}

@dynamicMemberLookup
struct FnTest {
  subscript(dynamicMember member: StaticString) -> (_ a : Int)->() {
    return { a in () }
  }
}
func test_function(x : FnTest) {
  x.phunky(12)
}
func test_function_iuo(x : FnTest!) {
  x.flavor(12)
}


//===----------------------------------------------------------------------===//
// Error cases
//===----------------------------------------------------------------------===//

// Subscript index must be ExpressibleByStringLiteral.
@dynamicMemberLookup
struct Invalid1 {
  // expected-error @+1 {{@dynamicMemberLookup attribute requires 'Invalid1' to have a 'subscript(dynamicMember:)' member with a string index}}
  subscript(dynamicMember member: Int) -> Int {
    return 42
  }
}

// Subscript may not be variadic.
@dynamicMemberLookup
struct Invalid2 {
  // expected-error @+1 {{@dynamicMemberLookup attribute requires 'Invalid2' to have a 'subscript(dynamicMember:)' member with a string index}}
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

func testAmbiguity(a : Ambiguity) {
  let _ : Int = a.flexibility
  let _ : Float = a.dynamism
  _ = a.dynamism  // expected-error {{ambiguous use of 'subscript(dynamicMember:)'}}
}


// expected-error @+1 {{'@dynamicMemberLookup' attribute cannot be applied to this declaration}}
@dynamicMemberLookup
extension Int {
  subscript(dynamicMember member: String) -> Int {
    fatalError()
  }
}

@dynamicMemberLookup  // expected-error {{'@dynamicMemberLookup' attribute cannot be applied to this declaration}}
func NotAllowedOnFunc() {
}

//===----------------------------------------------------------------------===//
// Test Existential
//===----------------------------------------------------------------------===//

@dynamicMemberLookup
protocol PyVal {
  subscript(dynamicMember member: StaticString) -> PyVal { get nonmutating set }
}
extension PyVal {
  subscript(dynamicMember member: StaticString) -> PyVal {
    get { fatalError() } nonmutating set {}
  }
}

struct MyType : PyVal {
}


func testMutableExistential(a : PyVal, b : MyType) -> PyVal {
  a.x = b
//  a.x.y = b
//  b.x.y = b
//  return a.foo.bar.baz
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

  var stringValue : String? {
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
func test_json_example(x : JSON) -> String? {
  _ = x.name?.first
  return x.name?.first?.stringValue
}

//===----------------------------------------------------------------------===//
// Derived Class Example
//===----------------------------------------------------------------------===//

@dynamicMemberLookup
class BaseClass {
  subscript(dynamicMember member: String) -> Int {
    return 42
  }
}
class DerivedClass : BaseClass {
}

func testDerivedClass(x : BaseClass, y : DerivedClass) -> Int {
  return x.life - y.the + x.universe - y.and + x.everything
}


