// RUN: %target-swift-frontend -typecheck -verify %s
var global = 42

struct Gettable : DynamicMemberLookupProtocol {
  subscript(dynamicMember member: StaticString) -> Int {
    return 42
  }
}
struct Settable : DynamicMemberLookupProtocol {
  subscript(dynamicMember member: StaticString) -> Int {
    get {return 42}
    set {}
  }
}

struct MutGettable : DynamicMemberLookupProtocol {
  subscript(dynamicMember member: StaticString) -> Int {
    mutating get {
      return 42
    }
  }
}


struct NonMutSettable : DynamicMemberLookupProtocol {
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
  a.flavor = global  // expected-error {{cannot assign to property: 'a' is a 'let' constant}}
  
  global = b.flavor
  b.universal = global // expected-error {{cannot assign to property: 'b' is a 'let' constant}}
  
  var bm : Settable! = b
  
  global = bm.flavor
  bm.universal = global
}


struct FnTest : DynamicMemberLookupProtocol {
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
struct Invalid1 : DynamicMemberLookupProtocol {
  // expected-error @+1 {{type 'Invalid1' does not conform to 'DynamicMemberLookupProtocol'; subscript(dynamicMember:) should take a single string parameter}}
  subscript(dynamicMember member: Int) -> Int {
    return 42
  }
}

// Subscript may not be variadic.
struct Invalid2 : DynamicMemberLookupProtocol {
  // expected-error @+1 {{type 'Invalid2' does not conform to 'DynamicMemberLookupProtocol'; subscript(dynamicMember:) should take a single string parameter}}
  subscript(dynamicMember member: String...) -> Int {
    return 42
  }
}

// References to overloads are resolved just like normal subscript lookup:
// they are either contextually disambiguated or are invalid.
struct Ambiguity : DynamicMemberLookupProtocol {
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

//===----------------------------------------------------------------------===//
// Test Existential
//===----------------------------------------------------------------------===//

protocol PyVal : DynamicMemberLookupProtocol {
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
  a.x.y = b
  b.x.y = b
  return a.foo.bar.baz
}

//===----------------------------------------------------------------------===//
// JSON example
//===----------------------------------------------------------------------===//

enum JSON : DynamicMemberLookupProtocol {
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
// Retroactive conformance is invalid
//===----------------------------------------------------------------------===//

// expected-error @+1 {{retroactive conformance of 'Int' to 'DynamicMemberLookupProtocol' is not allowed, only primary type declarations may conform}}
extension Int : DynamicMemberLookupProtocol {
  subscript(dynamicMember member: String) -> Int {
    fatalError()
  }
}



