// RUN: %target-typecheck-verify-swift

struct X { } // expected-note * {{did you mean 'X'?}}

// Simple examples
struct X1 {
  var stored : Int

  subscript (i : Int) -> Int {
    get {
      return stored
    }
    mutating
    set {
      stored = newValue
    }
  }
}

struct X2 {
  var stored : Int

  subscript (i : Int) -> Int {
    get {
      return stored + i
    }
    set(v) {
      stored = v - i
    }
  }
}

struct X3 {
  var stored : Int

  subscript (_ : Int) -> Int {
    get {
      return stored
    }
    set(v) {
      stored = v
    }
  }
}

struct X4 {
  var stored : Int

  subscript (i : Int, j : Int) -> Int {
    get {
      return stored + i + j
    }
    set(v) {
      stored = v + i - j
    }
  }
}

// Semantic errors
struct Y1 {
  var x : X

  subscript(i: Int) -> Int {
    get {
      return x // expected-error{{cannot convert return expression of type 'X' to return type 'Int'}}
    }
    set {
      x = newValue // expected-error{{cannot assign value of type 'Int' to type 'X'}}
    }
  }
}

struct Y2 {
  subscript(idx: Int) -> TypoType { // expected-error 3{{use of undeclared type 'TypoType'}}
    get { repeat {} while true }
    set {}
  }
}

class Y3 {
  subscript(idx: Int) -> TypoType { // expected-error 3{{use of undeclared type 'TypoType'}}
    get { repeat {} while true }
    set {}
  }
}


protocol ProtocolGetSet0 {
  subscript(i: Int) -> Int {} // expected-error {{subscript declarations must have a getter}}
}
protocol ProtocolGetSet1 {
  subscript(i: Int) -> Int { get }
}
protocol ProtocolGetSet2 {
  subscript(i: Int) -> Int { set } // expected-error {{subscript declarations must have a getter}}
}
protocol ProtocolGetSet3 {
  subscript(i: Int) -> Int { get set }
}
protocol ProtocolGetSet4 {
  subscript(i: Int) -> Int { set get }
}

protocol ProtocolWillSetDidSet1 {
  subscript(i: Int) -> Int { willSet } // expected-error {{expected get or set in a protocol property}}
}
protocol ProtocolWillSetDidSet2 {
  subscript(i: Int) -> Int { didSet } // expected-error {{expected get or set in a protocol property}}
}
protocol ProtocolWillSetDidSet3 {
  subscript(i: Int) -> Int { willSet didSet } // expected-error {{expected get or set in a protocol property}}
}
protocol ProtocolWillSetDidSet4 {
  subscript(i: Int) -> Int { didSet willSet } // expected-error {{expected get or set in a protocol property}}
}

class DidSetInSubscript {
  subscript(_: Int) -> Int {
    didSet { // expected-error {{didSet is not allowed in subscripts}}
      print("eek")
    }
    get {}
  }
}

class WillSetInSubscript {
  subscript(_: Int) -> Int {
    willSet { // expected-error {{willSet is not allowed in subscripts}}
      print("eek")
    }
    get {}
  }
}

subscript(i: Int) -> Int { // expected-error{{'subscript' functions may only be declared within a type}}
  get {}
}

func f() {  // expected-note * {{did you mean 'f'?}}
  subscript (i: Int) -> Int { // expected-error{{'subscript' functions may only be declared within a type}}
    get {}
  }
}

struct NoSubscript { }

struct OverloadedSubscript {
  subscript(i: Int) -> Int {
    get {
      return i
    }
    set {}
  }

  subscript(i: Int, j: Int) -> Int {
    get { return i }
    set {}
  }
}

struct RetOverloadedSubscript {
  subscript(i: Int) -> Int {  // expected-note {{found this candidate}}
    get { return i }
    set {}
  }

  subscript(i: Int) -> Float {  // expected-note {{found this candidate}}
    get { return Float(i) }
    set {}
  }
}

struct MissingGetterSubscript1 {
  subscript (i : Int) -> Int {
  } // expected-error {{computed property must have accessors specified}}
}
struct MissingGetterSubscript2 {
  subscript (i : Int, j : Int) -> Int { // expected-error{{subscript declarations must have a getter}}
    set {}
  }
}

func test_subscript(_ x2: inout X2, i: Int, j: Int, value: inout Int, no: NoSubscript,
                    ovl: inout OverloadedSubscript, ret: inout RetOverloadedSubscript) {
  no[i] = value // expected-error{{type 'NoSubscript' has no subscript members}}

  value = x2[i]
  x2[i] = value

  value = ovl[i]
  ovl[i] = value

  value = ovl[(i, j)]
  ovl[(i, j)] = value

  value = ovl[(i, j, i)] // expected-error{{cannot convert value of type '(Int, Int, Int)' to expected argument type 'Int'}}

  ret[i] // expected-error{{ambiguous use of 'subscript'}}

  value = ret[i]
  ret[i] = value
}

func subscript_rvalue_materialize(_ i: inout Int) {
  i = X1(stored: 0)[i]
}

func subscript_coerce(_ fn: ([UnicodeScalar], [UnicodeScalar]) -> Bool) {}
func test_subscript_coerce() {
  subscript_coerce({ $0[$0.count-1] < $1[$1.count-1] })
}

struct no_index {
  subscript () -> Int { return 42 }
  func test() -> Int {
    return self[]
  }
}

struct tuple_index {
  subscript (x : Int, y : Int) -> (Int, Int) { return (x, y) }
  func test() -> (Int, Int) {
    return self[123, 456]
  }
}

struct MutableComputedGetter {
  var value: Int
  subscript(index: Int) -> Int {
    value = 5 // expected-error{{cannot assign to property: 'self' is immutable}}
    return 5
  }
  var getValue : Int {
    value = 5 // expected-error {{cannot assign to property: 'self' is immutable}}
    return 5
  }
}

struct MutableSubscriptInGetter {
  var value: Int
  subscript(index: Int) -> Int {
    get { // expected-note {{mark accessor 'mutating' to make 'self' mutable}}
      value = 5 // expected-error{{cannot assign to property: 'self' is immutable}}
      return 5
    }
  }
}

struct SubscriptTest1 {
  subscript(keyword:String) -> Bool { return true }  // expected-note 2 {{found this candidate}}
  subscript(keyword:String) -> String? {return nil }  // expected-note 2 {{found this candidate}}
}

func testSubscript1(_ s1 : SubscriptTest1) {
  let _ : Int = s1["hello"]  // expected-error {{ambiguous subscript with base type 'SubscriptTest1' and index type 'String'}}
  
  if s1["hello"] {}
  
  
  let _ = s1["hello"]  // expected-error {{ambiguous use of 'subscript'}}
}

struct SubscriptTest2 {
  subscript(a : String, b : Int) -> Int { return 0 }
  subscript(a : String, b : String) -> Int { return 0 }
}

func testSubscript1(_ s2 : SubscriptTest2) {
  _ = s2["foo"] // expected-error {{cannot subscript a value of type 'SubscriptTest2' with an index of type 'String'}}
  // expected-note @-1 {{overloads for 'subscript' exist with these partially matching parameter lists: (String, Int), (String, String)}}
  
  let a = s2["foo", 1.0] // expected-error {{cannot subscript a value of type 'SubscriptTest2' with an index of type '(String, Double)'}}
  // expected-note @-1 {{overloads for 'subscript' exist with these partially matching parameter lists: (String, Int), (String, String)}}
  
  
  let b = s2[1, "foo"] // expected-error {{cannot convert value of type 'Int' to expected argument type 'String'}}

  // rdar://problem/27449208
  let v: (Int?, [Int]?) = (nil [17]) // expected-error {{cannot subscript a nil literal value}}
}

// sr-114 & rdar://22007370

class Foo {
    subscript(key: String) -> String { // expected-note {{'subscript' previously declared here}}
        get { a } // expected-error {{use of unresolved identifier 'a'}}
        set { b } // expected-error {{use of unresolved identifier 'b'}}
    }
    
    subscript(key: String) -> String { // expected-error {{invalid redeclaration of 'subscript'}}
        get { a } // expected-error {{use of unresolved identifier 'a'}}
        set { b } // expected-error {{use of unresolved identifier 'b'}}
    }
}

// <rdar://problem/23952125> QoI: Subscript in protocol with missing {}, better diagnostic please
protocol r23952125 {
  associatedtype ItemType
  var count: Int { get }
  subscript(index: Int) -> ItemType  // expected-error {{subscript in protocol must have explicit { get } or { get set } specifier}} {{36-36= { get set \}}}
  
  var c : Int // expected-error {{property in protocol must have explicit { get } or { get set } specifier}}
}

// <rdar://problem/16812341> QoI: Poor error message when providing a default value for a subscript parameter
struct S4 {
  subscript(subs: Int = 0) -> Int {  // expected-error {{default arguments are not allowed in subscripts}}
    get {
      return 1
    }
  }
}
