// RUN: %swift %s -verify

let bad_property_1: Int {    // expected-error {{'let' declarations cannot be a computed property}} expected-error {{'let' declarations require an initializer expression}}
  get {
    return 42
  }
}
let bad_property_2: Int = 0 {
  get { // expected-error {{use of unresolved identifier 'get'}}
    return 42
  }
}

let no_initializer : Int    // expected-error {{'let' declarations require an initializer expression}}



func foreach_variable() {
  for i in 0..42 {
    i = 11   // expected-error {{cannot assign to 'let' value 'i'}}
  }
}

func takeClosure(fn : (Int)->Int) {}

func passClosure() {
  takeClosure { a in
    a = 42        // expected-error {{cannot assign to 'let' value 'a'}}
    return a
  }
  
  takeClosure {
    $0 = 42     // expected-error{{cannot assign to 'let' value '$0'}}
    42
  }
  
  takeClosure { (a : Int) -> Int in
    a = 42     // expected-error{{cannot assign to 'let' value 'a'}}
    return 42
  }
}



class FooClass {
  class let type_let = 5  // TODO: expected-error {{class variables not yet supported}}


  init() {
    self = FooClass()  // expected-error {{cannot assign to 'self' in a method}}
  }
  
  func bar() {
    self = FooClass()  // expected-error {{cannot assign to 'self' in a method}}
  }
  
  mutating init(a : Bool) {}     // expected-error {{'mutating' isn't allowed on init methods}}
  
  mutating            // expected-error {{'mutating' isn't valid on methods in classes or class-bound protocols}}
  func baz() {}

  var x : Int {
    get {
      return 32
    }
    set(value) {
      value = 42         // expected-error {{cannot assign to 'let' value 'value'}}
    }
  }
  
  subscript(i : Int) -> Int {
    get {
      i = 42             // expected-error {{cannot assign to 'let' value 'i'}}
      return 1
    }
  }

}


func let_decls() {
  let a = 42
  a = 17   // expected-error {{cannot assign}}

  let (b,c) = (4, "hello")
  print(b); print(c)
  b = 17   // expected-error {{cannot assign}}

  let d = (4, "hello")
  print(d.0); print(d.1)
  d.0 = 17   // expected-error {{cannot assign}}

}

struct SomeStruct {
  var iv = 32
  
  static let type_let = 5

  mutating static func f() {  // expected-error {{static functions may not be declared mutating}}
  }
  
  mutating func g() {
    iv = 42
  }

  mutating func g2() {
    iv = 42
  }


  func h() {
    iv = 12      // expected-error {{cannot assign}}
  }

  var p: Int {
    // Getters default to non-mutating.
    get {
      iv = 37 // expected-error {{cannot assign to 'iv' in 'self'}}
      return 42
    }

    // Setters default to mutating.
    set {
      iv = newValue
    }
  }

  // Defaults can be changed.
  var q : Int {
    mutating
    get {
      iv = 37
      return 42
    }
    nonmutating
    set {
      iv = newValue // expected-error {{cannot assign to 'iv' in 'self'}}
    }
  }

  var r : Int {
    get {
      iv = 37 // expected-error {{cannot assign to 'iv' in 'self'}}
      return 42
    }
    mutating // Redundant but OK.
    set {
      iv = newValue
    }
  }

}

print(SomeStruct.type_let)   // ok
SomeStruct.type_let = 17     // expected-error {{cannot assign to the result of this expression}}

struct TestMutableStruct {
  mutating
  func f() {}

  func g() {}
  
  
  var mutating_property : Int {
    mutating
    get {}
    set {}
  }
  
  var nonmutating_property : Int {
    get {}
    nonmutating
    set {}
  }
  
  // This property has a mutating getter and !mutating setter.
  var weird_property : Int {
    mutating get {}
    nonmutating set {}
  }

  @mutating func mutating_attr() {}  // expected-error {{'mutating' is not an attribute, use the mutating keyword}}
  @!mutating func nonmutating_attr() {}  // expected-error {{'mutating' is not an attribute, use the nonmutating keyword}}
}

func test_mutability() {
  // Non-mutable method on rvalue is ok.
  TestMutableStruct().g()

  // Non-mutable method on let is ok.
  let x = TestMutableStruct()
  x.g()


  // Mutable methods on let and rvalue are not ok.
  x.f()                         // expected-error {{immutable value of type 'TestMutableStruct' only has mutating members named 'f'}}
  TestMutableStruct().f()       // expected-error {{immutable value of type 'TestMutableStruct' only has mutating members named 'f'}}
}


func test_arguments(a : Int, var b : Int, let c : Int) {
  a = 1  // expected-error {{cannot assign to 'let' value 'a'}}
  b = 2  // ok.
  c = 3  // expected-error {{cannot assign to 'let' value 'c'}}
}


@class_protocol
protocol ClassBoundProtocolMutating {
  mutating       // expected-error {{'mutating' isn't valid on methods in classes or class-bound protocols}}
  func f()
}

protocol MutatingTestProto {
  mutating
  func mutatingfunc()
  
  func nonmutatingfunc()  // expected-note {{protocol requires}}
}

class TestClass : MutatingTestProto {
  func mutatingfunc() {}  // Ok, doesn't need to be mutating.
  func nonmutatingfunc() {}
}

struct TestStruct1 : MutatingTestProto {
  func mutatingfunc() {}  // Ok, doesn't need to be mutating.
  func nonmutatingfunc() {}
}

struct TestStruct2 : MutatingTestProto {
  mutating
  func mutatingfunc() {}  // Ok, can be mutating.
  func nonmutatingfunc() {}
}

struct TestStruct3 : MutatingTestProto {   // expected-error {{type 'TestStruct3' does not conform to protocol 'MutatingTestProto'}}
  func mutatingfunc() {}

  // This is not ok, "nonmutatingfunc" doesn't allow mutating functions.
  mutating
  func nonmutatingfunc() {}          // expected-note {{candidate is marked 'mutating' but protocol does not allow it}}
}


// <rdar://problem/16722603> invalid conformance of mutating setter
protocol NonMutatingSubscriptable {
  subscript(i: Int) -> Int {get nonmutating set} // expected-note {{protocol requires subscript with type '(Int) -> Int'}}
}
struct MutatingSubscriptor : NonMutatingSubscriptable {  // expected-error {{type 'MutatingSubscriptor' does not conform to protocol 'NonMutatingSubscriptable'}}
  subscript(i: Int) -> Int {
    get { return 42 }
    mutating set {}   // expected-note {{candidate is marked 'mutating' but protocol does not allow it}}
  }
}

protocol NonMutatingGet {
  var a: Int { get } // expected-note {{protocol requires property 'a' with type 'Int'}}
}
struct MutatingGet : NonMutatingGet { // expected-error {{type 'MutatingGet' does not conform to protocol 'NonMutatingGet'}}
  var a: Int { mutating get { return 0 } } // expected-note {{candidate is marked 'mutating' but protocol does not allow it}}
}

func test_properties() {
  let rvalue = TestMutableStruct()
  print(rvalue.nonmutating_property) // ok
  print(rvalue.mutating_property)    // expected-error {{immutable value of type 'TestMutableStruct' only has mutating members named 'mutating_property'}}
  print(rvalue.weird_property)       // expected-error {{immutable value of type 'TestMutableStruct' only has mutating members named 'weird_property'}}
  rvalue.nonmutating_property = 1    // ok
  rvalue.mutating_property = 1       // expected-error {{immutable value of type 'TestMutableStruct' only has mutating members named 'mutating_property'}}
  rvalue.weird_property = 1          // expected-error {{mmutable value of type 'TestMutableStruct' only has mutating members named 'weird_property'}}

 var lvalue = TestMutableStruct()
 print(lvalue.mutating_property)    // ok
 print(lvalue.nonmutating_property) // ok
 print(lvalue.weird_property) // ok
 lvalue.mutating_property = 1       // ok
 lvalue.nonmutating_property = 1    // ok
 lvalue.weird_property = 1    // ok
}

struct DuplicateMutating {
  mutating mutating func f() {} // expected-error {{'mutating' specified twice}}{{12-20=}}
}

protocol SubscriptNoGetter {
  subscript (i: Int) -> Int { get }
}

func testSubscriptNoGetter(let iis: SubscriptNoGetter) {
  var i: Int = iis[17]
}

func testSelectorStyleArguments1(var x: Int, var bar y: Int) {
  ++x; ++y
}

func testSelectorStyleArguments2(let x: Int, let bar y: Int) {
  ++x  // expected-error {{could not find an overload for '++' that accepts the supplied arguments}}
  ++y  // expected-error {{could not find an overload for '++' that accepts the supplied arguments}}
}

func invalid_inout(inout var x : Int) { // expected-error {{'inout' arguments may not also be marked 'var' or 'let'}}
}



func updateInt(inout x : Int) {}

// rdar://15785677 - allow 'let' declarations in structs/classes be initialized in init()
class LetClassMembers {
  let a : Int
  let b = 42

  init(arg : Int) {
    a = arg             // ok, a is mutable in init()
    updateInt(&a)       // ok, a is mutable in init() and has been initialized
    b = 17              // ok, b is mutable in init()
  }

  func f() {
    a = 42  // expected-error {{cannot assign to 'a' in 'self'}}
    b = 42  // expected-error {{cannot assign to 'b' in 'self'}}
    updateInt(&a)   // expected-error {{'Int' is not a subtype of '@lvalue $T3'}}
  }
}
struct LetStructMembers {
  let a : Int
  let b = 42

  init(arg : Int) {
    a = arg             // ok, a is mutable in init()
    updateInt(&a)       // ok, a is mutable in init() and has been initialized
    b = 17              // ok, b is mutable in init()
  }

  func f() {
    a = 42  // expected-error {{cannot assign to 'a' in 'self'}}
    b = 42  // expected-error {{cannot assign to 'b' in 'self'}}
    updateInt(&a)   // expected-error {{'Int' is not a subtype of '@lvalue $T3'}}
  }
}

func QoI() {
  let x = 97
  x = 17         // expected-error {{cannot assign to 'let' value 'x'}}

  var get_only: Int {
    get { return 7 }
  }
  get_only = 92            // expected-error {{cannot assign to a get-only property 'get_only'}}
}
