// RUN: %target-parse-verify-swift

let bad_property_1: Int {    // expected-error {{'let' declarations cannot be computed properties}}
  get {
    return 42
  }
}
let bad_property_2: Int = 0 {
  get { // expected-error {{use of unresolved identifier 'get'}}
    return 42
  }
}

let no_initializer : Int



func foreach_variable() {
  for i in 0..<42 {
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
  class let type_let = 5 // expected-error {{class stored properties not yet supported in classes}}


  init() {
    self = FooClass()  // expected-error {{cannot assign to 'self' in a method}}
  }
  
  func bar() {
    self = FooClass()  // expected-error {{cannot assign to 'self' in a method}}
  }
  
  mutating init(a : Bool) {}     // expected-error {{'mutating' may only be used on 'func' declarations}}
  
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

  @mutating func mutating_attr() {}  // expected-error {{'mutating' is a declaration modifier, not an attribute}}
  @nonmutating func nonmutating_attr() {}  // expected-error {{'nonmutating' is a declaration modifier, not an attribute}}
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


protocol ClassBoundProtocolMutating : class {
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
  mutating mutating func f() {} // expected-error {{duplicate modifier}} expected-note {{modifier already specified here}}
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
  ++x  // expected-error {{cannot pass 'let' value 'x' to mutating unary operator '++'}}
  ++y  // expected-error {{cannot pass 'let' value 'y' to mutating unary operator '++'}}
}

func invalid_inout(inout var x : Int) { // expected-error {{parameter may not have multiple 'inout', 'var', or 'let' specifiers}}
}



func updateInt(inout x : Int) {}

// rdar://15785677 - allow 'let' declarations in structs/classes be initialized in init()
class LetClassMembers {
  let a : Int
  let b : Int

  init(arg : Int) {
    a = arg             // ok, a is mutable in init()
    updateInt(&a)       // ok, a is mutable in init() and has been initialized
    b = 17              // ok, b is mutable in init()
  }

  func f() {
    a = 42  // expected-error {{cannot assign to 'a' in 'self'}}
    b = 42  // expected-error {{cannot assign to 'b' in 'self'}}
    updateInt(&a)   // expected-error {{cannot assign to immutable value of type 'Int'}}
  }
}
struct LetStructMembers {
  let a : Int
  let b : Int

  init(arg : Int) {
    a = arg             // ok, a is mutable in init()
    updateInt(&a)       // ok, a is mutable in init() and has been initialized
    b = 17              // ok, b is mutable in init()
  }

  func f() {
    a = 42  // expected-error {{cannot assign to 'a' in 'self'}}
    b = 42  // expected-error {{cannot assign to 'b' in 'self'}}
    updateInt(&a)   // expected-error {{cannot assign to immutable value of type 'Int'}}
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



// <rdar://problem/17051675> Structure initializers in an extension cannot assign to constant properties
struct rdar17051675_S {
  let x : Int
  init(newX: Int) {
    x = 42
  }
}

extension rdar17051675_S {
  init(newY: Int) {
    x = 42
  }
}

struct rdar17051675_S2<T> {
  let x : Int
  init(newX: Int) {
    x = 42
  }
}

extension rdar17051675_S2 {
  init(newY: Int) {
    x = 42
  }
}


// <rdar://problem/17400366> let properties should not be mutable in convenience initializers
class ClassWithConvenienceInit {
  let x : Int
  init(newX: Int) {
    x = 42
  }
  
  convenience init(newY: Int) {
    self.init(newX: 19)
    x = 67  // expected-error {{cannot assign to 'x' in 'self'}}
  }
}

struct StructWithDelegatingInit {
  let x: Int
  
  init(x: Int) { self.x = x }
  init() { self.init(x: 0); self.x = 22 } // expected-error {{cannot assign to 'x' in 'self'}}
}



func test_recovery_missing_name_1(var: Int) {} // expected-error 2{{expected ',' separator}} expected-error 2{{expected parameter type following ':'}}

func test_recovery_missing_name_2(let: Int) {} // expected-error 2{{expected ',' separator}} expected-error 2{{expected parameter type following ':'}}


// <rdar://problem/16792027> compiler infinite loops on a really really mutating function
struct F {
  mutating mutating mutating f() { // expected-error 2 {{duplicate modifier}} expected-note 2 {{modifier already specified here}} expected-error {{consecutive declarations on a line must be separated by ';'}} expected-error 2 {{expected declaration}}
  }
  
  mutating nonmutating func g() {  // expected-error {{method may not be declared both mutating and nonmutating}}
  }
}

