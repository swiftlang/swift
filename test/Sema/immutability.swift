// RUN: %target-parse-verify-swift

func markUsed<T>(t: T) {}

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
  
  takeClosure { (a : Int) -> Int in // expected-note {{mark parameter with 'var' to make it mutable}}
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
  // <rdar://problem/16927246> provide a fixit to change "let" to "var" if needing to mutate a variable
  let a = 42  // expected-note {{change 'let' to 'var' to make it mutable}}
  a = 17   // expected-error {{cannot assign to 'let' value 'a'}}

  let (b,c) = (4, "hello")   // expected-note {{change 'let' to 'var' to make it mutable}}
  markUsed(b); markUsed(c)
  b = 17   // expected-error {{cannot assign to 'let' value 'b'}}

  let d = (4, "hello")  // expected-note {{change 'let' to 'var' to make it mutable}}
  markUsed(d.0); markUsed(d.1)
  d.0 = 17   // expected-error {{cannot assign to '0': 'd' is immutable}}

  
  let e = 42  // expected-note {{change 'let' to 'var' to make it mutable}}
  ++e         // expected-error {{cannot pass immutable value to mutating operator: 'e' is a 'let' constant}}
  
  // <rdar://problem/16306600> QoI: passing a 'let' value as an inout results in an unfriendly diagnostic
  let f = 96 // expected-note {{change 'let' to 'var' to make it mutable}}
  var v = 1
  swap(&f, &v)  // expected-error {{cannot pass immutable value as inout argument: 'f' is a 'let' constant}}

  
  // <rdar://problem/19711233> QoI: poor diagnostic for operator-assignment involving immutable operand
  let g = 14 // expected-note {{change 'let' to 'var' to make it mutable}}
  g /= 2  // expected-error {{left side of mutating operator isn't mutable: 'g' is a 'let' constant}}
}

struct SomeStruct {
  var iv = 32
  
  static let type_let = 5  // expected-note {{change 'let' to 'var' to make it mutable}}

  mutating static func f() {  // expected-error {{static functions may not be declared mutating}}
  }
  
  mutating func g() {
    iv = 42
  }

  mutating func g2() {
    iv = 42
  }


  func h() {  // expected-note {{mark method 'mutating' to make 'self' mutable}}
    iv = 12      // expected-error {{cannot assign to 'iv': 'self' is immutable}}
  }

  var p: Int {
    // Getters default to non-mutating.
    get {          // expected-note {{mark accessor 'mutating' to make 'self' mutable}}
      iv = 37 // expected-error {{cannot assign to 'iv': 'self' is immutable}}
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
    set {      // expected-note {{mark accessor 'mutating' to make 'self' mutable}}
      iv = newValue // expected-error {{cannot assign to 'iv': 'self' is immutable}}
    }
  }

  var r : Int {
    get {        // expected-note {{mark accessor 'mutating' to make 'self' mutable}}
      iv = 37 // expected-error {{cannot assign to 'iv': 'self' is immutable}}
      return 42
    }
    mutating // Redundant but OK.
    set {
      iv = newValue
    }
  }

}

markUsed(SomeStruct.type_let)   // ok
SomeStruct.type_let = 17     // expected-error {{cannot assign to 'let' property 'type_let'}}

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


func test_arguments(a : Int,       // expected-note {{mark parameter with 'var' to make it mutable}} {{21-21=var}}
                    var b : Int,
                    let c : Int) {   // expected-note {{change 'let' parameter to 'var' to make it mutable}}  {{21-24=var}}
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
  markUsed(rvalue.nonmutating_property) // ok
  markUsed(rvalue.mutating_property)    // expected-error {{immutable value of type 'TestMutableStruct' only has mutating members named 'mutating_property'}}
  markUsed(rvalue.weird_property)       // expected-error {{immutable value of type 'TestMutableStruct' only has mutating members named 'weird_property'}}
  rvalue.nonmutating_property = 1    // ok
  rvalue.mutating_property = 1       // expected-error {{immutable value of type 'TestMutableStruct' only has mutating members named 'mutating_property'}}
  rvalue.weird_property = 1          // expected-error {{mmutable value of type 'TestMutableStruct' only has mutating members named 'weird_property'}}

 var lvalue = TestMutableStruct()
 markUsed(lvalue.mutating_property)    // ok
 markUsed(lvalue.nonmutating_property) // ok
 markUsed(lvalue.weird_property) // ok
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
  var _: Int = iis[17]
}

func testSelectorStyleArguments1(var x: Int, var bar y: Int) {
  ++x; ++y
}

func testSelectorStyleArguments2(let x: Int,  // expected-note {{change 'let' parameter to 'var' to make it mutable}}
                                 let bar y: Int) { // expected-note {{change 'let' parameter to 'var' to make it}}
  ++x  // expected-error {{cannot pass immutable value to mutating operator: 'x' is a 'let' constant}}
  ++y  // expected-error {{cannot pass immutable value to mutating operator: 'y' is a 'let' constant}}
}

func invalid_inout(inout var x : Int) { // expected-error {{parameter may not have multiple 'inout', 'var', or 'let' specifiers}}
}



func updateInt(inout x : Int) {}

// rdar://15785677 - allow 'let' declarations in structs/classes be initialized in init()
class LetClassMembers {
  let a : Int       // expected-note 2 {{change 'let' to 'var' to make it mutable}}
  let b : Int       // expected-note {{change 'let' to 'var' to make it mutable}}

  init(arg : Int) {
    a = arg             // ok, a is mutable in init()
    updateInt(&a)       // ok, a is mutable in init() and has been initialized
    b = 17              // ok, b is mutable in init()
  }

  func f() {
    a = 42  // expected-error {{cannot assign to 'let' property 'a'}}
    b = 42  // expected-error {{cannot assign to 'let' property 'b'}}
    updateInt(&a)   // expected-error {{cannot pass immutable value as inout argument: 'a' is a 'let' constant}}
  }
}
struct LetStructMembers {
  let a : Int       // expected-note 2 {{change 'let' to 'var' to make it mutable}}
  let b : Int       // expected-note {{change 'let' to 'var' to make it mutable}}

  init(arg : Int) {
    a = arg             // ok, a is mutable in init()
    updateInt(&a)       // ok, a is mutable in init() and has been initialized
    b = 17              // ok, b is mutable in init()
  }

  func f() {
    a = 42  // expected-error {{cannot assign to 'let' property 'a'}}
    b = 42  // expected-error {{cannot assign to 'let' property 'b'}}
    updateInt(&a)   // expected-error {{cannot pass immutable value as inout argument: 'a' is a 'let' constant}}
  }
}

func QoI() {
  let x = 97 // expected-note {{change 'let' to 'var' to make it mutable}}
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
  let x : Int       // expected-note {{change 'let' to 'var' to make it mutable}}
  init(newX: Int) {
    x = 42
  }
  
  convenience init(newY: Int) {
    self.init(newX: 19)
    x = 67  // expected-error {{cannot assign to 'let' property 'x'}}
  }
}

struct StructWithDelegatingInit {
  let x: Int       // expected-note {{change 'let' to 'var' to make it mutable}}
  
  init(x: Int) { self.x = x }
  init() { self.init(x: 0); self.x = 22 } // expected-error {{cannot assign to 'let' property 'x'}}
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

protocol SingleIntProperty {
  var i: Int { get }
}

struct SingleIntStruct : SingleIntProperty {
  let i: Int       // expected-note {{change 'let' to 'var' to make it mutable}}
}

extension SingleIntStruct {
  init(_ other: SingleIntStruct) {
    other.i = 999 // expected-error {{cannot assign to 'let' property 'i'}}
  }
}



// <rdar://problem/19370429> QoI: fixit to add "mutating" when assigning to a member of self in a struct
// <rdar://problem/17632908> QoI: Modifying struct member in non-mutating function produces difficult to understand error message
struct TestSubscriptMutability {
  let let_arr = [1,2,3]  // expected-note 2 {{change 'let' to 'var' to make it mutable}}
  var var_arr = [1,2,3]

  func nonmutating1() {
    let_arr[1] = 1  // expected-error {{cannot assign through subscript: 'let_arr' is a 'let' constant}}
  }
  func nonmutating2() { // expected-note {{mark method 'mutating' to make 'self' mutable}}
    var_arr[1] = 1  // expected-error {{cannot assign through subscript: 'self' is immutable}}
  }

  func nonmutating3() { // expected-note {{mark method 'mutating' to make 'self' mutable}}
    self = TestSubscriptMutability() // expected-error {{cannot assign to 'self' in a method}}
  }

  subscript(a : Int) -> TestSubscriptMutability {
    return TestSubscriptMutability()
  }

  func test() {
    self[1] = TestSubscriptMutability()  // expected-error {{cannot assign through subscript: subscript is get-only}}
    self[1].var_arr = [] // expected-error {{cannot assign to 'var_arr': base subscript is get-only}}
    self[1].let_arr = [] // expected-error {{cannot assign to 'let' property 'let_arr'}}
  }
}

func f(a : TestSubscriptMutability) { // expected-note {{mark parameter with 'var' to make it mutable}}
  a.var_arr = []  // expected-error {{cannot assign to 'var_arr': 'a' is immutable}}
}

struct TestSubscriptMutability2 {
  subscript(a : Int) -> Int {
    get { return 42 }
    set {}
  }

  func test() {  // expected-note {{mark method 'mutating' to make 'self' mutable}}
    self[1] = 2  // expected-error {{cannot assign through subscript: 'self' is immutable}}
  }
}

struct TestBangMutability {
  let let_opt = Optional(1)  // expected-note 2 {{change 'let' to 'var' to make it mutable}}
  var var_opt = Optional(1)

  func nonmutating1() {      // expected-note {{mark method 'mutating' to make 'self' mutable}}
    let_opt! = 1             // expected-error {{cannot assign through '!': 'let_opt' is a 'let' constant}}
    var_opt! = 1             // expected-error {{cannot assign through '!': 'self' is immutable}}
    self[]! = 2              // expected-error {{cannot assign through '!': subscript is get-only}}
  }
  mutating func nonmutating2() {
    let_opt! = 1             // expected-error {{cannot assign through '!': 'let_opt' is a 'let' constant}}
    var_opt! = 1             // ok

    self[]! = 2              // expected-error {{cannot assign through '!': subscript is get-only}}
  }

  subscript() -> Int? { return nil }

}
