// RUN: %target-typecheck-verify-swift

func markUsed<T>(_ t: T) {}

prefix operator ++
public prefix func ++ <T>(rhs: inout T) -> T { fatalError() }

let bad_property_1: Int {    // expected-error {{'let' declarations cannot be computed properties}} {{1-4=var}}
  get {
    return 42
  }
}
let bad_property_2: Int = 0 { // expected-error {{'let' declarations cannot be computed properties}} {{1-4=var}} expected-error {{variable with getter/setter cannot have an initial value}}
  get {
    return 42
  }
}

let no_initializer : Int



func foreach_variable() {
  for i in 0..<42 {
    i = 11   // expected-error {{cannot assign to value: 'i' is a 'let' constant}}
  }
}

func takeClosure(_ fn : (Int) -> Int) {}

func passClosure() {
  takeClosure { a in
    a = 42        // expected-error {{cannot assign to value: 'a' is a 'let' constant}}
    return a
  }
  
  takeClosure {
    $0 = 42     // expected-error{{cannot assign to value: '$0' is immutable}}
    return 42
  }
  
  takeClosure { (a : Int) -> Int in
    a = 42     // expected-error{{cannot assign to value: 'a' is a 'let' constant}}
    return 42
  }
}



class FooClass {
  class let type_let = 5 // expected-error {{class stored properties not supported in classes}}


  init() {
    self = FooClass()  // expected-error {{cannot assign to value: 'self' is immutable}}
  }
  
  func bar() {
    self = FooClass()  // expected-error {{cannot assign to value: 'self' is immutable}}
  }
  
  mutating init(a : Bool) {}     // expected-error {{'mutating' may only be used on 'func' declarations}} {{3-12=}}
  
  mutating            // expected-error {{'mutating' isn't valid on methods in classes or class-bound protocols}} {{3-12=}}
  func baz() {}

  nonmutating         // expected-error {{'nonmutating' isn't valid on methods in classes or class-bound protocols}} {{3-15=}}
  func bay() {}

  mutating nonmutating // expected-error {{'mutating' isn't valid on methods in classes or class-bound protocols}} expected-error {{'nonmutating' isn't valid on methods in classes or class-bound protocols}}
  func bax() {}

  var x : Int {
    get {
      return 32
    }
    set(value) {
      value = 42         // expected-error {{cannot assign to value: 'value' is a 'let' constant}}
    }
  }
  
  subscript(i : Int) -> Int {
    get {
      i = 42             // expected-error {{cannot assign to value: 'i' is immutable}}
      return 1
    }
  }

}


func let_decls() {
  // <rdar://problem/16927246> provide a fixit to change "let" to "var" if needing to mutate a variable
  let a = 42  // expected-note {{change 'let' to 'var' to make it mutable}} {{3-6=var}}
  a = 17   // expected-error {{cannot assign to value: 'a' is a 'let' constant}}

  let (b,c) = (4, "hello")   // expected-note {{change 'let' to 'var' to make it mutable}} {{3-6=var}}
  markUsed(b); markUsed(c)
  b = 17   // expected-error {{cannot assign to value: 'b' is a 'let' constant}}

  let d = (4, "hello")  // expected-note {{change 'let' to 'var' to make it mutable}} {{3-6=var}}
  markUsed(d.0); markUsed(d.1)
  d.0 = 17   // expected-error {{cannot assign to property: 'd' is a 'let' constant}}

  
  let e = 42  // expected-note {{change 'let' to 'var' to make it mutable}} {{3-6=var}}
  ++e         // expected-error {{cannot pass immutable value to mutating operator: 'e' is a 'let' constant}}
  
  // <rdar://problem/16306600> QoI: passing a 'let' value as an inout results in an unfriendly diagnostic
  let f = 96 // expected-note {{change 'let' to 'var' to make it mutable}} {{3-6=var}}
  var v = 1
  swap(&f, &v)  // expected-error {{cannot pass immutable value as inout argument: 'f' is a 'let' constant}}

  
  // <rdar://problem/19711233> QoI: poor diagnostic for operator-assignment involving immutable operand
  let g = 14 // expected-note {{change 'let' to 'var' to make it mutable}} {{3-6=var}}
  g /= 2  // expected-error {{left side of mutating operator isn't mutable: 'g' is a 'let' constant}}
}

struct SomeStruct {
  var iv = 32
  
  static let type_let = 5  // expected-note {{change 'let' to 'var' to make it mutable}} {{10-13=var}}

  mutating static func f() {  // expected-error {{static functions must not be declared mutating}} {{3-12=}}
  }
  
  mutating func g() {
    iv = 42
  }

  mutating func g2() {
    iv = 42
  }


  func h() {  // expected-note {{mark method 'mutating' to make 'self' mutable}} {{3-3=mutating }}
    iv = 12      // expected-error {{cannot assign to property: 'self' is immutable}}
  }

  var p: Int {
    // Getters default to non-mutating.
    get {          // expected-note {{mark accessor 'mutating' to make 'self' mutable}} {{5-5=mutating }}
      iv = 37 // expected-error {{cannot assign to property: 'self' is immutable}}
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
    set {      // expected-note {{mark accessor 'mutating' to make 'self' mutable}} {{5-5=mutating }}
      iv = newValue // expected-error {{cannot assign to property: 'self' is immutable}}
    }
  }

  var r : Int {
    get {        // expected-note {{mark accessor 'mutating' to make 'self' mutable}} {{5-5=mutating }}
      iv = 37 // expected-error {{cannot assign to property: 'self' is immutable}}
      return 42
    }
    mutating // Redundant but OK.
    set {
      iv = newValue
    }
  }

}

markUsed(SomeStruct.type_let)   // ok
SomeStruct.type_let = 17     // expected-error {{cannot assign to property: 'type_let' is a 'let' constant}}

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

  @mutating func mutating_attr() {}  // expected-error {{'mutating' is a declaration modifier, not an attribute}} {{3-4=}}
  @nonmutating func nonmutating_attr() {}  // expected-error {{'nonmutating' is a declaration modifier, not an attribute}} {{3-4=}}
  @__consuming func consuming_attr() {}  // expected-error {{'__consuming' is a declaration modifier, not an attribute}} {{3-4=}}
}

func test_mutability() {
  // Non-mutable method on rvalue is ok.
  TestMutableStruct().g()

  // Non-mutable method on let is ok.
  let x = TestMutableStruct() // expected-note {{change 'let' to 'var' to make it mutable}} {{3-6=var}}
  x.g()


  // Mutable methods on let and rvalue are not ok.
  x.f()                         // expected-error {{cannot use mutating member on immutable value: 'x' is a 'let' constant}}
  TestMutableStruct().f()       // expected-error {{cannot use mutating member on immutable value: function call returns immutable value}}
  
  _ = TestMutableStruct().weird_property  // expected-error {{cannot use mutating getter on immutable value: function call returns immutable value}}

  let tms = TestMutableStruct() // expected-note {{change 'let' to 'var' to make it mutable}} {{3-6=var}}
  _ = tms.weird_property        // expected-error {{cannot use mutating getter on immutable value: 'tms' is a 'let' constant}}
}


func test_arguments(_ a : Int,
                    b : Int,
                    let c : Int) { // expected-error {{'let' as a parameter attribute is not allowed}} {{21-25=}}
  var b = b
  a = 1  // expected-error {{cannot assign to value: 'a' is a 'let' constant}}
  b = 2  // ok.
  _ = b
}


protocol ClassBoundProtocolMutating : class {
  mutating       // expected-error {{'mutating' isn't valid on methods in classes or class-bound protocols}} {{3-12=}}
  func f()
}

protocol MutatingTestProto {
  mutating
  func mutatingfunc()
  
  func nonmutatingfunc() // expected-note {{protocol requires}}
  __consuming
  func consuming_nonmutating_func() // expected-note {{protocol requires}}
}

class TestClass : MutatingTestProto {
  func mutatingfunc() {}  // Ok, doesn't need to be mutating.
  func nonmutatingfunc() {}
  __consuming // OK, but doesn't make much sense.
  func consuming_nonmutating_func() {}
}

struct TestStruct1 : MutatingTestProto {
  func mutatingfunc() {}  // Ok, doesn't need to be mutating.
  func nonmutatingfunc() {}
  __consuming func consuming_nonmutating_func() {}
}

struct TestStruct2 : MutatingTestProto {
  mutating
  func mutatingfunc() {}  // Ok, can be mutating.
  func nonmutatingfunc() {}
  __consuming func consuming_nonmutating_func() {}
}

struct TestStruct3 : MutatingTestProto {   // expected-error {{type 'TestStruct3' does not conform to protocol 'MutatingTestProto'}}
  func mutatingfunc() {}

  // This is not ok, "nonmutatingfunc" doesn't allow mutating functions.
  mutating
  func nonmutatingfunc() {} // expected-note {{candidate is marked 'mutating' but protocol does not allow it}}
  mutating
  func consuming_nonmutating_func() {} // expected-note {{candidate is marked 'mutating' but protocol does not allow it}}
}

struct TestStruct4 : MutatingTestProto {
  mutating
  func mutatingfunc() {}  // Ok, can be mutating.
  func nonmutatingfunc() {}
  nonmutating func consuming_nonmutating_func() {}
}

struct TestStruct5 : MutatingTestProto {
  mutating
  func mutatingfunc() {}  // Ok, can be mutating.
  __consuming
  func nonmutatingfunc() {}
  __consuming func consuming_nonmutating_func() {}
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
  let rvalue = TestMutableStruct()      // expected-note 4 {{change 'let' to 'var' to make it mutable}} {{3-6=var}} {{3-6=var}} {{3-6=var}} {{3-6=var}}
  markUsed(rvalue.nonmutating_property) // ok
  markUsed(rvalue.mutating_property)    // expected-error {{cannot use mutating getter on immutable value: 'rvalue' is a 'let' constant}}
  markUsed(rvalue.weird_property)       // expected-error {{cannot use mutating getter on immutable value: 'rvalue' is a 'let' constant}}
  rvalue.nonmutating_property = 1    // ok
  rvalue.mutating_property = 1       // expected-error {{cannot use mutating getter on immutable value: 'rvalue' is a 'let' constant}}
  rvalue.weird_property = 1          // expected-error {{cannot use mutating getter on immutable value: 'rvalue' is a 'let' constant}}

 var lvalue = TestMutableStruct()
 markUsed(lvalue.mutating_property)    // ok
 markUsed(lvalue.nonmutating_property) // ok
 markUsed(lvalue.weird_property) // ok
 lvalue.mutating_property = 1       // ok
 lvalue.nonmutating_property = 1    // ok
 lvalue.weird_property = 1    // ok
}

protocol OpaqueBase {}
extension OpaqueBase {
  var x: Int { get { return 0 } set { } } // expected-note {{candidate is marked 'mutating' but protocol does not allow it}}
}

protocol OpaqueRefinement : class, OpaqueBase {
  var x: Int { get set } // expected-note {{protocol requires property 'x' with type 'Int'}}
}

class SetterMutatingConflict : OpaqueRefinement {} // expected-error {{type 'SetterMutatingConflict' does not conform to protocol 'OpaqueRefinement'}}

struct DuplicateMutating {
  mutating mutating func f() {} // expected-error {{duplicate modifier}} expected-note {{modifier already specified here}}
}

protocol SubscriptNoGetter {
  subscript (i: Int) -> Int { get }
}

func testSubscriptNoGetter(let iis: SubscriptNoGetter) { // expected-error {{'let' as a parameter attribute is not allowed}}{{28-31=}}
  var _: Int = iis[17]
}

func testSelectorStyleArguments1(_ x: Int, bar y: Int) {
  var x = x
  var y = y
  x += 1
  y += 1
  _ = x
  _ = y
}

func testSelectorStyleArguments2(let x: Int, // expected-error {{'let' as a parameter attribute is not allowed}}{{34-37=}}
                                 let bar y: Int) { // expected-error {{'let' as a parameter attribute is not allowed}}{{34-38=}}
                                 
  
}
func testSelectorStyleArguments3(_ x: Int, bar y: Int) {
  ++x  // expected-error {{cannot pass immutable value to mutating operator: 'x' is a 'let' constant}}
  ++y  // expected-error {{cannot pass immutable value to mutating operator: 'y' is a 'let' constant}}
}

func invalid_inout(inout var x : Int) { // expected-error {{parameter must not have multiple '__owned', 'inout', '__shared', 'var', or 'let' specifiers}} {{26-30=}}
// expected-error @-1 {{'inout' before a parameter name is not allowed, place it before the parameter type instead}}{{20-25=}}{{34-34=inout }}
}
func invalid_var(var x: Int) { // expected-error {{'var' as a parameter attribute is not allowed}}
  
}
func takesClosure(_: (Int) -> Int) {
  takesClosure { (var d) in d } // expected-error {{'var' as a parameter attribute is not allowed}}
}

func updateInt(_ x : inout Int) {}

// rdar://15785677 - allow 'let' declarations in structs/classes be initialized in init()
class LetClassMembers {
  let a : Int       // expected-note 2 {{change 'let' to 'var' to make it mutable}} {{3-6=var}} {{3-6=var}}
  let b : Int       // expected-note {{change 'let' to 'var' to make it mutable}} {{3-6=var}}

  init(arg : Int) {
    a = arg             // ok, a is mutable in init()
    updateInt(&a)       // ok, a is mutable in init() and has been initialized
    b = 17              // ok, b is mutable in init()
  }

  func f() {
    a = 42  // expected-error {{cannot assign to property: 'a' is a 'let' constant}}
    b = 42  // expected-error {{cannot assign to property: 'b' is a 'let' constant}}
    updateInt(&a)   // expected-error {{cannot pass immutable value as inout argument: 'a' is a 'let' constant}}
  }
}
struct LetStructMembers {
  let a : Int       // expected-note 2 {{change 'let' to 'var' to make it mutable}} {{3-6=var}} {{3-6=var}}
  let b : Int       // expected-note {{change 'let' to 'var' to make it mutable}} {{3-6=var}}

  init(arg : Int) {
    a = arg             // ok, a is mutable in init()
    updateInt(&a)       // ok, a is mutable in init() and has been initialized
    b = 17              // ok, b is mutable in init()
  }

  func f() {
    a = 42  // expected-error {{cannot assign to property: 'a' is a 'let' constant}}
    b = 42  // expected-error {{cannot assign to property: 'b' is a 'let' constant}}
    updateInt(&a)   // expected-error {{cannot pass immutable value as inout argument: 'a' is a 'let' constant}}
  }
}

func QoI() {
  let x = 97 // expected-note {{change 'let' to 'var' to make it mutable}} {{3-6=var}}
  x = 17         // expected-error {{cannot assign to value: 'x' is a 'let' constant}}

  var get_only: Int {
    get { return 7 }
  }
  get_only = 92            // expected-error {{cannot assign to value: 'get_only' is a get-only property}}
}

func func1() -> Int { return 7 }

func func2() {
  func func3() {}

  func3() = 0 // expected-error {{expression is not assignable: 'func3' returns immutable value}}
}

func assignmentsToFuncs() {

  LetClassMembers(arg: 7).f() = 5  // expected-error {{expression is not assignable: function call returns immutable value}}
  LetStructMembers(arg: 7).f() = 5 // expected-error {{expression is not assignable: function call returns immutable value}}

  func1() = 9     // expected-error {{expression is not assignable: 'func1' returns immutable value}}
  func2() = "rrr" // expected-error {{expression is not assignable: 'func2' returns immutable value}}

  var x = 0
  (x, func1() = 0) = (4, 5) // expected-error {{expression is not assignable: 'func1' returns immutable value}}
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
  let x : Int // expected-note {{declared here}}
  init(newX: Int) {
    x = 42
  }
  
  convenience init(newY: Int) {
    self.init(newX: 19)
    x = 67  // expected-error {{'let' property 'x' may not be initialized directly; use "self.init(...)" or "self = ..." instead}}
  }
}

struct StructWithDelegatingInit {
  let x: Int // expected-note {{declared here}}
  
  init(x: Int) { self.x = x }
  init() { self.init(x: 0); self.x = 22 } // expected-error {{'let' property 'x' may not be initialized directly; use "self.init(...)" or "self = ..." instead}}
}

func test_recovery_missing_name_2(let: Int) {} // expected-error {{'let' as a parameter attribute is not allowed}}{{35-38=}} 
// expected-error @-1 {{expected parameter name followed by ':'}}

// <rdar://problem/16792027> compiler infinite loops on a really really mutating function
struct F { // expected-note 1 {{in declaration of 'F'}}
  mutating mutating mutating f() { // expected-error 2 {{duplicate modifier}} expected-note 2 {{modifier already specified here}} expected-error {{expected declaration}}
  }
  
  mutating nonmutating func g() {}  // expected-error {{method must not be declared both mutating and nonmutating}}
  __consuming nonmutating func h() {}  // expected-error {{method must not be declared both __consuming and nonmutating}}
  __consuming mutating func i() {}  // expected-error {{method must not be declared both __consuming and mutating}}
  nonmutating mutating func j() {}  // expected-error {{method must not be declared both nonmutating and mutating}}
  __consuming nonmutating mutating func k() {}  // expected-error {{method must not be declared both __consuming and mutating}} expected-error {{method must not be declared both nonmutating and mutating}}
}

protocol SingleIntProperty {
  var i: Int { get }
}

struct SingleIntStruct : SingleIntProperty {
  let i: Int       // expected-note {{change 'let' to 'var' to make it mutable}} {{3-6=var}}
}

extension SingleIntStruct {
  init(_ other: SingleIntStruct) {
    other.i = 999 // expected-error {{cannot assign to property: 'i' is a 'let' constant}}
  }
}



// <rdar://problem/19370429> QoI: fixit to add "mutating" when assigning to a member of self in a struct
// <rdar://problem/17632908> QoI: Modifying struct member in non-mutating function produces difficult to understand error message
struct TestSubscriptMutability {
  let let_arr = [1,2,3]  // expected-note 2 {{change 'let' to 'var' to make it mutable}} {{3-6=var}} {{3-6=var}}
  var var_arr = [1,2,3]

  func nonmutating1() {
    let_arr[1] = 1  // expected-error {{cannot assign through subscript: 'let_arr' is a 'let' constant}}
  }
  func nonmutating2() { // expected-note {{mark method 'mutating' to make 'self' mutable}} {{3-3=mutating }}
    var_arr[1] = 1  // expected-error {{cannot assign through subscript: 'self' is immutable}}
  }

  func nonmutating3() { // expected-note {{mark method 'mutating' to make 'self' mutable}} {{3-3=mutating }}
    self = TestSubscriptMutability() // expected-error {{cannot assign to value: 'self' is immutable}}
  }

  subscript(a : Int) -> TestSubscriptMutability {
    return TestSubscriptMutability()
  }

  func test() {
    self[1] = TestSubscriptMutability()  // expected-error {{cannot assign through subscript: subscript is get-only}}
    self[1].var_arr = [] // expected-error {{cannot assign to property: subscript is get-only}}
    self[1].let_arr = [] // expected-error {{cannot assign to property: 'let_arr' is a 'let' constant}}
  }
}

func f(_ a : TestSubscriptMutability) {
  a.var_arr = []  // expected-error {{cannot assign to property: 'a' is a 'let' constant}}
}

struct TestSubscriptMutability2 {
  subscript(a : Int) -> Int {
    get { return 42 }
    set {}
  }

  func test() {  // expected-note {{mark method 'mutating' to make 'self' mutable}} {{3-3=mutating }}
    self[1] = 2  // expected-error {{cannot assign through subscript: 'self' is immutable}}
  }
}

struct TestBangMutability {
  let let_opt = Optional(1)  // expected-note 2 {{change 'let' to 'var' to make it mutable}} {{3-6=var}} {{3-6=var}}
  var var_opt = Optional(1)

  func nonmutating1() {      // expected-note {{mark method 'mutating' to make 'self' mutable}} {{3-3=mutating }}
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

// <rdar://problem/21176945> mutation through ? not considered a mutation
func testBindOptional() {
  var a : TestStruct2? = nil  // Mutated through optional chaining.
  a?.mutatingfunc()
}

struct HasTestStruct2 {
  var t = TestStruct2()
}

func testConditional(b : Bool) {
  var x = TestStruct2()
  var y = TestStruct2()
  var z = HasTestStruct2()

  (b ? x : y).mutatingfunc() // expected-error {{cannot use mutating member on immutable value: result of conditional operator '? :' is never mutable}}

  (b ? (b ? x : y) : y).mutatingfunc() // expected-error {{cannot use mutating member on immutable value: result of conditional operator '? :' is never mutable}}

  (b ? x : (b ? x : y)).mutatingfunc() // expected-error {{cannot use mutating member on immutable value: result of conditional operator '? :' is never mutable}}

  (b ? x : z.t).mutatingfunc() // expected-error {{cannot use mutating member on immutable value: result of conditional operator '? :' is never mutable}}
}



func f(a : FooClass, b : LetStructMembers) {
  a.baz = 1   // expected-error {{cannot assign to value: 'baz' is a method}}
  b.f = 42    // expected-error {{cannot assign to value: 'f' is a method}}
}

// SR-2354: Reject subscript declarations with mutable parameters.
class MutableSubscripts {
  var x : Int = 0

  subscript(x: inout Int) -> () { x += 1 } // expected-error {{'inout' must not be used on subscript parameters}}
  subscript<T>(x: inout T) -> () { // expected-error {{'inout' must not be used on subscript parameters}}
    fatalError()
  }

  static func initialize(from state: inout MutableSubscripts) -> MutableSubscripts {
    return state
  }
}


// SR-4214: Misleading location-less diagnostic when closure parameter type
// is inferred to be inout.
func sr4214() {
  func sequence<T>(_ x : T, _ f : (T) -> T) -> T {
    return f(x)
  }

  let closure = { val in val.x = 7 } as (inout MutableSubscripts) -> () // Ok
  var v = MutableSubscripts()
  closure(&v)
  // FIXME: This diagnostic isn't really all that much better
  // expected-error@+1 {{cannot convert value of type '(inout MutableSubscripts) -> ()' to expected argument type '(_) -> _'}}
  sequence(v) { (state : inout MutableSubscripts) -> () in
    _ = MutableSubscripts.initialize(from: &state)
    return ()
  }
}

