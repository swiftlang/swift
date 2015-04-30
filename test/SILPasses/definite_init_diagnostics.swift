// RUN: %target-swift-frontend -emit-sil -sdk %S/../SILGen/Inputs %s -I %S/../SILGen/Inputs -enable-source-import -parse-stdlib -o /dev/null -verify

// FIXME: rdar://problem/19648117 Needs splitting objc parts out
// XFAIL: linux

import Swift
import gizmo

// These are tests for definite initialization, which is implemented by the
// memory promotion pass.

func test1() -> Int {
  var a : Int  // expected-note {{variable defined here}}
  return a     // expected-error {{variable 'a' used before being initialized}}
}

func takes_inout(inout a: Int) {}
func takes_closure(fn: () -> ()) {}

class SomeClass { 
  var x : Int   // expected-note {{'self.x' not initialized}}
  
  var computedProperty : Int { return 42 }

  init() { x = 0 }
  init(b : Bool) {
    if (b) {}
  } // expected-error {{return from initializer without initializing all stored properties}}
  
  func baseMethod() {}
}

struct SomeStruct { var x = 1 }

func test2() {
  // inout.

  var a1 : Int        // expected-note {{variable defined here}}
  takes_inout(&a1)    // expected-error {{variable 'a1' passed by reference before being initialized}}

  var a2 = 4
  takes_inout(&a2)    // ok.

  var a3 : Int
  a3 = 4
  takes_inout(&a3)    // ok.
  
  // Address-of with Builtin.addressof.
  var a4 : Int            // expected-note {{variable defined here}}
  Builtin.addressof(&a4)  // expected-error {{address of variable 'a4' taken before it is initialized}}


  // Closures.

  var b1 : Int        // expected-note {{variable defined here}}
  takes_closure {     // expected-error {{variable 'b1' used before being initialized}}
    print(b1)
  }

  var b2 = 4
  takes_closure {     // ok.
    print(b2)
  }

  var b3 : Int
  b3 = 4
  takes_closure {     // ok.
    print(b3)
  }

  // Structs
  var s1 : SomeStruct
  s1 = SomeStruct()   // ok

  var s2 : SomeStruct  // expected-note {{variable defined here}}
  s2.x = 1             // expected-error {{struct 's2' must be completely initialized before a member is stored to}}


  // Classes
  var c1 : SomeClass   // expected-note {{variable defined here}}
  print(c1.x)          // expected-error {{variable 'c1' used before being initialized}}


  var c2 = SomeClass()
  print(c2.x)          // ok
  
  
  // Weak
  weak var w1 : SomeClass?
  var _ = w1                // ok: default-initialized

  weak var w2 = SomeClass()
  var _ = w2                // ok
  
  
  // Unowned.  This is immediately crashing code (it causes a retain of a
  // released object) so it should be diagnosed with a warning someday.
  unowned var u1 : SomeClass // expected-note {{variable defined here}}
  var _ = u1                // expected-error {{variable 'u1' used before being initialized}}

  unowned var u2 = SomeClass()
  var _ = u2                // ok
}



// Tuple field sensitivity.
func test4() {
  var t1 = (1, 2, 3)
  print(t1.0 + t1.1 + t1.2)  // ok


  var t2 : (Int, Int, Int)   // expected-note 3 {{variable defined here}}
  print(t2.0)   // expected-error {{variable 't2.0' used before being initialized}}
  print(t2.1)   // expected-error {{variable 't2.1' used before being initialized}}
  print(t2.2)   // expected-error {{variable 't2.2' used before being initialized}}


  var t3 : (Int, Int, Int)   // expected-note {{variable defined here}}
  t3.0 = 1; t3.2 = 42
  print(t3.0)
  print(t3.1)   // expected-error {{variable 't3.1' used before being initialized}}
  print(t3.2)


  // Partially set, wholey read.
  var t4 : (Int, Int, Int)   // expected-note 1 {{variable defined here}}
  t4.0 = 1; t4.2 = 42
  var _ = t4            // expected-error {{variable 't4.1' used before being initialized}}
  

  // Subelement sets.
  var t5 : (a : (Int, Int), b : Int)  // expected-note {{variable defined here}}
  t5.a = (1,2)
  print(t5.a.0)
  print(t5.a.1)
  print(t5.b)       // expected-error {{variable 't5.b' used before being initialized}}
  

  var t6 : (a : (Int, Int), b : Int)  // expected-note {{variable defined here}}
  t6.b = 12; t6.a.1 = 1
  print(t6.a.0)     // expected-error {{variable 't6.a.0' used before being initialized}}
  print(t6.a.1)
  print(t6.b)
}

func tupleinout(inout a: (lo: Int, hi: Int)) {
  print(a.0)   // ok
  print(a.1)   // ok
}

// Address only types
func test5<T>(x: T, y: T) {
  var a : ((T, T), T)  // expected-note {{variable defined here}}
  a.0 = (x, y)
  
  var b = a     // expected-error {{variable 'a.1' used before being initialized}}
}


struct IntFloatStruct { var a = 1, b = 2.0 }

func test6() -> Int {
  var a = IntFloatStruct()

  a.a = 1
  a.b = 2

  return a.a
}

// Control flow.
func test7(cond: Bool) {
  var a : Int
  
  if cond { a = 42 } else { a = 17 }
  print(a)         // ok

  var b : Int      // expected-note {{variable defined here}}
  if cond { } else { b = 17 }
  print(b)         // expected-error {{variable 'b' used before being initialized}}
}

protocol SomeProtocol {
  func protoMe()
}

protocol DerivedProtocol : SomeProtocol {}

func existentials(i: Int, dp: DerivedProtocol) {
  var a : Any = ()
  a = i

  var b : Any
  b = ()

  var c : Any   // no uses.
  
  var d1 : Any  // expected-note {{variable defined here}}
  var d2 = d1   // expected-error {{variable 'd1' used before being initialized}}
  
  var e : SomeProtocol  // expected-note {{variable defined here}}
  e.protoMe()           // expected-error {{variable 'e' used before being initialized}}
  
  var f : SomeProtocol = dp  // ok, init'd by existential upcast.
  
  var g : DerivedProtocol   // expected-note {{variable defined here}}
  f = g                     // expected-error {{variable 'g' used before being initialized}}
}


// Tests for top level code.
var g1 : Int                 // expected-note {{variable defined here}}
var g2 : Int = 42

func testTopLevelCode() {    // expected-error {{variable 'g1' used by function definition before being initialized}}
  print(g1)
  print(g2)
}

var (g3,g4) : (Int,Int)          // expected-note 2 {{variable defined here}}
class DITLC_Class {
  init() {            // expected-error {{variable 'g3' used by function definition before being initialized}}
    print(g3)
  }
  deinit {            // expected-error {{variable 'g4' used by function definition before being initialized}}
    print(g4)
  }
}

struct EmptyStruct {}

func useEmptyStruct(a: EmptyStruct) {}

func emptyStructTest() {
  var a : EmptyStruct  // expected-note {{variable defined here}}
  useEmptyStruct(a)    // expected-error {{variable 'a' used before being initialized}}

  var (b,c,d) : (EmptyStruct,EmptyStruct,EmptyStruct) // expected-note 2 {{variable defined here}}
  
  c = EmptyStruct()

  useEmptyStruct(b)     // expected-error {{variable 'b' used before being initialized}}
  useEmptyStruct(c)
  useEmptyStruct(d)     // expected-error {{variable 'd' used before being initialized}}

  var (e,f) : (EmptyStruct,EmptyStruct)
  (e, f) = (EmptyStruct(),EmptyStruct())
  
  useEmptyStruct(e)
  useEmptyStruct(f)

  var g : (EmptyStruct,EmptyStruct)
  g = (EmptyStruct(),EmptyStruct())
  
  useEmptyStruct(g.0)
  useEmptyStruct(g.1)
}

// These are tests for values that are only initialized on some path through the
// CFG.
func partialInit() {

  // Value of trivial type.
  func trivial(ni : Int) {
    var n = ni
    while (n > 0) {
      --n
      var x : Int
      if (n > 2) { continue }
      x = 1
    }
  }
  
  // Tuple with only some elements specified.
  func trivial_tuple() {
    var a : (Int, Int)
    a.1 = 1
  }


  func tuple_test(cond : Bool) {
    var x : (SomeClass, SomeClass)

    if cond {
      x.0 = SomeClass()
    } else {
      x.1 = SomeClass()
    }
  }
}

func takesTuplePair(inout a : (SomeClass, SomeClass)) {}

// This tests cases where an store might be an init or assign based on control
// flow path reaching it.
func conditionalInitOrAssign(c : Bool, x : Int) {
  var t : Int  // Test trivial types.
  if c {
    t = 0
  }
  if c {
    t = x
  }
  t = 2
  
  // Nontrivial type
  var sc : SomeClass
  if c {
    sc = SomeClass()
  }
  sc = SomeClass()
  
  // Tuple element types
  var tt : (SomeClass, SomeClass)

  if c {
    tt.0 = SomeClass()
  } else {
    tt.1 = SomeClass()
  }

  tt.0 = SomeClass()
  tt.1 = tt.0
  
  var t2 : (SomeClass, SomeClass)  // expected-note {{variable defined here}}
  t2.0 = SomeClass()
  takesTuplePair(&t2)   // expected-error {{variable 't2.1' passed by reference before being initialized}}
}

enum NotInitializedUnion {
  init() {
  }    // expected-error {{return from enum initializer method without storing to 'self'}}
  case X
  case Y
}

extension NotInitializedUnion {
  init(a : Int) {
  }   // expected-error {{return from enum initializer method without storing to 'self'}}
}

enum NotInitializedGenericUnion<T> {
  init() { 
  }    // expected-error {{return from enum initializer method without storing to 'self'}}
  case X
}


func tuple_test() -> Int {
  var t : (Int, Int)

  t.1 = 4

  for i in 0..<45 {
  }

  t.0 = 1

  for i in 0..<45 {
  }
  
  return t.1+t.0  // No diagnostic, everything is fully initialized.
}

class SomeDerivedClass : SomeClass {
  var y : Int
  override init() { 
    y = 42  // ok
    super.init()
  }
  
  init(a : Bool) {
    super.init() // expected-error {{property 'self.y' not initialized at super.init call}}
  }

  init(a : Bool, b : Bool) {
    // x is a superclass member.  It cannot be used before we are initialized.
    x = 17  // expected-error {{use of 'self' in property access 'x' before super.init initializes self}}
    y = 42
    super.init()
  }
  
  init(a : Bool, b : Bool, c : Bool) {
    y = 42
    super.init()
  }

  init(a : Bool, b : Bool, c : Bool, d : Bool) {
    y = 42
    super.init()
    super.init() // expected-error {{super.init called multiple times in initializer}}
  }

  init(a : Bool, b : Bool, c : Bool, d : Bool, e : Bool) {
    super.init()  // expected-error {{property 'self.y' not initialized at super.init call}}
    super.init()  // expected-error {{super.init called multiple times in initializer}}
  }
  
  init(a : Bool, b : Bool, c : Bool, d : Bool, e : Bool, f : Bool) {
    y = 11
    if a { super.init() }
    x = 42        // expected-error {{use of 'self' in property access 'x' before super.init initializes self}}
  }               // expected-error {{super.init isn't called before returning from initializer}}
  
  func someMethod() {}
  
  init(a : Int) {
    y = 42
    super.init()
  }

  init(a : Int, b : Bool) {
    y = 42
    someMethod() // expected-error {{use of 'self' in method call 'someMethod' before super.init initializes self}}
    super.init()
  }

  init(a : Int, b : Int) {
    y = 42
    baseMethod()  // expected-error {{use of 'self' in method call 'baseMethod' before super.init initializes self}}
    super.init()
  }
  
  init(a : Int, b : Int, c : Int) {
    y = computedProperty  // expected-error {{use of 'self' in property access 'computedProperty' before super.init initializes self}}
    super.init()
  }

}

//===----------------------------------------------------------------------===//
//  Delegating initializers
//===----------------------------------------------------------------------===//


class DelegatingCtorClass {
  var ivar: EmptyStruct

  init() { ivar = EmptyStruct() }

  convenience init(x: EmptyStruct) {
    self.init()
    var tmp = ivar // okay: ivar has been initialized by the delegation above
  }
  
  convenience init(x: EmptyStruct, y: EmptyStruct) {
    ivar = x       // expected-error {{use of 'self' in delegating initializer before self.init is called}}
    self.init()
  }

  convenience init(x: EmptyStruct, y: EmptyStruct, z: EmptyStruct) {
    self.init()
    self.init()    // expected-error {{self.init called multiple times in initializer}}
  }

  convenience init(c : Bool) {
    if c {
      return
    }
    self.init()
  }                // expected-error {{self.init isn't called on all paths in delegating initializer}}

  convenience init(bool: Bool) {
    doesntReturn()
  }

  convenience init(double: Double) {
  } // expected-error{{self.init isn't called on all paths in delegating initializer}}
}


struct DelegatingCtorStruct {
  var ivar : EmptyStruct

  init() { ivar = EmptyStruct() }


  init(a : Double) {
    self.init()
    var tmp = ivar // okay: ivar has been initialized by the delegation above
  }
  
  init(a : Int) {
    var tmp = ivar // expected-error {{use of 'self' in delegating initializer before self.init is called}}
    self.init()
  }

  init(a : Float) {
    self.init()
    self.init()    // expected-error {{self.init called multiple times in initializer}}
  }
  
  init(c : Bool) {
    if c {
      return
    }

    self.init()
  }                // expected-error {{self.init isn't called on all paths in delegating initializer}}

}


enum DelegatingCtorEnum {
  case Dinosaur, Train, Truck

  init() { self = .Train }

  init(a : Double) {
    self.init()
    var tmp = self // okay: self has been initialized by the delegation above
    self = .Dinosaur
  }
  
  init(a : Int) {
    var tmp = self // expected-error {{use of 'self' in delegating initializer before self.init is called}}
    self.init()
  }

  init(a : Float) {
    self.init()
    self.init()    // expected-error {{self.init called multiple times in initializer}}
  }
  
  init(c : Bool) {
     if c {
      return
    }

    self.init()
  }                // expected-error {{self.init isn't called on all paths in delegating initializer}}
}

@requires_stored_property_inits
class RequiresInitsDerived : Gizmo {
  var a = 1

  override init() {
    super.init()
  }

  init(i: Int) {
    if i > 0 {
      super.init()
    }
  } // expected-error{{super.init isn't called before returning from initializer}}

  init(d: Double) {
    f() // expected-error {{use of 'self' in method call 'f' before super.init initializes self}}
    super.init()
  }

  func f() { }
}


// rdar://16119509 - Dataflow problem where we reject valid code.
class rdar16119509_Buffer {
  init(x : Int) { }
}
class rdar16119509_Base {}
class rdar16119509_Derived : rdar16119509_Base {
  override init() {
    var capacity = 2
    while capacity < 2 {
      capacity <<= 1
    }
    buffer = rdar16119509_Buffer(x: capacity)
  }

  var buffer : rdar16119509_Buffer
}

// <rdar://problem/16797372> Bogus error: self.init called multiple times in initializer
extension Foo {
  convenience init() {
    for i in 0..<42 {
    }
    self.init(a: 4)
  }
}

class Foo {
  init(a : Int) {}
}



@noreturn
func doesntReturn() {
  while true {}
}

func doesReturn() {}

func testNoReturn1(b : Bool) -> Any {
  var a : Any
  if b {
    a = 42
  } else {
    doesntReturn()
  }

  return a   // Ok, because the noreturn call path isn't viable.
}

func testNoReturn2(b : Bool) -> Any {
  var a : Any  // expected-note {{variable defined here}}
  if b {
    a = 42
  } else {
    doesReturn()
  }

  // Not ok, since doesReturn() doesn't kill control flow.
  return a   // expected-error {{variable 'a' used before being initialized}}
}



// <rdar://problem/16687555> [DI] New convenience initializers cannot call inherited convenience initializers
class BaseWithConvenienceInits {
 init(int: Int) {}
 convenience init() {
    self.init(int: 3)
 }
}

class DerivedUsingConvenienceInits : BaseWithConvenienceInits {
  convenience init(string: String) {
    self.init()
  }
}

// <rdar://problem/16660680> QoI: _preconditionFailure() in init method complains about super.init being called multiple times
class ClassWhoseInitDoesntReturn : BaseWithConvenienceInits {
  init() {  
    _preconditionFailure("leave me alone dude");
  }
}

// <rdar://problem/17233681> DI: Incorrectly diagnostic in delegating init with generic enum
enum r17233681Lazy<T>  {
  case Thunk(() -> T)
  case Value(T)
  
  init(value: T) {
    self = .Value(value)
  }
}

extension r17233681Lazy {
  init(otherValue: T) {
    self.init(value: otherValue)
  }
}


// <rdar://problem/17556858> delegating init that delegates to @transparent init fails
struct FortyTwo { }

extension Double {
  init(v : FortyTwo) {
    self.init(0.0)
  }
}

// <rdar://problem/17686667> If super.init is implicitly inserted, DI diagnostics have no location info
class r17686667Base {}
class r17686667Test : r17686667Base {
  var x: Int
  override init() {  // expected-error {{property 'self.x' not initialized at implicitly generated super.init call}}

  }
}

// <rdar://problem/18199087> DI doesn't catch use of super properties lexically inside super.init call
class r18199087BaseClass {
  let data: Int
  init(val: Int) {
    data = val
  }
}
class r18199087SubClassA: r18199087BaseClass {
  init() {
    super.init(val: self.data)  // expected-error {{use of 'self' in property access 'data' before super.init initializes self}}
  }
}

// <rdar://problem/18414728> QoI: DI should talk about "implicit use of self" instead of individual properties in some cases
class rdar18414728Base {
  var prop:String? { return "boo" }

  // expected-note @+1 {{change 'let' to 'var' to make it mutable}}
  let aaaaa:String  // expected-note 3 {{'self.aaaaa' not initialized}}

  init() {
    if let p1 = prop { // expected-error {{use of 'self' in property access 'prop' before all stored properties are initialized}}
      aaaaa = p1
    }
    aaaaa = "foo"  // expected-error {{immutable value 'self.aaaaa' may only be initialized once}}
  }

  init(a : ()) {
    method1(42)   // expected-error {{use of 'self' in method call 'method1' before all stored properties are initialized}}
    aaaaa = "foo"
  }

  init(b : ()) {
    final_method() // expected-error {{use of 'self' in method call 'final_method' before all stored properties are initialized}}
    aaaaa = "foo"
  }

  init(c : ()) {
    aaaaa = "foo"
    final_method()  // ok
  }

  func method1(a : Int) {}
  final func final_method() {}
}

class rdar18414728Derived : rdar18414728Base {
  var prop2:String? { return "boo" }

  // expected-note @+1 2 {{change 'let' to 'var' to make it mutable}}
  let aaaaa2:String

  override init() {
    if let p1 = prop2 {  // expected-error {{use of 'self' in property access 'prop2' before super.init initializes self}}
      aaaaa2 = p1
    }
    aaaaa2 = "foo"    // expected-error {{immutable value 'self.aaaaa2' may only be initialized once}}
    super.init()
  }

  override init(a : ()) {
    method2()            // expected-error {{use of 'self' in method call 'method2' before super.init initializes self}}
    aaaaa2 = "foo"
    super.init()
  }

  override init(b : ()) {
    aaaaa2 = "foo"
    method2()           // expected-error {{use of 'self' in method call 'method2' before super.init initializes self}}
    super.init()
  }

  override init(c : ()) {
    super.init()        // expected-error {{property 'self.aaaaa2' not initialized at super.init call}}
    aaaaa2 = "foo"      // expected-error {{immutable value 'self.aaaaa2' may only be initialized once}}
    method2()
  }

  func method2() {}
}

struct rdar18414728Struct {
  var computed:Int? { return 4 }

  var i : Int  // expected-note 2 {{'self.i' not initialized}}
  var j : Int  // expected-note {{'self.j' not initialized}}

  init() {
    j = 42
    if let p1 = computed { // expected-error {{'self' used before all stored properties are initialized}}
      i = p1
    }
    i = 1
  }
  init(a : ()) {
    method(42)     // expected-error {{'self' used before all stored properties are initialized}}
    i = 1
    j = 2
  }

  func method(a : Int) {}
}


// <rdar://problem/17207456> Unable to access dynamicType of an object in a class initializer that isn't done
func use(a : Any) {}

class rdar17207456Base {
  var x: Int

  init() {
    use(self.dynamicType)
    x = 0
  }

  convenience init(a : Int) {
    use(self.dynamicType)
    self.init()
  }
}

class rdar17207456Derived : rdar17207456Base {
  override init() {
    use(self.dynamicType)
    super.init()
  }

  convenience init(a : Int) {
    use(self.dynamicType)
    self.init()
  }
}

struct rdar17207456Struct {
  var x: Int

  init() {
    use(self.dynamicType)
    x = 0
  }

  init(a : Int) {
    use(self.dynamicType)
    self.init()
  }
}

extension Int {
  mutating func mutate() {}
  func inspect() {}
}


// <rdar://problem/19035287> let properties should only be initializable, not reassignable
struct LetProperties {
  // expected-note @+1 {{change 'let' to 'var' to make it mutable}}
  let arr : [Int]
  let (u, v) : (Int, Int)
  // expected-note @+1 2 {{change 'let' to 'var' to make it mutable}}
  let w : (Int, Int)
  let x = 42
  // expected-note @+1 {{change 'let' to 'var' to make it mutable}}
  let y : Int
  let z : Int?  // expected-note{{'self.z' not initialized}}

  // Let properties can be initialized naturally exactly once along any given
  // path through an initializer.
  init(cond : Bool) {
    if cond {
      w.0 = 4
      (u,v) = (4,2)
      y = 71
    } else {
      y = 13
      v = 2
      u = v+1
      w.0 = 7
    }
    w.1 = 19
    z = nil
    arr = []
  }

  // Multiple initializations are an error.
  init(a : Int) {
    y = a
    y = a   // expected-error {{immutable value 'self.y' may only be initialized once}}

    u = a
    v = a
    u = a   // expected-error {{immutable value 'self.u' may only be initialized once}}
    v = a   // expected-error {{immutable value 'self.v' may only be initialized once}}

    w.0 = a
    w.1 = a

    w.0 = a  // expected-error {{immutable value 'self.w.0' may only be initialized once}}
    w.1 = a  // expected-error {{immutable value 'self.w.1' may only be initialized once}}

    arr = []
    arr = [] // expected-error {{immutable value 'self.arr' may only be initialized once}}
  }  // expected-error {{return from initializer without initializing all stored properties}}

  // inout uses of let properties are an error.
  init() {
    u = 1; v = 13; w = (1,2); y = 1 ; z = u

    var variable = 42
    swap(&u, &variable)  // expected-error {{immutable value 'self.u' may not be passed inout}}
    
    u.inspect()  // ok, non mutating.
    u.mutate()  // expected-error {{mutating method 'mutate' may not be used on immutable value 'self.u'}}
    
    arr = []
    arr += []      // expected-error {{mutating operator '+=' may not be used on immutable value 'self.arr'}}
    arr.append(4)  // expected-error {{mutating method 'append' may not be used on immutable value 'self.arr'}}
    arr[12] = 17   // expected-error {{immutable value 'self.arr' may not be assigned to}}
  }
}


// <rdar://problem/19215313> let properties don't work with protocol method dispatch
protocol TestMutabilityProtocol {
  func toIntMax()
  mutating func changeToIntMax()
}
 
class C<T : TestMutabilityProtocol> {
  let x : T
  let y : T
  
  init(a : T) {
    x = a; y = a
    x.toIntMax()
    y.changeToIntMax()  // expected-error {{mutating method 'changeToIntMax' may not be used on immutable value 'self.y'}}
  }
}

struct MyMutabilityImplementation : TestMutabilityProtocol {
  func toIntMax() {
  }
  
  mutating func changeToIntMax() {
  }
}


// <rdar://problem/16181314> don't require immediate initialization of 'let' values
func testLocalProperties(b : Int) -> Int {
  // expected-note @+1 {{change 'let' to 'var' to make it mutable}}
  let x : Int
  let y : Int // never assigned is ok

  x = b
  x = b    // expected-error {{immutable value 'x' may only be initialized once}}

  // This is ok, since it is assigned multiple times on different paths.
  let z : Int
  if true || false {
    z = b
  } else {
    z = b
  }

  return x
}

// Should be rejected as multiple assignment.
func testAddressOnlyProperty<T>(b : T) -> T {
  // expected-note @+1 {{change 'let' to 'var' to make it mutable}}
  let x : T
  let y : T
  let z : T   // never assigned is ok.
  x = b
  y = b
  x = b   // expected-error {{immutable value 'x' may only be initialized once}}

  var tmp = b
  swap(&x, &tmp)   // expected-error {{immutable value 'x' may not be passed inout}}
  return y
}


// <rdar://problem/19254812> DI bug when referencing let member of a class
class r19254812Base {}
class r19254812Derived: r19254812Base{
  let pi = 3.14159265359
  
  init(x : ()) {
    println(pi)  // ok, no diagnostic expected.
  }
}


// <rdar://problem/19259730> Using mutating methods in a struct initializer with a let property is rejected
struct StructMutatingMethodTest {
  let a, b: String  // expected-note 2 {{'self.b' not initialized}}
  
  init(x: String, y: String) {
    a = x
    b = y
    mutate()   // ok
  }
  
  init(x: String) {
    a = x
    mutate() // expected-error {{'self' used before all stored properties are initialized}}
    b = x
  }

  init() {
    a = ""
    nonmutate() // expected-error {{'self' used before all stored properties are initialized}}
    b = ""
  }

  mutating func mutate() {}
  func nonmutate() {}
}

 // <rdar://problem/19268443> DI should reject this call to transparent function
 class TransparentFunction {
  let x : Int
  let y : Int
  init() {
    x = 42
    ++x     // expected-error {{mutating operator '++' may not be used on immutable value 'self.x'}}

    y = 12
    myTransparentFunction(&y)  // expected-error {{immutable value 'self.y' may not be passed inout}}
  }
}

@transparent
func myTransparentFunction(inout x : Int) {}


// <rdar://problem/19782264> Immutable, optional class members can't have their subproperties read from during init()
class MyClassWithAnInt {
  let channelCount : Int = 42
}
class MyClassTestExample {
  let clientFormat : MyClassWithAnInt!

  init(){
    clientFormat = MyClassWithAnInt()
    let channels = clientFormat.channelCount
  }
}


// <rdar://problem/19746552> QoI: variable "used before being initialized" instead of "returned uninitialized" in address-only enum/struct

struct AddressOnlyStructWithInit<T, U> {
  let a : T?
  let b : U?   // expected-note {{'self.b' not initialized}}
  
  init(a : T) {
    self.a = a
  }     // expected-error {{return from initializer without initializing all stored properties}}
}

enum AddressOnlyEnumWithInit<T> {
  case X(T), Y
  
  init() {
  }  // expected-error {{return from enum initializer method without storing to 'self'}}
}


// <rdar://problem/20135113> QoI: enum failable init that doesn't assign to self produces poor error
enum MyAwesomeEnum {
  case One, Two

  init?() {

  }// expected-error {{return from enum initializer method without storing to 'self'}}
}

// <rdar://problem/20679379> DI crashes on initializers on protocol extensions
extension SomeProtocol {
  init?() {
    let a = self  // expected-error {{variable 'self' used before being initialized}}
    self = a
  }

  init(a : Int) {
  } // expected-error {{return from initializer without initializing all stored properties}}
  // expected-note@-1 {{'self' not initialized}}

  init(c : Float) {
    protoMe()   // expected-error {{variable 'self' used before being initialized}}
  } // expected-error {{return from initializer without initializing all stored properties}}
  // expected-note@-1 {{'self' not initialized}}
}



// Lvalue check when the archetypes are not the same.
struct LValueCheck<T> {
  // expected-note @+1 {{change 'let' to 'var' to make it mutable}}
  let x = 0  // expected-note {{initial value already provided in 'let' declaration}}
}

extension LValueCheck<A> {
  init(newY: Int) {
    x = 42  // expected-error {{immutable value 'self.x' may only be initialized once}}
  }
}

// <rdar://problem/20477982> Accessing let-property with default value in init() can throw spurious error
struct DontLoadFullStruct {
  let x: Int = 1
  let y: Int
  init() {
    y = x  // ok!
  }
}


func testReassignment() {
  let c : Int  // expected-note {{change 'let' to 'var' to make it mutable}}
  c = 12
  c = 32  // expected-error {{immutable value 'c' may only be initialized once}}
}

