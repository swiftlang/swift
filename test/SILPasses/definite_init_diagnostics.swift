// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -module-cache-path %t/clang-module-cache -sdk %S/../SILGen/Inputs %s -I %S/../SILGen/Inputs -enable-source-import -parse-stdlib -o /dev/null -verify

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
  var x : Int
  
  var computedProperty : Int { return 42 }

  init() { x = 0 }
  init(b : Bool) {
    if (b) {}
  } // expected-error {{property 'self.x' not initialized}}
  
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
  
  var f : SomeProtocol = dp  // ok, init'd by upcast_existential.
  
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

  for i in 0..45 {
  }

  t.0 = 1

  for i in 0..45 {
  }
  
  return t.1+t.0  // No diagnostic, everything is fully initialized.
}

class SomeDerivedClass : SomeClass {
  var y : Int
  init() { 
    y = 42  // ok
    super.init()
  }
  
  init(a : Bool) {
    super.init() // expected-error {{property 'self.y' not initialized at super.init call}}
  }

  init(a : Bool, b : Bool) {
    // x is a superclass member.  It cannot be used before we are initialized.
    x = 17  // expected-error {{use of property 'x' in base object before super.init}}
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
    x = 42        // expected-error {{use of property 'x' in base object before super.init}}
  }               // expected-error {{super.init isn't called before returning from initializer}}
  
  func someMethod() {}
  
  init(a : Int) {
    y = 42
    super.init()
  }

  init(a : Int, b : Bool) {
    y = 42
    someMethod() // expected-error 2 {{'self' used before super.init call}}
    super.init()
  }

  init(a : Int, b : Int) {
    y = 42
    baseMethod()  // expected-error {{use of method 'baseMethod' in base object before super.init initializes it}}
    super.init()
  }
  
  init(a : Int, b : Int, c : Int) {
    y = computedProperty  // expected-error {{use of property 'computedProperty' in base object before super.init initializes it}}
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

  init() {
    super.init()
  }

  init(i: Int) {
    if i > 0 {
      super.init()
    }
  } // expected-error{{super.init isn't called before returning from initializer}}

  init(d: Double) {
    f() // expected-error 2{{'self' used before super.init call}}
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
  init() {
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
    for i in 0..42 {
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

