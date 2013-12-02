// RUN: %swift %s -o /dev/null -verify

// These are tests for definite initialization, which is implemented by the
// memory promotion pass.

func test1() -> Int {
  var a : Int  // expected-note {{variable defined here}}
  return a     // expected-error {{variable 'a' used before being initialized}}
}

func takes_inout(a: @inout Int) {}
func takes_closure(fn: () -> ()) {}

class SomeClass { 
  init() { }
  var x : Int 
}

struct SomeStruct { var x : Int }

func test2() {
  // inout.

  var a1 : Int        // expected-note {{variable defined here}}
  takes_inout(&a1)    // expected-error {{variable 'a1' passed by reference before being initialized}}

  var a2 = 4
  takes_inout(&a2)    // ok.

  var a3 : Int
  a3 = 4
  takes_inout(&a3)    // ok.
  

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
  @weak var w1 : SomeClass?// expected-note {{variable defined here}}
  var _ = w1                // expected-error {{variable 'w1' used before being initialized}}

  @weak var w2 = SomeClass()
  var _ = w2                // ok
  
  
  // Unowned.  This is immediately crashing code (it causes a retain of a
  // released object) so it should be diagnosed with a warning someday.
  @unowned var u1 : SomeClass // expected-note {{variable defined here}}
  var _ = u1                // expected-error {{variable 'u1' used before being initialized}}

  @unowned var u2 = SomeClass()
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

func tupleinout(a: @inout (lo: Int, hi: Int)) {
  print(a.0)   // ok
  print(a.1)   // ok
}

// Address only types
func test5<T>(x: T, y: T) {
  var a : ((T, T), T)  // expected-note {{variable defined here}}
  a.0 = (x, y)
  
  var b = a     // expected-error {{variable 'a.1' used before being initialized}}
}


struct IntFloatStruct { var a : Int, b : Float }

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
  destructor() {            // expected-error {{variable 'g4' used by function definition before being initialized}}
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
  func trivial(n : Int) {
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
}
