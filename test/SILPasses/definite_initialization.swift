// RUN: %swift %s -enable-definite-init -o /dev/null -verify

// These are tests for definite initialization, which is implemented by the
// memory promotion pass.

func test1() -> Int {
  var a : Int  // expected-note {{variable defined here}}
  return a     // expected-error {{variable 'a' used before being initialized}}
}

func takes_byref(a : [byref] Int) {}
func takes_closure(fn : () -> ()) {}

class SomeClass { var x : Int }

func test2() {
  // byref.

  var a1 : Int        // expected-note {{variable defined here}}
  takes_byref(&a1)    // expected-error {{variable 'a1' passed by reference before being initialized}}

  var a2 = 4
  takes_byref(&a2)    // ok.

  var a3 : Int
  a3 = 4
  takes_byref(&a3)    // ok.
  

  // Closures.

  var b1 : Int        // expected-note {{variable defined here}}
  takes_closure {     // expected-error {{variable 'b1' captured by a closure before being initialized}}
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


  // Classes
  var c1 : SomeClass   // expected-note {{variable defined here}}
  print(c1.x)          // expected-error {{variable 'c1' used before being initialized}}


  var c2 = SomeClass()
  print(c2.x)          // ok
  
  
  // Weak
  var [weak] w1 : SomeClass // expected-note {{variable defined here}}
  var _ = w1                // expected-error {{variable 'w1' used before being initialized}}

  var [weak] w2 = SomeClass()
  var _ = w2                // ok
  
  
  // Unowned.  This is immediately crashing code (it causes a retain of a
  // released object) so it should be diagnosed with a warning someday.
  var [unowned] u1 : SomeClass // expected-note {{variable defined here}}
  var _ = u1                // expected-error {{variable 'u1' used before being initialized}}

  var [unowned] u2 = SomeClass()
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





