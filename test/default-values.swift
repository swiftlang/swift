// RUN: %swift %s -verify

import swift

var func5 : (fn : (:int,:int) -> ()) -> ();   // Takes a fn, returns nothing.
func foo3(a : int = 2, b : int = 3);

// Tuple type with default values.
func test5() {
  typealias ta1 : (a : int = ());        // expected-error {{cannot convert default value type '()' to explicitly specified type 'int'}}
  typealias ta2 : (a : int = 4);
  var ta2_v1 : ta2;
  var ta2_v2 : ta2 = 52;

  var c1 : (a : int, b : int, c : int = 3, d = 4) = (1, 2, 3, 4);

  var c2 : (a : int, b = 4)       // Value is initialized to (0,4)
  var c3 : (a : int, b = 4) = 1;  // Value is initialized to (1,4)

  var c4 : (a : int, b = 4) = (1,2);
  var c5 : (a = 3, b = 4) = ();
  var c6 : (a : int, b : int, c = 4) = (1,2);

  var c7 : (a : int = ());        // expected-error {{cannot convert default value type '()' to explicitly specified type 'int'}}

  var c8 : (a : int = 1 2); // expected-error {{expected a singular expression: this expression is unbound}}

  var c9 : (a : int = 4) = (); // Single element initialized with nothing.
  var c10 : (a : int = 4) = 4; // Single element initialized with one.
  var c11 : (a : int) = 4;     // Single nondefault element initialized with 1.

  func5 {
  var d : (a : int, b = 
    $0) = 1; // expected-error {{ambiguous expression could not resolve a concrete type}}
  }

  // Default values for functions.
  foo3(4)
  foo3()
  foo3(.a = 4)
  foo3(.b = 4)
  foo3(.a = 2, .b = 4)
}

