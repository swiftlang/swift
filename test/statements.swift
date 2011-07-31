// RUN: %swift %s -verify

import swift

func f1(a : int, y : int)
func f2()
func f3() -> int

func funcdecl5(a : int, y : int) {
  var x : int

  // a few statements
  if (x != 0) {
    if (x != 0 || f3() != 0) {
      while(true) ( 4 2 min 123 min 425 () )
    }
  }

  // Semi colon statements.
  ;;;;

  // Brace statements.
  {
    {
      f2()
    }
  }


  var B : bool;

  // if/then/else.
  if (B) {
  } else if (y == 2) {
  }

  if (x) {}   // expected-error {{expression of type 'int' is not legal in a condition}}

  if (B) {
    f1(1,2)
  } else {
    f2();
  }

  if (B) {
    if (B) {
      f1(1,2)
    } else {
      f2();
    }
  } else {
    f2();
  }
}
