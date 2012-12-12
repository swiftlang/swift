// RUN: %swift %s -verify

class A {
  func foo() { }
}

class B : A {
  func bar() { }
}

class Other { }

func acceptA(a : A) { }

func f0<T : A>(obj : T, a : A, b : B) { // expected-note{{found this candidate}}
  // Method access
  obj.foo()
  obj.bar() // expected-error{{}}

  // Calls
  acceptA(obj)

  // Derived-to-base conversion for assignment
  a = obj

  // Invalid assignments
  obj = a // expected-error{{}}
  obj = b // expected-error{{}}

  // Downcasts
  a = A(obj)
  b = B(obj)
}

func call_f0(a : A, b : B, other : Other) {
  f0(a, a, b)
  f0(b, a, b)
  f0(other, a, b) // expected-error{{}}
}

// Declaration errors
func f1<T : A requires T : Other>() { } // expected-error{{generic parameter 'T' cannot be a subclass of both 'A' and 'Other'}}
func f2<T : A requires T : B>() { } // FIXME: expected-error{{cannot be a subclass}}
