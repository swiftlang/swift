// RUN: %swift -parse %s -verify

func f1() -> Int { }
func f2(Int = 5) -> Int { }
func f3(Int...) -> Int { }

class A { }
class B : A { 
  func iAmAB() {}
}

func f4() -> B { }
func f5(a: A) { }
func f6(a: A, Int) { }

func createB() -> B { }
func createB(i: Int) -> B { }

func f7(a: A, () -> Int) -> B { }
func f7(a: A, Int) -> Int { }

// Forgot the '()' to call a function.
func forgotCall() {
  // Simple cases
  var x: Int
  x = f1 // expected-error{{function produces expected type 'Int'; did you mean to call it with '()'?}}{{9-9=()}}
  x = f2 // expected-error{{function produces expected type 'Int'; did you mean to call it with '()'?}}{{9-9=()}}
  x = f3 // expected-error{{function produces expected type 'Int'; did you mean to call it with '()'?}}{{9-9=()}}

  // With a supertype conversion
  var a = A()
  a = f4 // expected-error{{function produces expected type 'B'; did you mean to call it with '()'?}}{{9-9=()}}

  // As a call
  f5(f4) // expected-error{{function produces expected type 'B'; did you mean to call it with '()'?}}{{8-8=()}}
  f6(f4, f2) // expected-error{{function produces expected type 'B'; did you mean to call it with '()'?}}{{8-8=()}}
  // expected-error @-1{{function produces expected type 'Int'; did you mean to call it with '()'?}}{{12-12=()}}

  // With overloading: only one succeeds.
  a = createB // expected-error{{function produces expected type 'B'; did you mean to call it with '()'?}}{{14-14=()}}

  // With overloading, pick the fewest number of fixes.
  var b = f7(f4, f1) // expected-error{{function produces expected type 'B'; did you mean to call it with '()'?}}{{16-16=()}}
  b.iAmAB()
}
