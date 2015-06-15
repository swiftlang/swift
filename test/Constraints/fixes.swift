// RUN: %target-parse-verify-swift

func f1() -> Int { }
func f2(Int = 5) -> Int { }
func f3(Int...) -> Int { }

class A { }
class B : A { 
  func iAmAB() {}
  func createB() -> B { return B() }
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

/// Forgot the '!' to unwrap an optional.
func parseInt() -> Int? { }

func forgotOptionalBang(a: A, obj: AnyObject) {
  var i: Int = parseInt() // expected-error{{value of optional type 'Int?' not unwrapped; did you mean to use '!' or '?'?}}{{26-26=!}}

  var a = A(), b = B()
  b = a as? B  // expected-error{{value of optional type 'B?' not unwrapped; did you mean to use '!' or '?'?}}{{7-7=(}}{{14-14=)!}}

  // rdar://problem/20377684 -- take care that the '!' doesn't fall into an
  // optional evaluation context
  let bo: B? = b
  let b2: B = bo?.createB() // expected-error{{value of optional type 'B?' not unwrapped; did you mean to use '!' or '?'?}}{{15-15=(}}{{28-28=)!}}
}

// Crash with one-element tuple with labeled element
class Dinner {}

func microwave() -> Dinner {
  return (n: Dinner?()) // expected-error{{value of optional type 'Dinner?' not unwrapped; did you mean to use '!' or '?'?}}
}

func forgotAnyObjectBang(obj: AnyObject) {
  var a = A()
  a = obj // expected-error{{'AnyObject' is not convertible to 'A'; did you mean to use 'as!' to force downcast?}}{{10-10= as! A}}
  _ = a
}

func increment(inout x: Int) { }

func forgotAmpersand() {
  var i = 5
  increment(i) // expected-error{{passing value of type 'Int' to an inout parameter requires explicit '&'}}{{13-13=&}}

  var array = [1,2,3]
  increment(array[1]) // expected-error{{passing value of type 'Int' to an inout parameter requires explicit '&'}}{{13-13=&}}
}

func maybeFn() -> (Int -> Int)? { }

func extraCall() {
  var i = 7
  i = i() // expected-error{{invalid use of '()' to call a value of non-function type 'Int'}}{{8-10=}}

  maybeFn()(5) // expected-error{{value of optional type '(Int -> Int)?' not unwrapped; did you mean to use '!' or '?'?}}{{12-12=!}}
}

class U {
    var prop1 = 0
}

class T {
    func m1() {
        // FIXME: should apply nullary function fixit here. {{function produces expected type 'U'; did you mean to call it with '()'?}}
        // <rdar://problem/17741575>
        let l = self.m2!.prop1 // expected-error{{operand of postfix '!' should have optional type; type is '() -> U!'}}
    }

    func m2() -> U! {
      return U()
    }
}

// Used an optional in a conditional expression
class C {
  var a: Int = 1
}
var co: C? = nil
var ciuo: C! = nil 

if co {} // expected-error{{optional type '@lvalue C?' cannot be used as a boolean; test for '!= nil' instead}}{{4-4=(}} {{6-6= != nil)}}
if ciuo {} // expected-error{{optional type '@lvalue C!' cannot be used as a boolean; test for '!= nil' instead}}{{4-4=(}} {{8-8= != nil)}}
co ? true : false // expected-error{{optional type '@lvalue C?' cannot be used as a boolean; test for '!= nil' instead}}{{1-1=(}} {{3-3= != nil)}}
!co ? false : true // expected-error{{optional type '@lvalue C?' cannot be used as a boolean; test for '!= nil' instead}}{{2-2=(}} {{4-4= != nil)}}
ciuo ? true : false // expected-error{{optional type '@lvalue C!' cannot be used as a boolean; test for '!= nil' instead}}{{1-1=(}} {{5-5= != nil)}}
!ciuo ? false : true // expected-error{{optional type '@lvalue C!' cannot be used as a boolean; test for '!= nil' instead}}{{2-2=(}} {{6-6= != nil)}}
!co // expected-error{{optional type '@lvalue C?' cannot be used as a boolean; test for '!= nil' instead}}{{2-2=(}} {{4-4= != nil)}}
!ciuo // expected-error{{optional type '@lvalue C!' cannot be used as a boolean; test for '!= nil' instead}}{{2-2=(}} {{6-6= != nil)}}

// Forgotten ! or ?
var someInt = co.a // expected-error{{value of optional type 'C?' not unwrapped; did you mean to use '!' or '?'?}}
