// RUN: %swift %s -verify

class A {
  func foo() { }
}

class B : A {
  func bar() { }
}

class Other { }

func acceptA(a: A) { }

func f0<T : A>(obji: T, ai: A, bi: B) {
  var obj = obji, a = ai, b = bi
  // Method access
  obj.foo()
  obj.bar() // expected-error{{}}

  // Calls
  acceptA(obj)

  // Derived-to-base conversion for assignment
  a = obj

  // Invalid assignments
  obj = a // expected-error{{'A' is not convertible to 'T'}}
  obj = b // expected-error{{'B' is not convertible to 'T'}}

  // Downcast that is actually a coercion
  a = (obj as A)! // expected-error{{operand of postfix '!' should have optional type; type is 'A'}}
  a = obj as A

  // Downcasts
  b = (obj as B)!
}

func call_f0(a: A, b: B, other: Other) {
  f0(a, a, b)
  f0(b, a, b)
  f0(other, a, b) // expected-error{{}}
}

// Declaration errors
func f1<T : A where T : Other>() { } // expected-error{{generic parameter 'T' cannot be a subclass of both 'A' and 'Other'}}
func f2<T : A where T : B>() { } // FIXME: expected-error{{cannot be a subclass}}

class X<T> {
  func f() -> T {}
}

class Y<T> : X<T[]> {
}

func testGenericInherit() {
  var yi : Y<Int>
  var ia : Int[] = yi.f()
}


struct SS<T> : T { } // expected-error{{inheritance from non-protocol type 'T'}}
enum SE<T> : T { case X } // expected-error{{raw type 'T' is not convertible from any literal}} expected-error{{enum cases require explicit raw values when the raw type is not integer literal convertible}}

// Also need Equatable for fromRaw()
enum SE2<T : IntegerLiteralConvertible> 
  : T // expected-error{{'fromRaw' method cannot be synthesized because raw type 'T' is not Equatable}}
{ case X }

// ... but not if fromRaw() is directly implemented some other way.
enum SE3<T : IntegerLiteralConvertible> : T { 
  case X 

  static func fromRaw(raw: T) -> SE3? {
    return SE3.X
  }
}

enum SE4<T : protocol<IntegerLiteralConvertible,Equatable> > : T { case X }
