// RUN: %target-parse-verify-swift

class A { }
class B : A { }
class C : B { }
class D : B { }

class E<T> : D { }
class F<T> : E<[T]> { }

var a : A
var b : B
var c : C
var d : D
var ef : E<Float> 
var fi : F<Int>

func f0(b : B) {}

func ternary<T>(cond: Bool,
                @autoclosure _ ifTrue: () -> T,
                @autoclosure _ ifFalse: () -> T) -> T {}

f0(c)
f0(a) // expected-error{{cannot invoke 'f0' with an argument list of type '(A)'}} expected-note {{expected an argument list of type '(B)'}}
f0(ef)
f0(fi)

// FIXME: Test subtyping of class metatypes.

ternary(true, ef, c)


class X {
  init() {}
  init(x:Int, y:UnicodeScalar) {}
}

var x0 = X()
var x1 = X(x: 1, y: "2")

