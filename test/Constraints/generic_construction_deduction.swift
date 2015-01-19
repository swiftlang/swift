// RUN: %target-parse-verify-swift

struct A<T> {
  // Can deduce from this constructor
  init(x:T) { }

  // Can't from this one
  init(x:Int, y:Int) { }

  static func bort(x: T) -> T { return x }
}

var a = A(x: 0)
var a1 : A<Int> = a

var b = A(x: "zero")
var b1 : A<String> = b

class C<T> {
  init(x:T) { }
}

var c = C(x: 0)
var c1 : C<Int> = c

var d = C(x: "zero")
var d1 : C<String> = d

var x : Int = A.bort(0)
var y : String = A.bort("zero")

func foo(a: A<String>) { }
// Deduce A<String> from context
foo(A(x: 0, y: 0))

// Specifying only some of the generic arguments.
struct B { }

struct X<T,U> {
  init(a:U) {}
}

var q = X<B,Int>(a: x)

