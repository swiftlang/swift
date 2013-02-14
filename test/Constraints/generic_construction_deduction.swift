// RUN: %swift -constraint-checker -parse %s

struct A<T> {
  constructor(x:T) { }

  static func bort(x:T) -> T { return x }
}

var a = A(0)
var a1 : A<Int> = a

var b = A("zero")
var b1 : A<String> = b

class C<T> {
  constructor(x:T) { }
}

var c = new C(0)
var c1 : C<Int> = c

var d = new C("zero")
var d1 : C<String> = d

var x : Int = A.bort(0)
var y : String = A.bort("zero")
