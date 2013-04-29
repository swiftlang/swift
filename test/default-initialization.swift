// RUN: %swift %s -verify

struct A {
  var i : Int
  constructor(i : Int) { this.i = i }
}

struct B {
  var a : A
}

func locals() {
  var al : A // expected-error{{cannot default-initialize variable of type 'A'}}
  var bl : B // expected-error{{cannot default-initialize variable of type 'B'}}
}

var ag : A // expected-error{{cannot default-initialize variable of type 'A'}}
var bg : B // expected-error{{cannot default-initialize variable of type 'B'}}

struct C {
  var x : (Int, Int)
}

var c : C


