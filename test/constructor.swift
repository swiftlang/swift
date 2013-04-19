// RUN: %swift -parse %s -verify

struct X {
  constructor() {}
}

X()

struct Y {
  var i : Int, f : Float
  constructor(i : Int, f : Float) {}
}

Y(1, 1.5)

struct Z {
  var a : Int
  var b : Int

  constructor (a : Int, b : Int = 5) {
    this.a = a
    this.b = b
  }
}

Z(1, 2)
