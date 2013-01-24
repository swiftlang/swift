// RUN: %swift -i %s | FileCheck %s
// XFAIL: *

class B {
  var x : Int
  constructor() {
    x = 22
    println("I'm Henry VIII I am")
  }
}

class D : B {
  var y : Int
  constructor() {
    super.constructor()
    y = 44
    println("Second verse same as the first")
  }
}

// CHECK: I'm Henry VIII I am
// CHECK: Second verse same as the first
var d = new D()

// CHECK: 22
println(d.x)
// CHECK: 44
println(d.y)
