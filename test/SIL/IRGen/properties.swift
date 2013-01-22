// RUN: %swift -sil-i %s | FileCheck %s

var foo : Int {
get:
  println("foo gotten")
  return 219
set:
  println("foo set")
}

struct Bar {
  var bar : Int {
  get:
    println("bar gotten")
    return 20721
  set:
    println("bar set")
  }
}

func test() {
  var b = Bar()
  // CHECK: foo gotten
  // CHECK: 219
  println(foo)
  // CHECK: foo set
  foo = 1

  // CHECK: bar gotten
  // CHECK: 20721
  println(b.bar)
  // CHECK: bar set
  b.bar = 1
}
test()
