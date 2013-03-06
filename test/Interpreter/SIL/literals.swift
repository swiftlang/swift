// RUN: %swift -sil-i %s | FileCheck %s

func literals() {
  // CHECK: 0
  println(0)
  // CHECK: 1.5
  println(1.5)
  // CHECK: x
  println('x')
  // CHECK: xxx
  println("xxx")
}
literals()

// FIXME: this abuses zero initialization and will break when we have
// definitive assignment/default ctor initialization
func zero_inits() {
  var x:Int
  var y:Float
  var z:Char
  var w:(Int, Char, Float)

  struct Foo { var x, y: Int }
  var v:Foo
}
