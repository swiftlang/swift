// RUN: %target-run-simple-swift | FileCheck %s

class Foo<T:ReplPrintable> {
  init<U:ReplPrintable>(_ t:T, _ u:U) {
    print("init ")
    t.replPrint()
    print(" ")
    u.replPrint()
    println("")
  }

  func bar<U : ReplPrintable>(u: U) {
    print("bar ")
    u.replPrint()
    println("")
  }
}


// CHECK: init 1 "two"
var foo = Foo<Int>(1, "two")
// CHECK: bar "3"
var c = "3"
foo.bar(c)
