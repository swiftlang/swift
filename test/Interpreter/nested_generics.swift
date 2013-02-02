// RUN: %swift -i %s | FileCheck %s

class Foo<T:ReplPrintable> {
  constructor<U:ReplPrintable>(t:T, u:U) {
    print("constructor ")
    t.replPrint()
    print(" ")
    u.replPrint()
    println("")
  }

  func bar<U:ReplPrintable>(u:U) {
    print("bar ")
    u.replPrint()
    println("")
  }
}

typealias FooInt = Foo<Int>
// CHECK: constructor 1 "two"
var foo = new FooInt(1, "two")
// CHECK: bar '3'
foo.bar('3')
