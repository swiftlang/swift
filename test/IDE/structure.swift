// RUN: %swift-ide-test -structure -source-filename %s | FileCheck %s

// CHECK: Class at [[@LINE+1]]:1 - [[@LINE+22]]:2, name at [[@LINE+1]]:7 - [[@LINE+1]]:12, inherited types at [[@LINE+1]]:15 - [[@LINE+1]]:25
class MyCls : OtherClass {
  // CHECK: Property at [[@LINE+1]]:3 - [[@LINE+1]]:16, name at [[@LINE+1]]:7 - [[@LINE+1]]:10
  var bar : Int

  // CHECK: Property at [[@LINE+1]]:3 - [[@LINE+1]]:28, name at [[@LINE+1]]:7 - [[@LINE+1]]:17
  var anotherBar : Int = 42

  // CHECK: Func at [[@LINE+3]]:3 - [[@LINE+12]]:4, name at [[@LINE+3]]:8 - [[@LINE+3]]:36
  // CHECK: Parameter at [[@LINE+2]]:12 - [[@LINE+2]]:16
  // CHECK: Parameter at [[@LINE+1]]:23 - [[@LINE+1]]:27, name at [[@LINE+1]]:23 - [[@LINE+1]]:27
  func foo(arg1: Int, name: String) {
    var abc
    // CHECK: Brace at [[@LINE+1]]:10 - [[@LINE+6]]:6
    if 1 {
      // CHECK: Call at [[@LINE+3]]:7 - [[@LINE+3]]:26, name at [[@LINE+3]]:7 - [[@LINE+3]]:10
      // CHECK: Parameter at [[@LINE+2]]:11 - [[@LINE+2]]:12
      // CHECK: Parameter at [[@LINE+1]]:14 - [[@LINE+1]]:19, name at [[@LINE+1]]:14 - [[@LINE+1]]:18
      foo(1, name:"test")
    }
  }

}
