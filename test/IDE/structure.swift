// RUN: %swift-ide-test -structure -source-filename %s | FileCheck %s

// CHECK: Class at 4:1 - 11:1
class MyCls {
  
  //CHECK: Func at 7:3 - 9:3
  func foo() {
    var abc
  }

}
