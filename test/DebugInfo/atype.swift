// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

func markUsed<T>(t: T) {}

class Class {
// CHECK: _TtQq_FC5atype5Class8function
  func function<T>(x: T) {
    markUsed("hello world")
  }
}

func main() {
  var v = 1
  var c = Class()
  c.function(1)
}

main()
