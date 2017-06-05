// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s

func markUsed<T>(_ t: T) {}

class Class {
// CHECK: tag: DW_TAG_structure_type, name: "_T05atype5ClassC8functionyxlFQq_D"
  func function<T>(_ x: T) {
    markUsed("hello world")
  }
}

func main() {
  var v = 1
  var c = Class()
  c.function(1)
}

main()
