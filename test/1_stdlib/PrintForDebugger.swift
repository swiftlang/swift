// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test

struct StructWithMembers {
  var a = 1
  var b = "Hello World"
}

print(_PrintForDebugger.printForDebugger(value: StructWithMembers()))
// CHECK: StructWithMembers
// CHECK: a : 1
// CHECK: b : "Hello World"

class ClassWithMembers {
  var a = 1
  var b = "Hello World"
}

print(_PrintForDebugger.printForDebugger(value: ClassWithMembers()))
// CHECK: <ClassWithMembers: 0x

class ClassWithMirror: CustomReflectable {
  var customMirror: Mirror {
    return Mirror(self, children: ["a" : 1, "b" : "Hello World"])
  }
}

print(_PrintForDebugger.printForDebugger(value: ClassWithMirror()))
// CHECK: ClassWithMirror
// CHECK: a : 1
// CHECK: b : "Hello World"

print(_PrintForDebugger.printForDebugger(value: [1,2,3,4]))
// CHECK: 4 elements
// CHECK: - 0 : 1
// CHECK: - 1 : 2
// CHECK: - 2 : 3
// CHECK: - 3 : 4

print(_PrintForDebugger.printForDebugger(value: [1:2, 3:4]))
// CHECK: 2 elements
// CHECK: 0 : 2 elements
// CHECK: 1 : 2 elements

print(_PrintForDebugger.printForDebugger(value: nil as Int?))
// CHECK: nil

print(_PrintForDebugger.printForDebugger(value: 3 as Int?))
// CHECK: Optional<Int>
// CHECK: - some : 3

