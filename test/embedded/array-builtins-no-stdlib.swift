// RUN: %target-swift-emit-ir %s -parse-stdlib -module-name Swift -enable-experimental-feature Embedded -target arm64e-apple-none | %FileCheck %s

class MyClass {}

struct MyStruct {
  var c: MyClass
}

public func foo(x: Builtin.RawPointer, y: Builtin.RawPointer, count: Builtin.Word) {
  Builtin.copyArray(MyStruct.self, x, y, count)
  Builtin.copyArray(MyStruct.self, x, y, count)
  Builtin.takeArrayNoAlias(MyStruct.self, x, y, count)
  Builtin.takeArrayFrontToBack(MyStruct.self, x, y, count)
  Builtin.takeArrayBackToFront(MyStruct.self, x, y, count)
  Builtin.assignCopyArrayNoAlias(MyStruct.self, x, y, count)
  Builtin.assignCopyArrayFrontToBack(MyStruct.self, x, y, count)
  Builtin.assignCopyArrayBackToFront(MyStruct.self, x, y, count)
  Builtin.assignTakeArray(MyStruct.self, x, y, count)
  Builtin.destroyArray(MyStruct.self, x, count)
}

// No runtime calls should be present.
// CHECK-NOT: @swift_arrayInitWithCopy
// CHECK-NOT: @swift_arrayInitWithTakeNoAlias
// CHECK-NOT: @swift_arrayInitWithTakeFrontToBack
// CHECK-NOT: @swift_arrayInitWithTakeBackToFront
// CHECK-NOT: @swift_arrayAssignWithCopyNoAlias
// CHECK-NOT: @swift_arrayAssignWithCopyFrontToBack
// CHECK-NOT: @swift_arrayAssignWithCopyBackToFront
// CHECK-NOT: @swift_arrayAssignWithTake
// CHECK-NOT: @swift_arrayDestroy
