// RUN: %target-swift-emit-ir %s -parse-stdlib -module-name Swift -enable-experimental-feature Embedded -wmo -target arm64e-apple-none | %FileCheck %s
// RUN: %target-swift-emit-ir %s -parse-stdlib -module-name Swift -enable-experimental-feature Embedded -wmo -target arm64e-apple-none -Osize | %FileCheck %s
// RUN: %target-swift-emit-ir %s -parse-stdlib -module-name Swift -enable-experimental-feature Embedded -wmo -target arm64e-apple-none -O | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded

// TODO: these builtins in embedded Swift have a completely different IRGen, we need executable tests for them.

class MyClass {}

struct MyStruct {
  var c: MyClass
}

public func foo(x: Builtin.RawPointer, y: Builtin.RawPointer, count: Builtin.Word) {
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

public func bar(x: Builtin.RawPointer, y: Builtin.RawPointer, count: Builtin.Word) {
  var s = MyGenericStruct<MyStruct>()
  s.foo(x: x, y: y, count: count)
}

public struct MyGenericStruct<T> {
  public func foo(x: Builtin.RawPointer, y: Builtin.RawPointer, count: Builtin.Word) {
    Builtin.copyArray(T.self, x, y, count)
    Builtin.takeArrayNoAlias(T.self, x, y, count)
    Builtin.takeArrayFrontToBack(T.self, x, y, count)
    Builtin.takeArrayBackToFront(T.self, x, y, count)
    Builtin.assignCopyArrayNoAlias(T.self, x, y, count)
    Builtin.assignCopyArrayFrontToBack(T.self, x, y, count)
    Builtin.assignCopyArrayBackToFront(T.self, x, y, count)
    Builtin.assignTakeArray(T.self, x, y, count)
    Builtin.destroyArray(T.self, x, count)
  }
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
