// RUN: %target-swift-ide-test -print-module -module-to-print=EnableIf -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// The `init<T>` constructor template is not yet supported in Swift.

// CHECK: struct HasConstructorWithEnableIf {
// CHECK-NEXT:  @available(*, deprecated, message: "This zero-initializes
// CHECK-NEXT:  init()
// CHECK-NEXT:}

// CHECK: struct HasConstructorWithEnableIfUsed {
// CHECK-NEXT:  @available(*, deprecated, message: "This zero-initializes
// CHECK-NEXT:  init()
// CHECK-NEXT:  init<T, U>(_: T, _: U)
