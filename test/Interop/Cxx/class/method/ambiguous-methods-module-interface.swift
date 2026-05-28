// RUN: %target-swift-ide-test -print-module -module-to-print=AmbiguousMethods -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK: func increment(_ a: CInt) -> CInt
// CHECK: mutating func incrementMutating(_ a: CInt) -> CInt

// CHECK: mutating func incrementMutating(_ a: CInt, _ b: CInt, _ c: inout CInt)
// CHECK: func increment(_ a: CInt, _ b: CInt, _ c: inout CInt)

// CHECK: mutating func incrementMutating(_ a: inout CInt, _ b: CInt)
// CHECK: func increment(_ a: inout CInt, _ b: CInt)

// CHECK: func numberOfMutableMethodsCalled() -> CInt
// CHECK: mutating func numberOfMutableMethodsCalledMutating() -> CInt

// CHECK: struct HasAmbiguousMethods2
// CHECK: func increment(_ a: CInt) -> CInt
// CHECK-NOT: mutating func incrementMutating(_ a: CInt) -> CInt

// CHECK: struct HasAmbiguousUnsafeMethods {
// CHECK:   func __getUnsafeUnsafe() -> Unsafe
// CHECK:   mutating func __getUnsafeMutatingUnsafe() -> Unsafe
// CHECK: }
