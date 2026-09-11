// RUN: %target-swift-ide-test -print-module -module-to-print=FunctionTemplateWithOptionalFrt -I %S/Inputs -source-filename=x -cxx-interoperability-mode=default | %FileCheck %s

// CHECK: func passThrough<T>(_ value: T) -> T
// CHECK: func cast<R, I>(_ i: I) -> R
// CHECK: func dynamicCast<BasePtr, DerivedPtr>(_ x: BasePtr) -> DerivedPtr
// CHECK: func downcast<BasePtr, DerivedPtr>(_ x: BasePtr) -> DerivedPtr
// CHECK: func nullableDowncast<BasePtr, DerivedPtr>(_ x: BasePtr) -> DerivedPtr?
