// RUN: %target-swift-ide-test -print-module -module-to-print=FunctionTemplateNullability -I %S/Inputs -source-filename=x -cxx-interoperability-mode=default | %FileCheck %s

// CHECK: func nullableResult<T>(_ x: T) -> T?
// CHECK: func nonnullResult<T>(_ x: T) -> T
// CHECK: func unspecifiedResult<T>(_ x: T) -> T!
// CHECK: func nullableParameter<T>(_ x: T?) -> T?
// CHECK: func nullablePointerResult<T>(_ x: UnsafeMutablePointer<T>) -> UnsafeMutablePointer<T>?
// CHECK: func nullableConstPointerResult<T>(_ x: UnsafePointer<T>) -> UnsafePointer<T>?
// CHECK: func nonnullPointerResult<T>(_ x: UnsafeMutablePointer<T>) -> UnsafeMutablePointer<T>
