// RUN: %target-swift-ide-test -print-module -module-to-print=FunctionTemplates -I %S/Inputs -source-filename=x -enable-cxx-interop | %FileCheck %s

// CHECK: func add<T>(_ a: T, _ b: T) -> T
// CHECK: func addTwoTemplates<A, B>(_ a: A, _ b: B) -> A
// CHECK: func passThrough<T>(_ value: T) -> T
// CHECK: func passThroughConst<T>(_ value: T) -> T
// CHECK: func returns_template<R, T, U>(_ a: T, _ b: U) -> R
// CHECK: func cannot_infer_template<T>()
