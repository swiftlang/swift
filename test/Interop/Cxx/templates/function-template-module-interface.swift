// RUN: %target-swift-ide-test -print-module -module-to-print=FunctionTemplates -I %S/Inputs -source-filename=x -enable-cxx-interop | %FileCheck %s

// CHECK: func add<T>(_ a: T, _ b: T) -> T
// CHECK: func addTwoTemplates<A, B>(_ a: A, _ b: B) -> A
// CHECK: func passThrough<T>(_ value: T) -> T
// CHECK: func passThroughConst<T>(_ value: T) -> T
// CHECK: func returns_template<R, T, U>(_ a: T, _ b: U) -> R
// CHECK: func cannot_infer_template<T>()

// CHECK: struct HasVariadicMemeber {
// CHECK:   @available(*, unavailable, message: "Variadic function is unavailable")
// CHECK:   mutating func test1(_ varargs: Any...)
// CHECK:   @available(*, unavailable, message: "Variadic function is unavailable")
// CHECK:   mutating func test2(_: Int32, _ varargs: Any...)
// CHECK: }

// CHECK: func lvalueReference<T>(_ ref: UnsafeMutablePointer<T>)
// CHECK: func constLvalueReference<T>(_: UnsafePointer<T>)
// CHECK: func forwardingReference<T>(_: UnsafeMutablePointer<T>)

// CHECK: enum Orbiters {
// CHECK:   static func galileo<T>(_: T)
// CHECK:   static func cassini<T, U>(_: T, _: U)
// CHECK:   static func magellan<T>(_: UnsafeMutablePointer<T>)
// CHECK: }
