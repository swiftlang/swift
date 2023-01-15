// REQUIRES: asserts

// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck -module-name Macros -emit-module-interface-path %t/Macros.swiftinterface -enable-experimental-feature Macros %s
// RUN: %FileCheck %s < %t/Macros.swiftinterface --check-prefix CHECK
// RUN: %target-swift-frontend -compile-module-from-interface %t/Macros.swiftinterface -o %t/Macros.swiftmodule

// CHECK: #if compiler(>=5.3) && $Macros
// CHECK-NEXT: @expression public macro publicStringify<T>(_ value: T) -> (T, Swift.String) = #externalMacro(module: "SomeModule", type: "StringifyMacro")
// CHECK-NEXT: #endif
@expression public macro publicStringify<T>(_ value: T) -> (T, String) = #externalMacro(module: "SomeModule", type: "StringifyMacro")

// CHECK: #if compiler(>=5.3) && $Macros
// CHECK: @expression public macro publicLine<T>: T = #externalMacro(module: "SomeModule", type: "Line") where T : Swift.ExpressibleByIntegerLiteral
// CHECK-NEXT: #endif
@expression public macro publicLine<T: ExpressibleByIntegerLiteral>: T = #externalMacro(module: "SomeModule", type: "Line")

// CHECK: #if compiler(>=5.3) && $Macros
// CHECK: @attached(accessor) public macro myWrapper: Swift.Void = #externalMacro(module: "SomeModule", type: "Wrapper")
// CHECK-NEXT: #endif
@attached(accessor) public macro myWrapper: Void = #externalMacro(module: "SomeModule", type: "Wrapper")

// CHECK-NOT: internalStringify
@expression macro internalStringify<T>(_ value: T) -> (T, String) = #externalMacro(module: "SomeModule", type: "StringifyMacro")
