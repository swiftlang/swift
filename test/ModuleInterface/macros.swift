// REQUIRES: asserts
// REQUIRES: swift_swift_parser
// FIXME: Swift parser is not enabled on Linux CI yet.
// REQUIRES: OS=macosx

// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -typecheck -module-name Macros -emit-module-interface-path %t/Macros.swiftinterface %s
// RUN: %FileCheck %s < %t/Macros.swiftinterface --check-prefix CHECK
// RUN: %target-swift-frontend -compile-module-from-interface %t/Macros.swiftinterface -o %t/Macros.swiftmodule

// CHECK: #if compiler(>=5.3) && $Macros
// CHECK-NEXT: #if $FreestandingExpressionMacros
// CHECK-NEXT: @freestanding(expression) public macro publicStringify<T>(_ value: T) -> (T, Swift.String) = #externalMacro(module: "SomeModule", type: "StringifyMacro")
// CHECK-NEXT: #else
// CHECK-NEXT: @expression public macro publicStringify<T>(_ value: T) -> (T, Swift.String) = #externalMacro(module: "SomeModule", type: "StringifyMacro")
// CHECK-NEXT: #endif
// CHECK-NEXT: #endif
@freestanding(expression) public macro publicStringify<T>(_ value: T) -> (T, String) = #externalMacro(module: "SomeModule", type: "StringifyMacro")

@freestanding(expression) public macro labeledStringify<T>(_ value: T, label: String) -> (T, String) = #externalMacro(module: "SomeModule", type: "StringifyMacro")

// CHECK: @freestanding(expression) public macro unlabeledStringify<T>(_ value: T, label: Swift.String) -> (T, Swift.String) = #labeledStringify(value, label: "default label")
@freestanding(expression) public macro unlabeledStringify<T>(_ value: T, label: String) -> (T, String) = #labeledStringify(value, label: "default label")

// CHECK: #if compiler(>=5.3) && $Macros
// CHECK-NEXT: #if $FreestandingExpressionMacros
// CHECK: @freestanding(expression) public macro publicLine<T>() -> T = #externalMacro(module: "SomeModule", type: "Line") where T : Swift.ExpressibleByIntegerLiteral
// CHECK-NEXT: #else
// CHECK: @expression public macro publicLine<T>() -> T = #externalMacro(module: "SomeModule", type: "Line") where T : Swift.ExpressibleByIntegerLiteral
// CHECK-NEXT: #endif
// CHECK-NEXT: #endif
@freestanding(expression) public macro publicLine<T: ExpressibleByIntegerLiteral>() -> T = #externalMacro(module: "SomeModule", type: "Line")

// CHECK: #if compiler(>=5.3) && $Macros
// CHECK: @attached(accessor) public macro myWrapper() -> () = #externalMacro(module: "SomeModule", type: "Wrapper")
// CHECK-NEXT: #endif
@attached(accessor) public macro myWrapper() = #externalMacro(module: "SomeModule", type: "Wrapper")

// CHECK: #if compiler(>=5.3) && $Macros && $AttachedMacros
// CHECK: @attached(member, names: named(init), prefixed(`$`)) public macro MemberwiseInit() -> () = #externalMacro(module: "SomeModule", type: "MemberwiseInitMacro")
// CHECK-NEXT: #endif
@attached(member, names: named(init), prefixed(`$`)) public macro MemberwiseInit() -> () = #externalMacro(module: "SomeModule", type: "MemberwiseInitMacro")

// CHECK-NOT: internalStringify
@freestanding(expression) macro internalStringify<T>(_ value: T) -> (T, String) = #externalMacro(module: "SomeModule", type: "StringifyMacro")
