// REQUIRES: asserts
// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)

// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/../Macros/Inputs/syntax_macro_definitions.swift -no-toolchain-stdlib-rpath -swift-version 5

// RUN: %target-swift-emit-module-interface(%t/Macros.swiftinterface) -enable-experimental-feature ExtensionMacros -module-name Macros %s -load-plugin-library %t/%target-library-name(MacroDefinition)
// RUN: %FileCheck %s < %t/Macros.swiftinterface --check-prefix CHECK
// RUN: %target-swift-frontend -compile-module-from-interface %t/Macros.swiftinterface -o %t/Macros.swiftmodule

// CHECK: #if compiler(>=5.3) && $Macros && $FreestandingExpressionMacros
// CHECK-NEXT: @freestanding(expression) public macro publicStringify<T>(_ value: T) -> (T, Swift.String) = #externalMacro(module: "MacroDefinition", type: "StringifyMacro")
// CHECK-NEXT: #endif
@freestanding(expression) public macro publicStringify<T>(_ value: T) -> (T, String) = #externalMacro(module: "MacroDefinition", type: "StringifyMacro")

// CHECK: #if compiler(>=5.3) && $Macros && $FreestandingExpressionMacros
// CHECK: @freestanding(expression) public macro labeledStringify<T>(_ value: T, label: Swift.String) -> (T, Swift.String) = #externalMacro(module: "MacroDefinition", type: "StringifyMacro")
// CHECK-NEXT: #endif
@freestanding(expression) public macro labeledStringify<T>(_ value: T, label: String) -> (T, String) = #externalMacro(module: "MacroDefinition", type: "StringifyMacro")

// CHECK: #if compiler(>=5.3) && $Macros && $FreestandingExpressionMacros
// CHECK: @freestanding(expression) public macro unlabeledStringify<T>(_ value: T, label: Swift.String) -> (T, Swift.String) = #labeledStringify(value, label: "default label")
// CHECK-NEXT: #endif
@freestanding(expression) public macro unlabeledStringify<T>(_ value: T, label: String) -> (T, String) = #labeledStringify(value, label: "default label")

// CHECK: #if compiler(>=5.3) && $Macros && $FreestandingExpressionMacros
// CHECK: @freestanding(expression) public macro publicLine<T>() -> T = #externalMacro(module: "MacroDefinition", type: "Line") where T : Swift.ExpressibleByIntegerLiteral
// CHECK-NEXT: #endif
@freestanding(expression) public macro publicLine<T: ExpressibleByIntegerLiteral>() -> T = #externalMacro(module: "MacroDefinition", type: "Line")

// CHECK: #if compiler(>=5.3) && $Macros
// CHECK: @attached(accessor) public macro myWrapper() = #externalMacro(module: "MacroDefinition", type: "Wrapper")
// CHECK-NEXT: #endif
@attached(accessor) public macro myWrapper() = #externalMacro(module: "MacroDefinition", type: "Wrapper")

// CHECK: #if compiler(>=5.3) && $Macros && $AttachedMacros
// CHECK: @attached(member, names: named(init), prefixed(`$`)) public macro MemberwiseInit() = #externalMacro(module: "MacroDefinition", type: "MemberwiseInitMacro")
// CHECK-NEXT: #endif
@attached(member, names: named(init), prefixed(`$`)) public macro MemberwiseInit() = #externalMacro(module: "MacroDefinition", type: "MemberwiseInitMacro")

// CHECK: #if compiler(>=5.3) && $Macros && $AttachedMacros
// CHECK: @attached(member, names: named(`init`), prefixed(`$`)) public macro MemberwiseInitFunc() = #externalMacro(module: "MacroDefinition", type: "MemberwiseInitFuncMacro")
// CHECK-NEXT: #endif
@attached(member, names: named(`init`), prefixed(`$`)) public macro MemberwiseInitFunc() = #externalMacro(module: "MacroDefinition", type: "MemberwiseInitFuncMacro")

// CHECK: #if compiler(>=5.3) && $Macros && $AttachedMacros
// CHECK: @attached(accessor, names: named(init)) public macro AccessorInitFunc() = #externalMacro(module: "MacroDefinition", type: "AccessorInitFuncMacro")
// CHECK-NEXT: #endif
@attached(accessor, names: named(init)) public macro AccessorInitFunc() = #externalMacro(module: "MacroDefinition", type: "AccessorInitFuncMacro")

// CHECK: #if compiler(>=5.3) && $Macros && $AttachedMacros
// CHECK: @attached(extension, conformances: Swift.Sendable) @attached(member) public macro AddSendable() = #externalMacro(module: "MacroDefinition", type: "SendableExtensionMacro")
// CHECK-NEXT: #else
// CHECK: @attached(member) public macro AddSendable() = #externalMacro(module: "MacroDefinition", type: "SendableExtensionMacro")
// CHECK-NEXT: #endif
@attached(extension, conformances: Sendable) @attached(member) public macro AddSendable() = #externalMacro(module: "MacroDefinition", type: "SendableExtensionMacro")

// CHECK-NOT: internalStringify
@freestanding(expression) macro internalStringify<T>(_ value: T) -> (T, String) = #externalMacro(module: "MacroDefinition", type: "StringifyMacro")

@freestanding(declaration, names: named(StructWithUnqualifiedLookup))
macro structWithUnqualifiedLookup() = #externalMacro(module: "MacroDefinition", type: "DefineStructWithUnqualifiedLookupMacro")

let world = 17

public
#structWithUnqualifiedLookup
// CHECK-NOT: structWithUnqualifiedLookup
// CHECK-NOT: struct StructWithUnqualifiedLookup
// CHECK: struct StructWithUnqualifiedLookup
// CHECK-NOT: struct StructWithUnqualifiedLookup

@attached(peer, names: named(_foo))
macro AddPeerStoredProperty() = #externalMacro(module: "MacroDefinition", type: "AddPeerStoredPropertyMacro")

@AddPeerStoredProperty
public var test: Int = 10
// CHECK: var test
// CHECK-NOT: var _foo
// CHECK: var _foo
// CHECK-NOT: var _foo

// CHECK: struct TestStruct {
public struct TestStruct {
  public #structWithUnqualifiedLookup
  // CHECK-NOT: structWithUnqualifiedLookup
  // CHECK-NOT: struct StructWithUnqualifiedLookup
  // CHECK: struct StructWithUnqualifiedLookup
  // CHECK-NOT: struct StructWithUnqualifiedLookup

  @AddPeerStoredProperty
  public var test: Int = 10
  // CHECK: var test
  // CHECK-NOT: var _foo
  // CHECK: var _foo
  // CHECK-NOT: var _foo
}
