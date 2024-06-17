// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name MacroNameCollision -clang-header-expose-decls=all-public -emit-clang-header-path %t/macros.h
// RUN: %FileCheck %s < %t/macros.h

// RUN: %check-interop-cxx-header-in-clang(%t/macros.h)

// CHECK-LABEL: namespace MacroNameCollision SWIFT_PRIVATE_ATTR SWIFT_SYMBOL_MODULE("MacroNameCollision") {

@freestanding(expression)
public macro myLogMacro(error: String) = #externalMacro(module: "CompilerPlugin", type: "LogMacro")

@freestanding(expression)
public macro myLogMacro(fault: String) = #externalMacro(module: "CompilerPlugin", type: "LogMacro")

// CHECK: // Unavailable in C++: Swift macro 'myLogMacro(error:)'
// CHECK: // Unavailable in C++: Swift macro 'myLogMacro(fault:)'
