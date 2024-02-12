// REQUIRES: swift_swift_parser
// RUN: %empty-directory(%t.ranges)
// RUN: %refactor -find-rename-ranges -source-filename %s -pos="test" -is-function-like -old-name "StringifyMacro" | %FileCheck %s

// CHECK: struct /*test:def*/<base>StringifyMacro</base> {}
// CHECK: @freestanding(expression)
// CHECK: macro stringify<T>(_ value: T) -> (T, String) = #externalMacro(module: "MyMacroMacros", type: "/*test:ref*/<base>StringifyMacro</base>")

struct /*test:def*/StringifyMacro {}

@freestanding(expression)
macro stringify<T>(_ value: T) -> (T, String) = #externalMacro(module: "MyMacroMacros", type: "/*test:ref*/StringifyMacro")
