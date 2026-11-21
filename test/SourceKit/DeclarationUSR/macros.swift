// RUN: %sourcekitd-test -req=collect-decl-usr %s -- %s | %FileCheck %s

@freestanding(expression)
macro makeValue() -> Int = #externalMacro(module: "MacroDefinition", type: "MakeValueMacro")

func useMacroDeclaration() {}

// CHECK: key.kind: source.lang.swift.decl.macro
// CHECK-NEXT: key.usr:
// CHECK-SAME: makeValue
// CHECK: key.kind: source.lang.swift.decl.function.free
// CHECK-NEXT: key.usr:
// CHECK-SAME: useMacroDeclaration
