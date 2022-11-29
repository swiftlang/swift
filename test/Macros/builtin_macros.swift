// RUN: %target-swift-frontend -enable-experimental-feature Macros -enable-experimental-feature BuiltinMacros -dump-ast %s -module-name MacrosTest 2>&1 | %FileCheck %s
// REQUIRES: OS=macosx

macro function<T>: T = _SwiftSyntaxMacros.FunctionMacro where T: ExpressibleByStringLiteral
macro line<T: ExpressibleByIntegerLiteral>: T = _SwiftSyntaxMacros.LineMacro
macro column<T: ExpressibleByIntegerLiteral>: T = _SwiftSyntaxMacros.ColumnMacro

// CHECK: macro_expansion_expr implicit type='String'
// CHECK-NEXT: string_literal_expr{{.*}}Macro expansion of #function in{{.*}}value="MacrosTest"
print(#function)

func f(a: Int, b: Int) {
  print(#function, #line, #column)
  // CHECK: macro_expansion_expr implicit type='String'
  // CHECK-NEXT: string_literal_expr{{.*}}Macro expansion of #function in{{.*}}value="f(a:b:)"

  // CHECK: macro_expansion_expr implicit type='Int'
  // CHECK-NEXT: integer_literal_expr{{.*}}Macro expansion of #line in{{.*}}value=13

  // CHECK: macro_expansion_expr implicit type='Int'
  // CHECK-NEXT: integer_literal_expr{{.*}}Macro expansion of #column in{{.*}}value=27
}
