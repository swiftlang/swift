// RUN: %target-swift-frontend -enable-experimental-feature BuiltinMacros -dump-ast %s -module-name MacrosTest 2>&1 | %FileCheck %s
// REQUIRES: OS=macosx
// REQUIRES: asserts


// CHECK: macro_expansion_expr implicit type='String'
// CHECK-NEXT: string_literal_expr{{.*}}Macro expansion of #function in{{.*}}value="MacrosTest"
print(#function)

func f(a: Int, b: Int) {
  print(#function, #line, #column)
  // CHECK: macro_expansion_expr implicit type='String'
  // CHECK-NEXT: string_literal_expr{{.*}}Macro expansion of #function in{{.*}}value="f(a:b:)"

  // CHECK: macro_expansion_expr implicit type='Int'
  // CHECK-NEXT: integer_literal_expr{{.*}}Macro expansion of #line in{{.*}}value=11

  // CHECK: macro_expansion_expr implicit type='Int'
  // CHECK-NEXT: integer_literal_expr{{.*}}Macro expansion of #column in{{.*}}value=27
}
