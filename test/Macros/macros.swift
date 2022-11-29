// RUN: %target-swift-frontend -enable-experimental-feature Macros -dump-ast %s -module-name MacrosTest 2>&1 | %FileCheck %s
// REQUIRES: OS=macosx

macro stringify<T>(_ value: T) -> (T, String) = _SwiftSyntaxMacros.StringifyMacro

func test(a: Int, b: Int) {
  let s = #stringify(a + b)

  // CHECK: macro_expansion_expr type='(Int, String)'{{.*}}name=stringify
  // CHECK-NEXT: argument_list
  // CHECK: tuple_expr type='(Int, String)' location=Macro expansion of #stringify

  let (b, s2) = #stringify({ () -> Bool in return true })
  // CHECK: macro_expansion_expr type='(() -> Bool, String)'{{.*}}name=stringify
  // CHECK-NEXT: argument_list
  // CHECK: tuple_expr type='(() -> Bool, String)' location=Macro expansion of #stringify

  let (b2, s3) = #stringify<Double>(1 + 2)
  // CHECK: macro_expansion_expr type='(Double, String)'{{.*}}name=stringify
  // CHECK-NEXT: argument_list
  // CHECK: tuple_expr type='(Double, String)' location=Macro expansion of #stringify
}
