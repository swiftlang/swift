// RUN: %empty-directory(%t)
// RUN: %target-build-swift -I %swift-host-lib-dir -L %swift-host-lib-dir -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath
// RUNx: %target-swift-frontend -dump-ast -enable-experimental-feature Macros -enable-experimental-feature Macros -load-plugin-library %t/%target-library-name(MacroDefinition) -I %swift-host-lib-dir %s -module-name MacroUser 2>&1 | %FileCheck --check-prefix CHECK-AST %s
// RUN: %target-typecheck-verify-swift -enable-experimental-feature Macros -load-plugin-library %t/%target-library-name(MacroDefinition) -I %swift-host-lib-dir -module-name MacroUser -DTEST_DIAGNOSTICS
// RUN: %target-build-swift -enable-experimental-feature Macros -enable-experimental-feature Macros -load-plugin-library %t/%target-library-name(MacroDefinition) -I %swift-host-lib-dir -L %swift-host-lib-dir %s -o %t/main -module-name MacroUser
// RUN: %target-run %t/main | %FileCheck %s
// REQUIRES: executable_test

// FIXME: Swift parser is not enabled on Linux CI yet.
// REQUIRES: OS=macosx

macro customFileID: String = MacroDefinition.FileIDMacro
macro stringify<T>(_ value: T) -> (T, String) = MacroDefinition.StringifyMacro
macro fileID<T: _ExpressibleByStringLitera>: T = MacroDefinition.FileIDMacro

func testFileID(a: Int, b: Int) {
  // CHECK: MacroUser/macro_expand.swift
  print("Result is \(#customFileID)")

  // CHECK: Builtin result is MacroUser/macro_expand.swift
  // CHECK-AST: macro_expansion_expr type='String'{{.*}}name=line
  print("Builtin result is \(#fileID)")
}

testFileID(a: 1, b: 2)

func testStringify(a: Int, b: Int) {
  let s = #stringify(a + b)
  print(s)

  // CHECK-AST: macro_expansion_expr type='(Int, String)'{{.*}}name=stringify
  // CHECK-AST-NEXT: argument_list
  // CHECK-AST: tuple_expr type='(Int, String)' location=Macro expansion of #stringify

  let (b, s2) = #stringify({ () -> Bool in return true })
  // CHECK-AST: macro_expansion_expr type='(() -> Bool, String)'{{.*}}name=stringify
  // CHECK-AST-NEXT: argument_list
  // CHECK-AST: tuple_expr type='(() -> Bool, String)' location=Macro expansion of #stringify

  let (b2, s3) = #stringify<Double>(1 + 2)
  // CHECK-AST: macro_expansion_expr type='(Double, String)'{{.*}}name=stringify
  // CHECK-AST-NEXT: argument_list
  // CHECK-AST: tuple_expr type='(Double, String)' location=Macro expansion of #stringify

  _ = (b, b2, s2, s3)
}

// CHECK: (2, "a + b")
testStringify(a: 1, b: 1)

macro addBlocker<T>(_ value: T) -> T = MacroDefinition.AddBlocker

struct OnlyAdds {
  static func +(lhs: OnlyAdds, rhs: OnlyAdds) -> OnlyAdds { lhs }
}

func testAddBlocker(a: Int, b: Int, c: Int, oa: OnlyAdds) {
  _ = #addBlocker(a * b * c)
#if TEST_DIAGNOSTICS
  _ = #addBlocker(a + b * c) // expected-error{{blocked an add; did you mean to subtract? (from macro 'addBlocker')}}
  // expected-note@-1{{use '-'}}{{21-22=-}}
  _ = #addBlocker(oa + oa) // expected-error{{blocked an add; did you mean to subtract? (from macro 'addBlocker')}}
  // expected-note@-1{{in expansion of macro 'addBlocker' here}}
  // expected-note@-2{{use '-'}}{{22-23=-}}
#endif
}
