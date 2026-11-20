// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath
// RUN: %target-swift-frontend -swift-version 5 -emit-sil -load-plugin-library %t/%target-library-name(MacroDefinition) %s -module-name MacroUser -parse-as-library -o - -g | %FileCheck %s

// The scope of an exit block must match its location, inside or outside of a
// macro expansion.

@freestanding(expression) macro stringify<T>(_ value: T) -> (T, String) = #externalMacro(module: "MacroDefinition", type: "StringifyMacro")

@inline(never) func use<T>(_ t: T) {}
@inline(never) func mayThrow() throws -> Int { 0 }
struct E: Error {}

// The throw is emitted into the error path of the call in the expansion.
// CHECK-LABEL: sil @$s9MacroUser07throwInA0yyKF
// CHECK: try_apply {{.*}}, loc "@__swiftmacro_{{.*}}, scope [[TIM_MACRO:[0-9]+]]
// CHECK: throw {{.*}}, loc "@__swiftmacro_{{.*}}, scope [[TIM_MACRO]]
public func throwInMacro() throws {
  _ = #stringify(try mayThrow())
}

// The throw is shared with a throw outside of the expansion. The closest
// common scope is the scope defining "x".
// CHECK-LABEL: sil @$s9MacroUser07throwInA8AfterLetyySbKF
// CHECK: debug_value {{.*}}, let, name "x", {{.*}}, scope [[TIMAL_X:[0-9]+]]
// CHECK: try_apply {{.*}}, loc "@__swiftmacro_{{.*}}, scope [[TIMAL_MACRO:[0-9]+]]
// CHECK: throw {{.*}}, loc "{{.*}}macro_expand_shared_exit_scope_debuginfo.swift":[[@LINE+6]]:1, scope [[TIMAL_X]]
public func throwInMacroAfterLet(_ flag: Bool) throws {
  let x = 1
  if flag { throw E() }
  _ = #stringify(try mayThrow())
  use(x)
}
