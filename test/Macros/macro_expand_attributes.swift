// RUN: %empty-directory(%t)
// RUN: %target-build-swift -I %swift-host-lib-dir -L %swift-host-lib-dir -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath
// RUNx: %target-swift-frontend -dump-ast -enable-experimental-feature Macros -load-plugin-library %t/%target-library-name(MacroDefinition) -I %swift-host-lib-dir %s -module-name MacroUser 2>&1 | %FileCheck --check-prefix CHECK-AST %s
// RUN: %target-typecheck-verify-swift -enable-experimental-feature Macros -load-plugin-library %t/%target-library-name(MacroDefinition) -I %swift-host-lib-dir -module-name MacroUser -DTEST_DIAGNOSTICS -swift-version 5

// FIXME: Swift parser is not enabled on Linux CI yet.
// REQUIRES: OS=macosx

@attached(memberAttributes) macro wrapAllProperties() = #externalMacro(module: "MacroDefinition", type: "WrapAllProperties")

@propertyWrapper
struct Wrapper<T> {
  var wrappedValue: T
}

@wrapAllProperties
struct S {
  var x: Int
  var y: Int
}
