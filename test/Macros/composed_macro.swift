// RUN: %empty-directory(%t)
// RUN: %target-build-swift -swift-version 5 -I %swift-host-lib-dir -L %swift-host-lib-dir -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath
// RUNx: %target-swift-frontend -dump-ast -load-plugin-library %t/%target-library-name(MacroDefinition) -I %swift-host-lib-dir %s -module-name MacroUser 2>&1 | %FileCheck --check-prefix CHECK-AST %s
// RUN: %target-typecheck-verify-swift -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition) -I %swift-host-lib-dir -module-name MacroUser -DTEST_DIAGNOSTICS -swift-version 5
// RUN: %target-build-swift -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition) -I %swift-host-lib-dir -L %swift-host-lib-dir %s -o %t/main -module-name MacroUser -swift-version 5
// RUN: %target-run %t/main | %FileCheck %s
// REQUIRES: executable_test

// FIXME: Swift parser is not enabled on Linux CI yet.
// REQUIRES: OS=macosx

@attached(memberAttribute)
@attached(member, names: named(_storage))
macro myTypeWrapper() = #externalMacro(module: "MacroDefinition", type: "TypeWrapperMacro")
@attached(accessor) macro accessViaStorage() = #externalMacro(module: "MacroDefinition", type: "AccessViaStorageMacro")

struct _Storage {
  var x: Int = 0 {
    willSet { print("setting \(newValue)") }
  }
  var y: Int = 0 {
    willSet { print("setting \(newValue)") }
  }
}

@myTypeWrapper
struct S {
  var x: Int
  var y: Int
}

var s = S()

// CHECK: setting 10
s.x = 10

// CHECK: setting 100
s.y = 100
