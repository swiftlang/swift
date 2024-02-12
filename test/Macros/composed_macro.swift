// REQUIRES: swift_swift_parser, executable_test

// RUN: %empty-directory(%t)
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath
// RUN: %target-typecheck-verify-swift -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition) -module-name MacroUser -DTEST_DIAGNOSTICS -swift-version 5
// RUN: %target-build-swift -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition) %s -o %t/main -module-name MacroUser -swift-version 5
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

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
