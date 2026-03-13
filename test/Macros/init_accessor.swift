// RUN: %empty-directory(%t)
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath
// RUN: %target-typecheck-verify-swift -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition) -module-name MacroUser -DTEST_DIAGNOSTICS -swift-version 5
// RUN: %target-build-swift -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition) %s -o %t/main -module-name MacroUser -swift-version 5
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main

// REQUIRES: swift_swift_parser, executable_test

@attached(peer, names: named(_value), named(_$value))
macro Wrapped() = #externalMacro(module: "MacroDefinition",
                                 type: "InitWithProjectedValueWrapperMacro")

@propertyWrapper
struct Wrapper {
  var wrappedValue: Int {
    1
  }
  var projectedValue: Wrapper {
    self
  }
}

struct Test {
  @Wrapped
  var value: Int { 1 }
}

let test = Test(_$value: Wrapper())
print(test.value)
// CHECK: 1
