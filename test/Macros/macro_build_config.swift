// REQUIRES: swift_swift_parser, executable_test

// RUN: %empty-directory(%t)
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift

// Execution testing
// RUN: %target-build-swift -swift-version 5 -g -load-plugin-library %t/%target-library-name(MacroDefinition) %s -o %t/main -module-name MacroUser -Xfrontend -emit-dependencies-path -Xfrontend %t/main.d -Xfrontend -emit-reference-dependencies-path -Xfrontend %t/main.swiftdeps -DDEBUG
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

@freestanding(expression) macro isCustomConditionSet(_ name: String) -> Bool = #externalMacro(module: "MacroDefinition", type: "CustomConditionCheckMacro")

// CHECK: Release = false
print("Release = \(#isCustomConditionSet("RELEASE"))")

// CHECK: Debug = true
print("Debug = \(#isCustomConditionSet("DEBUG"))")
