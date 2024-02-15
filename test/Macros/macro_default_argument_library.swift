// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)

// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath

// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -module-name CheckInterface -load-plugin-library %t/%target-library-name(MacroDefinition) -I %t
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name CheckInterface -load-plugin-library %t/%target-library-name(MacroDefinition) -I %t

// RUN: %FileCheck %s < %t.swiftinterface

@freestanding(expression)
public macro stringify<T>(_ value: T) -> (T, String) = #externalMacro(
    module: "MacroDefinition", type: "StringifyMacro"
)

// CHECK: public func foo(param: (Swift.String, Swift.String) = #stringify(#fileID))
public func foo(param: (String, String) = #stringify(#fileID)) {
    print(param)
}
