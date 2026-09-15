// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath
// RUN: %target-swift-frontend -typecheck -verify -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition) %s

// When top-level code parsing is suppressed by @main, a top-level macro reference should be interpreted
// as a freestanding decl macro.

@freestanding(declaration, names: named(value)) macro varValue() = #externalMacro(module: "MacroDefinition", type: "VarValueMacro")

@main
struct Entry {
  static func main() {
    print(value)
  }
}

#varValue
