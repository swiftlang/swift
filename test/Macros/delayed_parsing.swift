// REQUIRES: swift_swift_parser
//
// RUN: %empty-directory(%t)
// RUN: %host-build-swift -swift-version 5 -parse-as-library -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath

// Type check testing
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/delayed_parsing.swiftmodule -experimental-skip-non-inlinable-function-bodies-without-types -swift-version 5 -parse-as-library -load-plugin-library %t/%target-library-name(MacroDefinition) %s

@freestanding(declaration)
macro freestandingWithClosure<T>(_ value: T, body: (T) -> T) = #externalMacro(module: "MacroDefinition", type: "EmptyDeclarationMacro")

#freestandingWithClosure(0) { (x: Int) in
  struct LocalStruct {
    func opaqueReturn() -> some Any {
      return 3
    }
  }

  return x
}
