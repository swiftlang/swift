// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath

// RUN: %target-swift-frontend -emit-module %t/Lib.swift -module-name Lib -emit-module-path %t/Lib.swiftmodule

// RUN: %target-swift-frontend -typecheck -verify -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition) -I %t %t/a.swift -primary-file %t/b.swift

//--- Lib.swift

@freestanding(expression)
public macro magicFile() -> String = #externalMacro(module: "MacroDefinition", type: "MagicFileMacro")

//--- a.swift

import Lib

func foo(x: String = #magicFile) {}

//--- b.swift

// We're missing the necessary import in this file, make sure we diagnose.
func bar() {
  foo() // expected-error {{no macro named 'magicFile'}}
}
