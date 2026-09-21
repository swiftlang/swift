// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath
// RUN: %target-swift-frontend -swift-version 5 -typecheck -verify -load-plugin-library %t/%target-library-name(MacroDefinition) %s

@attached(extension, conformances: Copyable)
macro ConditionalCopyable() = #externalMacro(module: "MacroDefinition", type: "ConditionalCopyableMacro")

@ConditionalCopyable
struct Box<T: ~Copyable>: ~Copyable {
  var value: T
}

func needsCopy<T>(_ x: T) -> (T, T) { (x, x) }

// `Box<Int>` is Copyable only because of the macro-generated conditional
// conformance; if the macro were not expanded here this would fail to compile.
func test(_ b: Box<Int>) {
  _ = needsCopy(b)
}
