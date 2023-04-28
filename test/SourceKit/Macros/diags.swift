@freestanding(expression)
macro coerceToInt<T>(_: T) -> Int = #externalMacro(module: "MacroDefinition", type: "CoerceToIntMacro")

@freestanding(expression)
macro stringify<T>(_: T) -> (T, String) = #externalMacro(module: "MacroDefinition", type: "StringifyMacro")

@attached(peer)
macro Invalid() = #externalMacro(module: "MacroDefinition", type: "InvalidMacro")

func foo() {
  let _ = #coerceToInt("a")
  let _ = #coerceToInt("b")

  // FIXME: There's currently a duplicate diagnostic issue with this,
  // so the response lists the same diagnostic twice, and has a spurious
  // buffer containing the expansion of `#coerceToInt` on its own (rdar://108622244).
  let _ = #stringify(#coerceToInt("c"))
}

@Invalid
struct Bad {}

// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)

//##-- Prepare the macro plugin.
// RUN: %host-build-swift -swift-version 5 -emit-library -o %t/%target-library-name(MacroDefinition) -module-name=MacroDefinition %S/../../Macros/Inputs/syntax_macro_definitions.swift -g -no-toolchain-stdlib-rpath

// RUN: %sourcekitd-test -req=diags %s -- -swift-version 5 -load-plugin-library %t/%target-library-name(MacroDefinition) -module-name MacroUser %s | %sed_clean > %t.response
// RUN: %diff -u %s.response %t.response
