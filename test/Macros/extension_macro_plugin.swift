// REQUIRES: swift_swift_parser

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/plugins)
//
//== Build the plugin library
// RUN: %host-build-swift \
// RUN:   -swift-version 5 \
// RUN:   -emit-library \
// RUN:   -o %t/plugins/%target-library-name(MacroDefinition) \
// RUN:   -module-name=MacroDefinition \
// RUN:   %S/Inputs/syntax_macro_definitions.swift \
// RUN:   -g -no-toolchain-stdlib-rpath

// RUN: env SWIFT_DUMP_PLUGIN_MESSAGING=1 %target-swift-frontend \
// RUN:   -typecheck -verify \
// RUN:   -swift-version 5 \
// RUN:   -external-plugin-path %t/plugins#%swift-plugin-server \
// RUN:   -module-name MyApp \
// RUN:   %s \
// RUN:   2>&1 | tee %t/macro-expansions.txt

// RUN: %FileCheck %s < %t/macro-expansions.txt

@attached(extension, conformances: P, names: named(requirement))
macro DelegatedConformance() = #externalMacro(module: "MacroDefinition", type: "DelegatedConformanceViaExtensionMacro")

protocol P {
  static func requirement()
}

struct Wrapped: P {
  static func requirement() {
    print("Wrapped.requirement")
  }
}

@DelegatedConformance
struct Generic<Element> {}

// CHECK: {"expandMacroResult":{"diagnostics":[],"expandedSource":"extension Generic: P where Element: P {\n  static func requirement() {\n    Element.requirement()\n  }\n}"}}

func requiresP(_ value: (some P).Type) {
  value.requirement()
}

requiresP(Generic<Wrapped>.self)

struct Outer {
  @DelegatedConformance
  struct Nested<Element> {}
}

// CHECK: {"expandMacroResult":{"diagnostics":[],"expandedSource":"extension Outer.Nested: P where Element: P {\n  static func requirement() {\n    Element.requirement()\n  }\n}"}}

requiresP(Outer.Nested<Wrapped>.self)
