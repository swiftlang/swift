// REQUIRES: swift_swift_parser

/// Test loading and external library through `-load-plugin-library`
/// TODO: switch this test case to use `-external-plugin-path`.

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/plugins)
// RUN: split-file %s %t
//
//== Build the plugin library
// RUN: %host-build-swift \
// RUN:   -swift-version 5 \
// RUN:   -emit-library \
// RUN:   -o %t/plugins/%target-library-name(MacroDefinition) \
// RUN:   -module-name=MacroDefinition \
// RUN:   %S/../Macros/Inputs/syntax_macro_definitions.swift \
// RUN:   -g -no-toolchain-stdlib-rpath

/// No macro plugin when macro not used.
// RUN: %target-swift-frontend -scan-dependencies -module-load-mode prefer-serialized -module-name MyApp -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/nomacro.swift -o %t/deps1.json -swift-version 5 -cache-compile-job -cas-path %t/cas -external-plugin-path %t/plugins#%swift-plugin-server
// RUN: %S/Inputs/SwiftDepsExtractor.py %t/deps1.json MyApp casFSRootID > %t/no_macro_fs.casid
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-include-tree-list @%t/no_macro_fs.casid | %FileCheck %s --check-prefix=NO-MACRO
// NO-MACRO-NOT: MacroDefinition

// RUN: %target-swift-frontend -scan-dependencies -module-load-mode prefer-serialized -module-name MyApp -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/macro.swift -o %t/deps.json -swift-version 5 -cache-compile-job -cas-path %t/cas -external-plugin-path %t/plugins#%swift-plugin-server

// RUN: %S/Inputs/SwiftDepsExtractor.py %t/deps.json MyApp casFSRootID > %t/fs.casid
// RUN: %cache-tool -cas-path %t/cas -cache-tool-action print-include-tree-list @%t/fs.casid | %FileCheck %s --check-prefix=FS

// FS: MacroDefinition

// RUN: %S/Inputs/BuildCommandExtractor.py %t/deps.json clang:SwiftShims > %t/SwiftShims.cmd
// RUN: %swift_frontend_plain @%t/SwiftShims.cmd

// RUN: %S/Inputs/BuildCommandExtractor.py %t/deps.json MyApp > %t/MyApp.cmd
// RUN: %{python} %S/Inputs/GenerateExplicitModuleMap.py %t/deps.json > %t/map.json
// RUN: llvm-cas --cas %t/cas --make-blob --data %t/map.json > %t/map.casid

// RUN: %target-swift-frontend \
// RUN:   -typecheck -verify -cache-compile-job -cas-path %t/cas \
// RUN:   -swift-version 5 -disable-implicit-swift-modules \
// RUN:   -external-plugin-path %t/plugins/#%swift-plugin-server \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   -module-name MyApp -explicit-swift-module-map-file @%t/map.casid \
// RUN:   %t/macro.swift @%t/MyApp.cmd

//--- nomacro.swift
func test() {}

//--- macro.swift
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
