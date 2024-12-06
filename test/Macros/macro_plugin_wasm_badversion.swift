// REQUIRES: swift_swift_parser
// REQUIRES: CODEGENERATOR=WebAssembly

// RUN: %empty-directory(%t)

// RUN: split-file %s %t

//#-- Prepare the Wasm macro plugin.
// RUN: %swift-build-wasm-c-plugin %t/MacroDefinition.c -o %t/Plugin.wasm

// RUN: %target-swift-frontend \
// RUN:   -typecheck -verify \
// RUN:   -swift-version 5 \
// RUN:   -load-resolved-plugin %t/Plugin.wasm#%swift-plugin-server#MacroDefinition \
// RUN:   -module-name MyApp \
// RUN:   %t/test.swift

//--- test.swift
// expected-warning @+2 {{Wasm plugin has an unknown ABI (could not find 'swift_wasm_macro_v1_pump')}}
// expected-note @+1 {{declared here}}
@freestanding(expression) macro constInt() -> Int = #externalMacro(module: "MacroDefinition", type: "ConstMacro")

func foo() {
  // expected-error @+1 {{Wasm plugin has an unknown ABI (could not find 'swift_wasm_macro_v1_pump')}}
  let _: Int = #constInt
}

//--- MacroDefinition.c
__attribute__((export_name("_start")))
void _start(void) {}

__attribute__((export_name("swift_wasm_macro_v100_pump")))
void pump(void) {}
