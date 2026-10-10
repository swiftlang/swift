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
// RUN:   %t/test.swift \
// RUN:   2>%t/macro-loading.txt

// RUN: %FileCheck %s < %t/macro-loading.txt

// CHECK: guest error!

//--- test.swift
// The forwarded getCapability is the guest's first pump, so its abort now surfaces at
// plugin-load time (warning at the decl + error at the use) rather than during expansion.
// expected-warning @+2 {{failed to load library plugin}}
// expected-note @+1 {{declared here}}
@freestanding(expression) macro constInt() -> Int = #externalMacro(module: "MacroDefinition", type: "ConstMacro")

func foo() {
  // expected-error @+1 {{failed to load library plugin}}
  let _: Int = #constInt
}

//--- MacroDefinition.c
#include "Inputs/wasi_shim.h"

__attribute__((export_name("_start")))
void _start(void) {}

__attribute__((export_name("swift_wasm_macro_v1_pump")))
void pump(void) {
  swift_abort("guest error!\n");
}
