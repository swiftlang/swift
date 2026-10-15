// End-to-end check that a `.custom_section.`-prefixed @section constant survives
// the whole pipeline from Swift source (AST) through IRGen and the linker as a
// real Wasm custom section, and that no addressable global is emitted for it
// (which would otherwise fail to link, since a custom section has no address).

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -parse-as-library -enable-experimental-feature CompileTimeValuesPreview -c -o %t/main.o
// RUN: wasm-ld --no-entry --allow-undefined %t/main.o -o %t/main.wasm
// RUN: obj2yaml %t/main.wasm | %FileCheck %s

// REQUIRES: CPU=wasm32
// REQUIRES: swift_feature_CompileTimeValuesPreview

@section(".custom_section.swift_metadata")
let metadata: InlineArray<4, UInt8> = [0xDE, 0xAD, 0xBE, 0xEF]

// CHECK:      - Type:    CUSTOM
// CHECK:        Name:    swift_metadata
// CHECK:        Payload: DEADBEEF
