// RUN: %empty-directory(%t/src)
// RUN: %empty-directory(%t/sdk)
// RUN: split-file %s %t/src

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck %t/src/main.swift \
// RUN:   -import-objc-header %t/src/Test.h \
// RUN:   -module-name main -I %t -verify

// REQUIRES: objc_interop

//--- Test.h
#include <stdint.h>

typedef struct {
    uint8_t* buffer;
} frame_t __attribute__((swift_name("Frame")));

//--- main.swift
func test(rawPointer: UnsafeMutableRawPointer) {
  _ = Frame(buffer: rawPointer) // Ok (even though init is synthesized the type is imported)
}
