// RUN: %empty-directory(%t/src)
// RUN: split-file %s %t/src

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck %t/src/main.swift \
// RUN:   -import-objc-header %t/src/Test.h \
// RUN:   -module-name main -I %t -verify

// REQUIRES: objc_interop

//--- Test.h
@import Foundation;

extern const unsigned char TestDir[];

extern NSString * _Nonnull __TestDir __attribute__((swift_name("TestDir")));

//--- main.swift
import Foundation

func test() {
  print(TestDir)
}
