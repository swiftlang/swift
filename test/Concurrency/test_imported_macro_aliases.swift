// RUN: %empty-directory(%t/src)
// RUN: %empty-directory(%t/sdk)
// RUN: split-file %s %t/src

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck %t/src/main.swift \
// RUN:   -import-objc-header %t/src/Test.h \
// RUN:   -swift-version 6 \
// RUN:   -enable-experimental-feature ImportMacroAliases \
// RUN:   -module-name main -I %t -verify -verify-ignore-unrelated

// REQUIRES: objc_interop
// REQUIRES: concurrency
// REQUIRES: swift_feature_ImportMacroAliases

// Make sure that aliases imported into Swift as properties don't create concurrency issues.

//--- Test.h
#define F f

void f();

//--- main.swift
func test() {
  _ = F // Ok
}

func testAsync() async {
  _ = F // Ok
}

actor A {
  func test() {
    _ = F // Ok
  }

  @MainActor static func testGlobalIsolated() {
    _ = F // Ok
  }
}
