// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-objc-interop -emit-sil -I %S/Inputs/custom-modules %s -verify > /dev/null

// expected-no-diagnostics

// This file tests the AST verifier, which performs extra checks when there are
// no errors emitted. Please do not add any.


import ObjCParseExtras

func test() {
  // Properties with instancetype getters.
  _ = InstancetypeAccessor.prop
}
