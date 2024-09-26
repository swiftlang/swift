// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -enable-experimental-feature IsolatedDeinit -emit-ir %s | %FileCheck %s

// REQUIRES: swift_feature_IsolatedDeinit

public class Foo {
  @MainActor
  deinit {}
}

// CHECK: @"$s20deinit_isolation_tbd3FooCfZ"
// CHECK: @"$s20deinit_isolation_tbd3FooCfD"
