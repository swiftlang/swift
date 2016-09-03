// RUN: rm -rf %t && mkdir -p %t
// RUN: %build-silgen-test-overlays

// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) -verify -emit-silgen %s | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

// Ensure we emit allocating constructor thunks for ObjC initializers that
// were inherited.
// CHECK-LABEL: sil shared @_TFCSo3FooCfT_S_ : $@convention(method) (@thick Foo.Type) -> @owned Foo {
func foo() {
  _ = Foo()
}

