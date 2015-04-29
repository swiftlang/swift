// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-silgen %s | FileCheck %s

// REQUIRES: objc_interop

import Foundation

func foo(a: NSKeyValueObservingOptions) -> Bool {
  return (a ^ a) == nil
}

// Note: nothing needs the BitwiseOperationsType conformance, so don't emit it
// CHECK-NOT: sil_witness_table shared NSKeyValueObservingOptions: BitwiseOperationsType module Foundation
// CHECK: sil_witness_table shared NSKeyValueObservingOptions: _RawOptionSetType module Foundation
// CHECK-NOT: sil_witness_table shared NSKeyValueObservingOptions: BitwiseOperationsType module Foundation
