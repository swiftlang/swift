// RUN: %target-swift-frontend %clang-importer-sdk -emit-silgen %s | FileCheck %s

// REQUIRES: objc_interop

import Foundation

func foo(a: NSKeyValueObservingOptions) -> Bool {
  return (a ^ a) == nil
}

// CHECK: sil_witness_table shared NSKeyValueObservingOptions: BitwiseOperationsType module Foundation
