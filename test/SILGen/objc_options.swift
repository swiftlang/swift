// RUN: %swift %clang-importer-sdk -emit-silgen -target x86_64-apple-macosx10.9 %s | FileCheck %s

import Foundation

func foo(a: NSKeyValueObservingOptions) -> Bool {
  return (a ^ a) == nil
}

// CHECK: sil_witness_table shared NSKeyValueObservingOptions: BitwiseOperationsType module Foundation
