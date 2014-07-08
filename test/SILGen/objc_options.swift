// RUN: rm -rf %t/clang-module-cache
// RUN: %swift %clang-importer-sdk -emit-silgen -module-cache-path %t/clang-module-cache -target x86_64-apple-macosx10.9 %s | FileCheck %s

import Foundation

func foo(a: NSKeyValueObservingOptions) -> Bool {
  return a || true
}

// CHECK: sil_witness_table shared NSKeyValueObservingOptions: LogicValue module Foundation
