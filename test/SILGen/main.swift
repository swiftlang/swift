// RUN: %target-swift-emit-silgen -module-name main %s | %FileCheck %s

_ = true

// CHECK-LABEL: sil hidden [ossa] @main : $@convention(c) (Int32, UnsafeMutablePointer<Optional<UnsafeMutablePointer<Int8>>>) -> Int32

