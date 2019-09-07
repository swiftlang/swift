// RUN: %target-swift-emit-silgen %s | %FileCheck %s
// UNSUPPORTED: OS=windows-msvc

// CHECK-LABEL: sil [weak_imported] [ossa] @$s21weak_linked_attribute0A8FunctionyyF : $@convention(thin) () -> ()
@_weakLinked public func weakFunction() {}
