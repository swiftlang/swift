// RUN: %target-swift-frontend -swift-version 6 %s -emit-silgen | %FileCheck %s

// REQUIRES: concurrency

class NonSendableKlass {}

// CHECK: subscript(_: sending NonSendableKlass) -> sending NonSendableKlass { get }

// CHECK: sil hidden [ossa] @$s17sending_subscript1SVyAA16NonSendableKlassCAEncig : $@convention(method) (@sil_sending @owned NonSendableKlass, S) -> @sil_sending @owned NonSendableKlass {
struct S {
  subscript(_: sending NonSendableKlass) -> sending NonSendableKlass { NonSendableKlass() }
}

