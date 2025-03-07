// RUN: %target-swift-frontend -swift-version 6 %s -emit-silgen | %FileCheck %s

// REQUIRES: concurrency

class NonSendableKlass {}

// CHECK-DAG: subscript(_: sending NonSendableKlass) -> sending NonSendableKlass { get }

// CHECK-DAG: sil hidden [ossa] @$s17sending_subscript1SVyAA16NonSendableKlassCAEncig : $@convention(method) (@sil_sending @owned NonSendableKlass, S) -> @sil_sending @owned NonSendableKlass {
struct S {
  subscript(_: sending NonSendableKlass) -> sending NonSendableKlass { NonSendableKlass() }
}

// CHECK-DAG: subscript(_: sending NonSendableKlass) -> sending (NonSendableKlass, NonSendableKlass) { get }

// CHECK-DAG: sil hidden [ossa] @$s17sending_subscript2S2VyAA16NonSendableKlassC_AEtAEncig : $@convention(method) (@sil_sending @owned NonSendableKlass, S2) -> (@sil_sending @owned NonSendableKlass, @sil_sending @owned NonSendableKlass) {
struct S2 {
  subscript(_: sending NonSendableKlass) -> sending (NonSendableKlass, NonSendableKlass) { 
    (NonSendableKlass(), NonSendableKlass()) 
  }
}

