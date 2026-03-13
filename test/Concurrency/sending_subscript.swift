// RUN: %target-swift-frontend -swift-version 6 %s -emit-silgen | %FileCheck %s

// REQUIRES: concurrency

protocol P {}

class NonSendableKlass: P {}

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

// CHECK-DAG: subscript(_: sending NonSendableKlass) -> sending some P { get }

// CHECK-DAG: sil hidden [ossa] @$s17sending_subscript2S3VyQrAA16NonSendableKlassCncig : $@convention(method) (@sil_sending @owned NonSendableKlass, S3) -> @sil_sending @out @_opaqueReturnTypeOf("$s17sending_subscript2S3VyQrAA16NonSendableKlassCncip", 0) __ {
struct S3 {
  subscript(_: sending NonSendableKlass) -> sending (some P) { NonSendableKlass() }
}

// CHECK-DAG: subscript(_: sending NonSendableKlass) -> sending (some P, NonSendableKlass) { get }

// CHECK-DAG: sil hidden [ossa] @$s17sending_subscript2S4VyQr_AA16NonSendableKlassCtAEncig : $@convention(method) (@sil_sending @owned NonSendableKlass, S4) -> (@sil_sending @out @_opaqueReturnTypeOf("$s17sending_subscript2S4VyQr_AA16NonSendableKlassCtAEncip", 0) __, @sil_sending @owned NonSendableKlass) {
struct S4 {
  subscript(_: sending NonSendableKlass) -> sending (some P, NonSendableKlass) {
    (NonSendableKlass(), NonSendableKlass())
  }
}
