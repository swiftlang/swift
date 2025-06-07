// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -Xllvm -sil-print-types -emit-silgen -verify %s | %FileCheck %s

protocol P { }

protocol Q: Sendable { }

// CHECK-LABEL: sil hidden [ossa] @$s24isolated_conformance_sil7castToPyyypF
func castToP(_ value: Any) {
  // CHECK: checked_cast_addr_br take_always Any in [[SOURCE:%[0-9]+]] : $*Any to any P
  if let p = value as? any P {
    _ = p
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s24isolated_conformance_sil8castToPQyyypF
func castToPQ(_ value: Any) {
  // CHECK: checked_cast_addr_br [prohibit_isolated_conformances] take_always Any in [[SOURCE:%[0-9]+]] : $*Any to any P & Q
  if let pq = value as? any P & Q {
    _ = pq
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s24isolated_conformance_sil13forceCastToPQyyypF 
func forceCastToPQ(_ value: Any) {
  // CHECK: unconditional_checked_cast_addr [prohibit_isolated_conformances] Any in [[SOURCE:%[0-9]+]] : $*Any to any P & Q in
  _ = value as! any P & Q
}

// CHECK-LABEL: sil hidden [ossa] @$s24isolated_conformance_sil14objectCastToPQyyyXlF
func objectCastToPQ(_ value: AnyObject) {
  // CHECK: checked_cast_br [prohibit_isolated_conformances] AnyObject in [[SOURCE:%[0-9]+]] : $AnyObject to any P & Q & AnyObject
  if let p = value as? any AnyObject & P & Q {
    _ = p
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s24isolated_conformance_sil19objectForceCastToPQyyyXlF
func objectForceCastToPQ(_ value: AnyObject) {
  // CHECK: unconditional_checked_cast [prohibit_isolated_conformances] [[SOURCE:%[0-9]+]] : $AnyObject to any P & Q & AnyObject
  _ = value as! any AnyObject & P & Q
}
