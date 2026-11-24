// RUN: %target-swift-frontend -sil-verify-all -enable-sil-opaque-values -parse-as-library -emit-sil -O -module-name main %s | %FileCheck %s

// Regression test: AddressLowering's final cleanup crashed on a `switch` over
// a `borrowing` non-`Copyable` existential.
//
//   Assertion failed: (detail::isPresent(Val) && "dyn_cast on a non-existent
//   value"), function dyn_cast
//   While running pass SILFunctionTransform "AddressLowering"
//
// Two entries of the valueStorageMap can share one storage address. The first
// deleted the defining instruction; the second then asked whether that same
// instruction was dead. A deleted instruction has been unlinked and has
// dropped its operands, so isInstructionTriviallyDead() read a null operand
// out of it and crashed.
//
// Needs -O: at -Onone the storage map does not reach the shape that shares an
// address. Needs `borrowing`: a `consuming` subject is clean. Needs no cast --
// a `switch` with nothing but `default` is enough.

public protocol P: ~Copyable {}

// CHECK-LABEL: sil @$s4main16switchOverBorrowySbAA1P_pRi_s_XPF :
// CHECK:       bb0(%0 : $*any P & ~Copyable):
// CHECK:       } // end sil function
public func switchOverBorrow(_ x: borrowing any P & ~Copyable) -> Bool {
  switch x {
  default: return false
  }
}
