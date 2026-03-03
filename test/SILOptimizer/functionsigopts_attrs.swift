// RUN: %target-swift-frontend -module-name main -O -emit-sil -primary-file %s | %FileCheck %s

// REQUIRES: swift_in_compiler

protocol P {
  func foo()
}

class C {}

public struct Inner {
  var x: C
  var y: Int
}

public struct S : P {
  var i: Inner

  func foo() {
    print(i.x)
  }
}

@_silgen_name("barrier")
func barrier()

// The specializations should be @_eagerMove.

// CHECK-LABEL: sil {{.*}}@$s4main27callee_guaranteed_eagerMoveyyAA1P_pFTf4e_n : {{.*}}{
// CHECK:       {{bb[0-9]+}}({{%[^,]+}} : @_eagerMove $
// CHECK-LABEL: } // end sil function '$s4main27callee_guaranteed_eagerMoveyyAA1P_pFTf4e_n'

// CHECK-LABEL: sil {{.*}}@$s4main27callee_guaranteed_eagerMoveyyAA1P_pFTf4e_nAA1SV_Tg5Tf4x_n : {{.*}}{
// CHECK:       {{bb[0-9]+}}({{%[^,]+}} : @_eagerMove
// CHECK-LABEL: } // end sil function '$s4main27callee_guaranteed_eagerMoveyyAA1P_pFTf4e_nAA1SV_Tg5Tf4x_n'
@inline(never)
func callee_guaranteed_eagerMove(@_eagerMove _ p: P) {
  p.foo()
  barrier()
}

public func caller_guaranteed_eagerMove(s: S) {
  callee_guaranteed_eagerMove(s)
}

// TODO: update the test. Some exitential->generic specialization is happening, too.
// xCHECK-LABEL: sil {{.*}}@$s4main22callee_owned_eagerMoveyyAA1P_pnFTf4e_nTf4g_n : {{.*}}{
// xCHECK:       {{bb[0-9]+}}({{%[^,]+}} : @_eagerMove $
// xCHECK-LABEL: } // end sil function '$s4main22callee_owned_eagerMoveyyAA1P_pnFTf4e_nTf4g_n'
@inline(never)
func callee_owned_eagerMove(@_eagerMove _ p: __owned P) {
  p.foo()
  barrier()
}

public func caller_owned_eagerMove(s: S) {
  callee_owned_eagerMove(s)
}
