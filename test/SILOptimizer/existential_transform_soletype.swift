// RUN: %target-swift-frontend -O -wmo -Xllvm -enable-existential-specializer -Xllvm -sil-disable-pass=GenericSpecializer -Xllvm -sil-disable-pass=FunctionSignatureOpts -Xllvm -sil-disable-pass=SILCombine -emit-sil -sil-verify-all %s | %FileCheck %s

internal protocol SPP {
  func bar()  -> Int32
}
internal class SCC: SPP {
  @inline(never) func bar() -> Int32 {
   return 5
  }
}

@inline(never) internal func opt2(b:SPP) -> Int32{
 return b.bar()
}

@inline(never) internal func opt1(b:SPP) -> Int32{
 return opt2(b:b)
}

// CHECK-LABEL: sil hidden [noinline] @$s30existential_transform_soletype4opt11bs5Int32VAA3SPP_p_tF : $@convention(thin) (@in_guaranteed SPP) -> Int32 {
// CHECK: bb0(%0 : $*SPP):
// CHECK:   debug_value_addr
// CHECK:   function_ref @$s30existential_transform_soletype4opt21bs5Int32VAA3SPP_p_tFTf4e_n : $@convention(thin) <τ_0_0 where τ_0_0 : SPP> (@in_guaranteed τ_0_0) -> Int32 // user: %4
// CHECK:   open_existential_addr
// CHECK:   apply
// CHECK:   return
// CHECK-LABEL: } // end sil function '$s30existential_transform_soletype4opt11bs5Int32VAA3SPP_p_tF'

// CHECK-LABEL: sil shared [noinline] @$s30existential_transform_soletype4opt21bs5Int32VAA3SPP_p_tFTf4e_n : $@convention(thin) <τ_0_0 where τ_0_0 : SPP> (@in_guaranteed τ_0_0) -> Int32 {
// CHECK: bb0(%0 : $*τ_0_0):
// CHECK:   alloc_stack
// CHECK:   init_existential_addr
// CHECK:   copy_addr
// CHECK:   debug_value_addr
// CHECK:   open_existential_addr
// CHECK:   witness_method 
// CHECK:   apply
// CHECK:   dealloc_stack
// CHECK:   return
// CHECK-LABEL: } // end sil function '$s30existential_transform_soletype4opt21bs5Int32VAA3SPP_p_tFTf4e_n'


@_optimize(none) func foo(number:Int32)->Int32 {
  var b:SPP
  if number < 5 {
    b = SCC()
  } else {
    b = SCC()
  }
  let x = opt1(b:b)
  return x
}
