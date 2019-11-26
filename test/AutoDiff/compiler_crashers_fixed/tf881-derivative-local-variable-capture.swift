// RUN: %target-swift-emit-silgen %s -verify
// REQUIRES: asserts

// TF-881: User-defined Swift derivative functions cannot capture local values.
// Captured local values become extra SIL function arguments, breaking the
// expected derivative function type logic.
//
// In the short term, we should diagnose these cases to prevent crashes.
// In the long term, we should investigate supporting these cases.

do {
  let capturedValue: Int = 3

  func original(_ x: Float) -> Float { x }

  // expected-error @+1 {{attribute '@derivative' can only be used in a non-local scope}}
  @derivative(of: original)
  func vjp(_ x: Float) -> (value: Float, pullback: (Float) -> Float) {
    // Reference a local variable.
    // This causes the top-level SIL function @vjp to have extra arguments.
    _ = capturedValue
    return (x, { $0 })
  }
}

// Original crasher:
// SIL verification failed: apply doesn't have right number of arguments for function: site.getNumArguments() == substConv.getNumSILArguments()
// Verifying instruction:
//    %0 = argument of bb0 : $Float                  // user: %2
//      // function_ref vjp #1 (_:) in
//   %1 = function_ref @$s4main3vjpL_ySf5value_S2fc8pullbacktSfF : $@convention(thin) (Float, Int) -> (Float, @owned @callee_guaranteed (Float) -> Float) // user: %2
// ->   %2 = apply %1(%0) : $@convention(thin) (Float, Int) -> (Float, @owned @callee_guaranteed (Float) -> Float) // user: %3
//      return %2 : $(Float, @callee_guaranteed (Float) -> Float) // id: %3
// In function:
// // AD__$s4main8originalL_yS2fF__vjp_src_0_wrt_0
// sil hidden [always_inline] [ossa] @AD__$s4main8originalL_yS2fF__vjp_src_0_wrt_0 : $@convention(thin) (Float) -> (Float, @owned @callee_guaranteed (Float) -> Float) {
// // %0                                             // user: %2
// bb0(%0 : $Float):
//   // function_ref vjp #1 (_:) in
//   %1 = function_ref @$s4main3vjpL_ySf5value_S2fc8pullbacktSfF : $@convention(thin) (Float, Int) -> (Float, @owned @callee_guaranteed (Float) -> Float) // user: %2
//   %2 = apply %1(%0) : $@convention(thin) (Float, Int) -> (Float, @owned @callee_guaranteed (Float) -> Float) // user: %3
//   return %2 : $(Float, @callee_guaranteed (Float) -> Float) // id: %3
// } // end sil function 'AD__$s4main8originalL_yS2fF__vjp_src_0_wrt_0'
