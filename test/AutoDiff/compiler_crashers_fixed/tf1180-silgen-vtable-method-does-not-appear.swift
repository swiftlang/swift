// RUN: %target-swift-frontend -emit-silgen %s

// TF-1180: SIL verification error regarding `@differentiable` class method
// witnesses for `@differentiable` protocol requirements.

import _Differentiation

protocol Protocol {
  @differentiable
  func method(_ x: Float) -> Float
}

class Class: Protocol {
  @differentiable
  public func method(_ x: Float) -> Float { x }
}

// Original error:
// SIL verification failed: method does not appear in the class's vtable: VerifyClassMethodVisitor(member).Seen
// Verifying instruction:
//      %2 = load_borrow %1 : $*Class                // users: %8, %4, %3
// ->   %3 = class_method %2 : $Class, #Class.method!jvp.SU.<Self where Self : Protocol> : (Class) -> (Float) -> Float, $@convention(method) (Float, @guaranteed Class) -> (Float, @owned @callee_guaranteed (Float) -> Float) // user: %4
//      %4 = apply %3(%0, %2) : $@convention(method) (Float, @guaranteed Class) -> (Float, @owned @callee_guaranteed (Float) -> Float) // user: %5
// In function:
// // AD__$s4main5ClassCAA8ProtocolA2aDP6methodyS2fFTW_jvp_SU
// sil private [transparent] [thunk] [ossa] @AD__$s4main5ClassCAA8ProtocolA2aDP6methodyS2fFTW_jvp_SU : $@convention(witness_method: Protocol) (Float, @in_guaranteed Class) -> (Float, @owned @callee_guaranteed (Float) -> Float) {
// // %0                                             // user: %4
// // %1                                             // user: %2
// bb0(%0 : $Float, %1 : $*Class):
//   %2 = load_borrow %1 : $*Class                   // users: %8, %4, %3
//   %3 = class_method %2 : $Class, #Class.method!jvp.SU.<Self where Self : Protocol> : (Class) -> (Float) -> Float, $@convention(method) (Float, @guaranteed Class) -> (Float, @owned @callee_guaranteed (Float) -> Float) // user: %4
//   %4 = apply %3(%0, %2) : $@convention(method) (Float, @guaranteed Class) -> (Float, @owned @callee_guaranteed (Float) -> Float) // user: %5
//   (%5, %6) = destructure_tuple %4 : $(Float, @callee_guaranteed (Float) -> Float) // users: %7, %7
//   %7 = tuple (%5 : $Float, %6 : $@callee_guaranteed (Float) -> Float) // user: %9
//   end_borrow %2 : $Class                          // id: %8
//   return %7 : $(Float, @callee_guaranteed (Float) -> Float) // id: %9
// } // end sil function 'AD__$s4main5ClassCAA8ProtocolA2aDP6methodyS2fFTW_jvp_SU'
