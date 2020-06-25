// RUN: %target-swift-emit-sil %s -verify
// REQUIRES: asserts

// TF-1037: Differentiation transform crashes due to multiple SIL
// differentiability witnesses with same parameter indices but different
// derivative generic signatures. Since derivative generic signatures are
// currently not mangled in derivative function names (TF-680), there is a
// name clash.
// Detailed explanation: https://github.com/apple/swift/pull/28621#issuecomment-562763390

// Test small derivative generic signature difference.
protocol P1: Differentiable {}
extension P1 {
  @differentiable // derivative generic signature: none
  func foo() -> Float { 1 }
}
extension P1 {
  @derivative(of: foo) // derivative generic signature: `<P1 where Self: P1>`
  func vjpFoo() -> (value: Float, pullback: (Float) -> (TangentVector)) {
    fatalError()
  }
}

// Test bigger derivative generic signature difference.
protocol P2: Differentiable {}
extension P2 {
  @differentiable // derivative generic signature: none
  func foo() -> Float { 1 }
}
extension P2 where Self: AdditiveArithmetic {
  // derivative generic signature: `<P2 where Self: P2, Self: AdditiveArithmetic>`
  @derivative(of: foo)
  func vjpFoo() -> (value: Float, pullback: (Float) -> (TangentVector)) {
    fatalError()
  }
}

// // AD__$s4main2P1PAAE3fooSfyF__vjp_src_0_wrt_0
// sil hidden [thunk] [always_inline] [ossa] @AD__$s4main2P1PAAE3fooSfyF__vjp_src_0_wrt_0 : $@convention(method) <τ_0_0 where τ_0_0 : P1> (@in_guaranteed τ_0_0) -> (Float, @owned @callee_guaranteed (Float) -> @out τ_0_0.TangentVector) {
// // %0                                             // user: %2
// bb0(%0 : $*τ_0_0):
//   // function_ref P1.vjpFoo()
//   %1 = function_ref @$s4main2P1PAAE6vjpFooSf5value_13TangentVectorQzSfc8pullbacktyF : $@convention(method) <τ_0_0 where τ_0_0 : P1> (@in_guaranteed τ_0_0) -> (Float, @owned @callee_guaranteed (Float) -> @out τ_0_0.TangentVector) // user: %2
//   %2 = apply %1<τ_0_0>(%0) : $@convention(method) <τ_0_0 where τ_0_0 : P1> (@in_guaranteed τ_0_0) -> (Float, @owned @callee_guaranteed (Float) -> @out τ_0_0.TangentVector) // user: %3
//   return %2 : $(Float, @callee_guaranteed (Float) -> @out τ_0_0.TangentVector) // id: %3
// } // end sil function 'AD__$s4main2P1PAAE3fooSfyF__vjp_src_0_wrt_0'
//
// Assertion failed: (!entry->getValue() && "function already exists"), function create, file /Users/danielzheng/swift-bart/swift/lib/SIL/SILFunction.cpp, line 74.
// Stack dump:
// 0.	Program arguments: /Users/danielzheng/swift-bart/build/Ninja-ReleaseAssert+stdlib-Release/swift-macosx-x86_64/bin/swift -frontend -interpret swift/test/AutoDiff/compiler_crashers/tf1037-multiple-differentiable-derivative-attributes.swift -enable-objc-interop -sdk /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.15.sdk -color-diagnostics -module-name main
// 1.	Swift version 5.1.1-dev (Swift f70940798d)
// 2.	While running pass #52 SILModuleTransform "Differentiation".
// 3.	While processing // differentiability witness for P1.foo()
// sil_differentiability_witness hidden [parameters 0] [results 0] @$s4main2P1PAAE3fooSfyF : $@convention(method) <Self where Self : P1> (@in_guaranteed Self) -> Float {
// }
//  on SIL function "@$s4main2P1PAAE3fooSfyF".
//  for 'foo()' (at swift/test/AutoDiff/compiler_crashers/tf1037-multiple-differentiable-derivative-attributes.swift:15:3)
// 4.	While creating SIL function "@AD__$s4main2P1PAAE3fooSfyF__vjp_src_0_wrt_0".
//  for 'foo()' (at swift/test/AutoDiff/compiler_crashers/tf1037-multiple-differentiable-derivative-attributes.swift:15:3)
