// RUN: %target-swift-frontend -emit-sib -O -sil-verify-all -module-name serialization -Xllvm -sil-disable-pass=serialize-sil %s -o %t.sib
// RUN: %target-sil-opt -enable-sil-verify-all -autodiff-closure-specialization -module-name serialization %t.sib -o - | %FileCheck %s --check-prefix=SERIALIZED --implicit-check-not=_wrt_0_spec
// RUN: %target-swift-frontend -emit-sil -O -sil-verify-all -module-name serialization -package-name Pkg -enable-library-evolution -experimental-package-cmo -experimental-allow-non-resilient-access -Xllvm -sil-disable-pass=serialize-sil -D PACKAGE %s | %FileCheck %s --check-prefix=PACKAGE --implicit-check-not=_wrt_0_spec
// RUN: %target-swift-frontend -emit-sil -O -sil-verify-all -module-name serialization %s | %FileCheck %s --check-prefix=SPECIALIZED

import _Differentiation

// Disable serialize-sil so the isolated pass still sees serialized functions.
// SERIALIZED-LABEL: {{^}}sil [serialized] [ossa] @$s13serialization6branchyS2f_SbtFTJrSUpSr :
// SERIALIZED:         %[[#CALLBACK:]] = partial_apply [callee_guaranteed]
// SERIALIZED:         = tuple {{.*}}%[[#CALLBACK]]
// SERIALIZED:         = function_ref @$s13serialization6branchyS2f_SbtFTJpSUpSr :
// SERIALIZED-LABEL: {{^}}sil [serialized] [ossa] @$s13serialization6branchyS2f_SbtFTJpSUpSr :
// SERIALIZED:         switch_enum
// SERIALIZED:         = apply %[[#]](%[[#]]) : $@callee_guaranteed (Float) -> (Float, Float)

// Package serialization keeps the pullback serialized even when the VJP is eligible.
// PACKAGE-LABEL: {{^}}sil @$s13serialization6branchyS2f_SbtFTJrSUpSr :
// PACKAGE:         = function_ref @$s13serialization6branchyS2f_SbtFTJpSUpSr :
// PACKAGE-LABEL: {{^}}sil shared [serialized_for_package] @$s13serialization6branchyS2f_SbtFTJpSUpSr :
// PACKAGE:         switch_enum
// PACKAGE:         = apply %[[#]](%[[#]]) : $@callee_guaranteed (Float) -> (Float, Float)

// SPECIALIZED-LABEL: {{^}}// reverse-mode derivative of branch(_:_:)
// SPECIALIZED:         // function_ref specialized pullback of branch(_:_:)
// SPECIALIZED-NEXT:    %[[#PULLBACK:]] = function_ref
// SPECIALIZED:         = partial_apply {{.*}} %[[#PULLBACK]](
// SPECIALIZED-LABEL: {{^}}// specialized pullback of branch(_:_:)
// SPECIALIZED:         switch_enum
// SPECIALIZED-NOT:     = apply {{.*}} : $@callee_guaranteed
// SPECIALIZED:       } // end sil function
#if !PACKAGE
@inlinable
#endif
@differentiable(reverse, wrt: x)
public func branch(_ x: Float, _ flag: Bool) -> Float {
  if flag { return x * x }
  return x + x
}
