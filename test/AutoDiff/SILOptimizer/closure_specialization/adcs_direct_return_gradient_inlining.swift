// RUN: %target-swift-frontend -O -sil-verify-all -parse-as-library -module-name Residual -emit-sil %s | %FileCheck %s

import _Differentiation

public protocol ScaleProvider {
  func coefficient() -> Float
}

@differentiable(reverse, wrt: x)
public func leaf(_ x: Float, _ provider: any ScaleProvider) -> Float {
  let c = provider.coefficient()
  return c * x * x
}

@differentiable(reverse, wrt: x)
public func outer(_ x: inout Float, _ flag: Bool, _ provider: any ScaleProvider) {
  if flag { x = -x }
  x = leaf(x, provider)
  x = leaf(x, provider)
  x = leaf(x, provider)
  x = leaf(x, provider)
}

// Inline the outer VJP into the gradient before the four leaf VJPs to keep
// the outer VJP within budget and let the optimizer eliminate the pullback contexts.
@inline(never)
public func full(_ x: Float, _ flag: Bool, _ provider: any ScaleProvider) -> Float {
  gradient(at: x) { x in
    var y = x
    outer(&y, flag, provider)
    return y
  }
}

// CHECK-LABEL:   {{^}}sil [noinline] @$s8Residual4full
// CHECK-NOT:       function_ref {{.*}}TJr
// CHECK-NOT:       function_ref {{.*}}TJp
// CHECK-NOT:       apply {{.*}} : $@callee_guaranteed
// CHECK-NOT:       partial_apply
// CHECK-COUNT-4: witness_method {{.*}} #ScaleProvider.coefficient
// CHECK-NOT:       function_ref {{.*}}TJr
// CHECK-NOT:       function_ref {{.*}}TJp
// CHECK-NOT:       apply {{.*}} : $@callee_guaranteed
// CHECK-NOT:       partial_apply
// CHECK:         } // end sil function

@differentiable(reverse, wrt: x)
public func weightedLeaf(_ x: Float, _ provider: any ScaleProvider) -> Float {
  let c = provider.coefficient()
  var y = c * x * x
  y += provider.coefficient() * x
  y += provider.coefficient() * x
  y += provider.coefficient() * x
  return y
}

@differentiable(reverse, wrt: x)
public func weighted(_ x: inout Float, _ flag: Bool, _ provider: any ScaleProvider) {
  if flag { x = -x }
  x = weightedLeaf(x, provider)
}

// Even one larger leaf VJP can exhaust the outer VJP's inlining budget here.
@inline(never)
public func fullWeighted(_ x: Float, _ flag: Bool, _ provider: any ScaleProvider) -> Float {
  gradient(at: x) { x in
    var y = x
    weighted(&y, flag, provider)
    return y
  }
}

// CHECK-LABEL:   {{^}}sil [noinline] @$s8Residual12fullWeighted
// CHECK-NOT:       function_ref {{.*}}TJr
// CHECK-NOT:       function_ref {{.*}}TJp
// CHECK-NOT:       apply {{.*}} : $@callee_guaranteed
// CHECK-NOT:       partial_apply
// CHECK-COUNT-4: witness_method {{.*}} #ScaleProvider.coefficient
// CHECK-NOT:       function_ref {{.*}}TJr
// CHECK-NOT:       function_ref {{.*}}TJp
// CHECK-NOT:       apply {{.*}} : $@callee_guaranteed
// CHECK-NOT:       partial_apply
// CHECK:         } // end sil function
