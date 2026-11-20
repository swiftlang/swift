// RUN: %target-swift-frontend -O -sil-verify-all -parse-as-library -module-name Inliner -emit-ir %s -o - | %FileCheck %s

import _Differentiation

public protocol Provider {
  func coefficient() -> Float
}

@differentiable(reverse, wrt: x)
public func leaf(_ x: Float, _ provider: any Provider) -> Float {
  x * x * provider.coefficient()
}

@differentiable(reverse, wrt: x)
public func child(_ x: Float, _ flag: Bool, _ provider: any Provider) -> Float {
  if flag { return leaf(x, provider) }
  let y = leaf(x, provider)
  return y + y
}

@differentiable(reverse, wrt: x)
public func parent(_ x: Float, _ flag: Bool, _ provider: any Provider) -> Float {
  var x = x
  x = child(x, flag, provider)
  x = child(x, flag, provider)
  x = child(x, flag, provider)
  return x
}

// The witness call keeps the leaf VJP out of early inlining. The second
// ADCS run can specialize its captures once mid-level inlining exposes them.
// CHECK-LABEL: define {{.*}}4fully{{[^ (]*}}(
// CHECK-NOT:   swift_allocObject
// CHECK-NOT:   call {{.*}}TJ{{[pr]}}
// CHECK:       }
@inline(never)
public func full(_ x: Float, _ flag: Bool, _ provider: any Provider) -> Float {
  gradient(at: x) { parent($0, flag, provider) }
}
