// RUN: %target-swift-frontend -emit-sil %s | %FileCheck %s

// Do not add other tests to this file, because differentiating other things
// can hide this bug.

// There was a bug where the AD pass would not see the custom [differentiable]
// attribute on `Float.*` wrt both parameters until after the AD pass generated
// its own adjoint for `Float.*` wrt the second parameter. This test verifies
// that the AD pass uses the custom adjoint defined on `Float.*`.

func mul3(_ x: Float) -> Float {
  return 3 * x
}

let _ = gradient(at: 0, in: mul3)

// CHECK-LABEL: sil{{.*}} @AD__{{.*}}mul3{{.*}}__primal{{.*}}
// CHECK: function_ref static Float._adjointMultiply(_:_:_:_:)
// CHECK: } // end sil function 'AD__{{.*}}mul3{{.*}}__primal{{.*}}'
