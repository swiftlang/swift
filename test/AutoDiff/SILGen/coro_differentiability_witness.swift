// RUN: %target-swift-frontend -emit-silgen -verify -enable-experimental-feature CoroutineFunctions %s | %FileCheck %s

// REQUIRES: swift_feature_CoroutineFunctions

import _Differentiation

struct StructWithModifyAccessor {
  private var _x : Float

  var x: Float {
    get { _x }
    _modify { yield &_x }
  }
}

extension StructWithModifyAccessor : Differentiable {
  @yield_once
  @derivative(of: x._modify)
  mutating func vjpModify() yields (inout Float) -> @yield_once (inout TangentVector) yields (inout Float) -> () {
    yield &x

    @yield_once func pb(_ x : inout TangentVector) yields (inout Float) -> () {
      var _x = Float(0.0)
      yield &_x
      x = TangentVector(_x : _x)
    }

    return pb
  }
}

// CHECK-LABEL: // differentiability witness for StructWithModifyAccessor.x.modify
// CHECK-NEXT: sil_differentiability_witness hidden [reverse] [parameters 0] [results 0 1] @$s30coro_differentiability_witness24StructWithModifyAccessorV1xSfvM : $@yield_once @convention(method) (@inout StructWithModifyAccessor) -> @yields @inout Float {
// CHECK-NEXT:  vjp
// CHECK-NEXT: }

// CHECK-NOT: // differentiability witness for StructWithModifyAccessor.x.modify


// TODO: Enable then thunked coroutine linear maps will be added
/*
struct GenStructWithModifyAccessor<T> {
  private var _x : T

  var x: T {
    get { _x }
    _modify { yield &_x }
  }
}

extension GenStructWithModifyAccessor : Differentiable where T : Differentiable, T == T.TangentVector {
  @yield_once
  @derivative(of: x._modify)
  mutating func vjpModify() yields (inout T) -> @yield_once (inout TangentVector) yields (inout T.TangentVector) -> () {
    yield &x

    @yield_once func pb(_ x : inout TangentVector) yields (inout T) -> () {
      var _x : T.TangentVector
      yield &_x
      x = TangentVector(_x : _x)
    }

    return pb
  }
}
*/
