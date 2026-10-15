// RUN: %target-swift-emit-silgen -module-name coro %s -enable-experimental-feature CoroutineFunctions | %FileCheck %s

// REQUIRES: swift_feature_CoroutineFunctions

// CHECK: @yield_once func coro(_ x: inout Float) yields (inout Float)
// CHECK: func retCoro() -> @yield_once (inout Float) yields (inout Float) -> ()
// CHECK: @yield_once func coroWithResult(_ x: Float) yields (inout Float) -> Float
// CHECK: func retCoroWithResult() -> @yield_once (Float) yields (inout Float) -> Float
// CHECK: @yield_once func coroGen<T>(_ x: inout T) yields (inout T)
// CHECK: func retGenCoro<T>() -> @yield_once (inout T) yields (inout T) -> ()
// CHECK: @yield_once func coroGenWithResult<T>(_ x: T) yields (inout T) -> T
// CHECK: func retCoroGenWithResult<T>() -> @yield_once (T) yields (inout T) -> T

// CHECK-LABEL: sil hidden [ossa] @$s4coroAAyySfzXySfzF : $@yield_once @convention(thin) (@inout Float) -> @yields @inout Float {
@yield_once func coro(_ x : inout Float) yields (inout Float) -> () {
  var _x = Float(0.0)
  yield &_x
  x = Float(_x)
}

// CHECK-LABEL: sil hidden [ossa] @$s4coro7retCoroySfzXySfzcyF : $@convention(thin) () -> @owned @yield_once @callee_guaranteed (@inout Float) -> @yields @inout Float {
func retCoro() -> @yield_once (inout Float) yields (inout Float) -> () {
  return coro
}

// CHECK-LABEL: sil hidden [ossa] @$s4coro0A10WithResultyS2fzXySfF : $@yield_once @convention(thin) (Float) -> (@yields @inout Float, Float) {
@yield_once func coroWithResult(_ x : Float) yields (inout Float) -> Float {
  var _x = Float(0.0)
  yield &_x
  return _x
}

// CHECK-LABEL: sil hidden [ossa] @$s4coro17retCoroWithResultS2fzXySfcyF : $@convention(thin) () -> @owned @yield_once @callee_guaranteed (Float) -> (@yields @inout Float, Float) {
func retCoroWithResult() -> @yield_once (Float) yields (inout Float) -> Float {
  return coroWithResult
}

// CHECK-LABEL: sil hidden [ossa] @$s4coro0A3GenyyxzXyxzlF : $@yield_once @convention(thin) <T> (@inout T) -> @yields @inout T {
@yield_once func coroGen<T>(_ x : inout T) yields (inout T) -> () {
  var _x = x
  yield &_x
  x = _x
}

// CHECK-LABEL: sil hidden [ossa] @$s4coro10retGenCoroyxzXyxzcylF : $@convention(thin) <T> () -> @owned @yield_once @callee_guaranteed @substituted <τ_0_0, τ_0_1> (@inout τ_0_0) -> @yields @inout τ_0_1 for <T, T> {
func retGenCoro<T>() -> @yield_once (inout T) yields (inout T) -> () {
  return coroGen
}

// CHECK-LABEL: sil hidden [ossa] @$s4coro0A13GenWithResultyxxzXyxlF : $@yield_once @convention(thin) <T> (@in_guaranteed T) -> (@yields @inout T, @out T) {
@yield_once func coroGenWithResult<T>(_ x : T) yields (inout T) -> T {
  var _x = x
  yield &_x
  return _x
}

// CHECK-LABEL: sil hidden [ossa] @$s4coro20retCoroGenWithResultxxzXyxcylF : $@convention(thin) <T> () -> @owned @yield_once @callee_guaranteed @substituted <τ_0_0, τ_0_1, τ_0_2> (@in_guaranteed τ_0_0) -> (@yields @inout τ_0_1, @out τ_0_2) for <T, T, T> {
func retCoroGenWithResult<T>() -> @yield_once (T) yields (inout T) -> T {
  return coroGenWithResult
}

// Real-world case: yield a value and return a pullback closure (coroutine itself)
struct S {
  private var _x : Float

  var x: Float {
    get{_x}
    _modify { yield &_x }
  }
}

extension S {
  struct TangentVector : AdditiveArithmetic {
    @_hasStorage var _x: Float
    typealias TangentVector = S.TangentVector
    static func + (lhs: S.TangentVector, rhs: S.TangentVector) -> S.TangentVector { fatalError() }
    static func - (lhs: S.TangentVector, rhs: S.TangentVector) -> S.TangentVector { fatalError() }    
    static var zero: S.TangentVector = TangentVector(_x : 0)
  }

  // CHECK-LABEL: sil hidden [ossa] @$s4coro1SV9vjpModifyySfzXyAC13TangentVectorVzcSfzXyyF : $@yield_once @convention(method) (@inout S) -> (@yields @inout Float, @owned @yield_once @callee_guaranteed (@inout S.TangentVector) -> @yields @inout Float) {
  @yield_once
  mutating func vjpModify() yields (inout Float) -> @yield_once (inout S.TangentVector) yields (inout Float) -> () {
    yield &x

    @yield_once func pb(_ x : inout S.TangentVector) yields (inout Float) -> () {
      var _x = Float(0.0)
      yield &_x
      x = S.TangentVector(_x : _x)
    }

    // CHECK: %[[PB:.*]] = function_ref @$s4coro1SV9vjpModifyySfzXyAC13TangentVectorVzcSfzXyyF2pbL_yySfzXyAFzF : $@yield_once @convention(thin) (@inout S.TangentVector) -> @yields @inout Float
    // CHECK: %[[FNCONV:.*]] = thin_to_thick_function %[[PB]] to $@yield_once @callee_guaranteed (@inout S.TangentVector) -> @yields @inout Float
    // CHECK: return %[[FNCONV]]
    return pb
  }
}
