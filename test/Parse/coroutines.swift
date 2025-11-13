// RUN: %target-typecheck-verify-swift

@yield_once func coro(_ x : inout Float) -> inout @yields Float {
  var _x = Float(0.0)
  yield &_x
  x = Float(_x)
}

func retCoro() -> @yield_once (_ x : inout Float) -> inout @yields Float {
  return coro
}

@yield_once func coroWithResult(_ x : Float) -> (yield: inout @yields Float, result: Float) {
  var _x = Float(0.0)
  yield &_x
  return _x
}

func retCoroWithResult() -> @yield_once (_ x : Float) -> (inout @yields Float, Float) {
  return coroWithResult
}

@yield_once func coroGen<T>(_ x : inout T) -> inout @yields T {
  var _x = x
  yield &_x
  x = _x
}

func retGenCoro<T>() -> @yield_once (_ x : inout T) -> inout @yields T {
  return coroGen
}

@yield_once func coroGenWithResult<T>(_ x : T) -> (yield: inout @yields T, result: T) {
  var _x = x
  yield &_x
  return _x
}

func retCoroGenWithResult<T>() -> @yield_once (_ x : T) -> (inout @yields T, T) {
  return coroGenWithResult
}
