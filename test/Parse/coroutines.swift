// RUN: %target-typecheck-verify-swift

@yield_once func coro(_ x : inout Float) yields (inout Float) -> () {
  var _x = Float(0.0)
  yield &_x
  x = Float(_x)
}

func retCoro() -> @yield_once (inout Float) yields (inout Float) -> () {
  return coro
}

@yield_once func coroWithResult(_ x : Float) yields (inout Float) -> Float {
  var _x = Float(0.0)
  yield &_x
  return _x
}

func retCoroWithResult() -> @yield_once (Float) yields (inout Float) -> (Float) {
  return coroWithResult
}

@yield_once func coroGen<T>(_ x : inout T) yields (inout T) -> () {
  var _x = x
  yield &_x
  x = _x
}

func retGenCoro<T>() -> @yield_once (inout T) yields (inout T) -> () {
  return coroGen
}

@yield_once func coroGenWithResult<T>(_ x : T) yields (inout T) -> T {
  var _x = x
  yield &_x
  return _x
}

func retCoroGenWithResult<T>() -> @yield_once (T) yields (inout T) -> T {
  return coroGenWithResult
}
