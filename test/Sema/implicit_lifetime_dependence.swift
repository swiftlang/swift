// RUN: %target-typecheck-verify-swift -enable-experimental-feature LifetimeDependence

// REQUIRES: swift_feature_LifetimeDependence

struct BufferView : ~Escapable, ~Copyable {
  let ptr: UnsafeRawBufferPointer?
  let c: Int
  @lifetime(borrow ptr)
  init(_ ptr: UnsafeRawBufferPointer?, _ c: Int) {
    self.ptr = ptr
    self.c = c
  }
}

struct ImplicitInit1 : ~Escapable {
  let ptr: UnsafeRawBufferPointer
}

struct ImplicitInit2 : ~Escapable, ~Copyable {
  let mbv: BufferView
}

struct ImplicitInit3 : ~Escapable, ~Copyable {
  let mbv1: BufferView
  let mbv2: BufferView
}

func foo1() -> BufferView { // expected-error{{a function with a ~Escapable result needs a parameter to depend on}}
  // expected-note@-1{{'@lifetime(immortal)' can be used to indicate that values produced by this initializer have no lifetime dependencies}}
  return BufferView(nil, 0)
}

func foo2(_ i: borrowing Int) -> BufferView {
  return BufferView(nil, 0)
}

func foo3<T: BitwiseCopyable>(arg: borrowing T) -> BufferView {
  return BufferView(nil, 0)
}

