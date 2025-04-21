// RUN: %target-typecheck-verify-swift \
// RUN:   -enable-experimental-feature LifetimeDependence \
// RUN:   -enable-experimental-feature CoroutineAccessors

// REQUIRES: swift_feature_LifetimeDependence
// REQUIRES: swift_feature_CoroutineAccessors

struct NE<T : ~Copyable & ~Escapable> : ~Copyable & ~Escapable {
  @lifetime(&t)
  init(
    t: inout T
  )
  {
  }
}

struct S : ~Copyable & ~Escapable {
  var mutableBytes: NE<S> {
    @lifetime(&self)
    mutating get {
      return NE(t: &self)
    }
  }
}
