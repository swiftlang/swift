// RUN: %target-typecheck-verify-swift \
// RUN:   -enable-experimental-feature Lifetimes \
// RUN:   -enable-experimental-feature CoroutineAccessors

// REQUIRES: swift_feature_Lifetimes
// REQUIRES: swift_feature_CoroutineAccessors

struct NE<T : ~Copyable & ~Escapable> : ~Copyable & ~Escapable {
  @_lifetime(&t)
  init(
    t: inout T
  )
  {
  }
}

struct S : ~Copyable & ~Escapable {
  var mutableBytes: NE<S> {
    @_lifetime(&self)
    mutating get {
      return NE(t: &self)
    }
  }
}
