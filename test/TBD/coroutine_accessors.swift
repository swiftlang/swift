// RUN: %target-build-swift-dylib(%t/%target-library-name(thing)) \
// RUN:     %s                                                    \
// RUN:     -Xfrontend -enable-callee-allocated-coro-abi          \
// RUN:     -emit-tbd                                             \
// RUN:     -Xfrontend -validate-tbd-against-ir=all               \
// RUN:     -enable-library-evolution                             \
// RUN:     -Xfrontend -tbd-install_name -Xfrontend thing         \
// RUN:     -emit-module                                          \
// RUN:     -module-name thing                                    \
// RUN:     -enable-experimental-feature CoroutineAccessors

// REQUIRES: swift_feature_CoroutineAccessors

public struct S {}

public protocol P {
  associatedtype A

  var s: S { yielding borrow set }
  var s2: S { yielding borrow set }
}

public protocol Q : P {
  override var s: S { yielding borrow set }
  var s2: S { yielding borrow set }
}
