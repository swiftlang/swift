// A 'yielding borrow' whose yielded value has a scoped (borrow) lifetime
// dependence borrows its source, so -- like a noncopyable value -- it does NOT
// synthesize an owned getter, even though the value type is copyable.  (Without
// the scoped-lifetime check this would fall through to the copyable-getter case
// and synthesize an invalid getter that returns a borrowed value as owned.)

// RUN: %target-swift-emit-silgen %s                          \
// RUN:     -enable-experimental-feature CoroutineAccessors   \
// RUN:     -enable-experimental-feature Lifetimes            \
// RUN:     -enable-callee-allocated-coro-abi                 \
// RUN:     -module-name m                                    \
// RUN:   | %FileCheck %s

// RUN: %target-swift-emit-silgen %s                          \
// RUN:     -enable-experimental-feature CoroutineAccessors   \
// RUN:     -enable-experimental-feature Lifetimes            \
// RUN:     -enable-callee-allocated-coro-abi                 \
// RUN:     -enable-library-evolution                         \
// RUN:     -module-name m                                    \
// RUN:   | %FileCheck %s

// A lone CHECK-NOT (own invocation) scans the whole input for the absent getter.
// RUN: %target-swift-emit-silgen %s                          \
// RUN:     -enable-experimental-feature CoroutineAccessors   \
// RUN:     -enable-experimental-feature Lifetimes            \
// RUN:     -module-name m                                    \
// RUN:   | %FileCheck %s --check-prefix=NO-GETTER

// RUN: %target-swift-emit-silgen %s                          \
// RUN:     -enable-experimental-feature CoroutineAccessors   \
// RUN:     -enable-experimental-feature Lifetimes            \
// RUN:     -enable-library-evolution                         \
// RUN:     -module-name m                                    \
// RUN:   | %FileCheck %s --check-prefix=NO-GETTER

// REQUIRES: swift_feature_CoroutineAccessors
// REQUIRES: swift_feature_Lifetimes

public struct NE: ~Escapable {
  @_lifetime(immortal) public init() {}
}

public struct Wrapper: ~Escapable {
  var _ne: NE
  @_lifetime(copy ne) public init(ne: NE) { self._ne = ne }
  public var ne: NE {
    @_lifetime(borrow self)
    yielding borrow { yield _ne }
  }
}

// The yielding_borrow coroutine is emitted, but no getter (a scoped-borrow
// dependence cannot be handed back as an owned value).
// CHECK-DAG: sil{{.*}} @$s1m7WrapperV2neAA2NEVvy : $@yield_once_2
// NO-GETTER-NOT: @$s1m7WrapperV2neAA2NEVvg
