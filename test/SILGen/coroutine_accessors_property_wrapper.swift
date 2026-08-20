// Property-wrapper-backed storage gets a synthesized write coroutine.  Under
// CoroutineAccessors that coroutine is `yielding mutate` (mangled `vx`); the
// legacy `_modify` (mangled `vM`) is only emitted additively for storage that
// needs a stable ABI, so a read-modify-write must not be lowered through
// `_modify` in general.  Naming it unconditionally used to crash SILGen, which
// dereferenced the absent accessor while computing the base access kind.
//
// The checks below discriminate the two accessors by mangled suffix rather than
// by lowered convention: whether a yielding accessor lowers to `@yield_once` or
// `@yield_once_2` is a platform-dependent default, and in a resilient build
// without the callee-allocated coro ABI *both* accessors lower to `@yield_once`.
// See coroutine_accessors_yielding_synthesis.swift for that ABI matrix.

// RUN: %target-swift-emit-silgen %s                          \
// RUN:     -enable-experimental-feature CoroutineAccessors   \
// RUN:     -module-name m                                    \
// RUN:   | %FileCheck %s --check-prefix=FRAGILE

// Check the read-modify-write site itself (separate invocation: these checks are
// ordered within one function, so they must not share a prefix with the
// order-independent whole-module checks above).
// RUN: %target-swift-emit-silgen %s                          \
// RUN:     -enable-experimental-feature CoroutineAccessors   \
// RUN:     -module-name m                                    \
// RUN:   | %FileCheck %s --check-prefix=CALLSITE

// Confirm the old accessors are absent altogether (separate invocation so a
// lone CHECK-NOT scans the whole input).
// RUN: %target-swift-emit-silgen %s                          \
// RUN:     -enable-experimental-feature CoroutineAccessors   \
// RUN:     -module-name m                                    \
// RUN:   | %FileCheck %s --check-prefix=NO-OLD-ABI

// The RESILIENT prefix carries the platform-independent checks; the additive
// legacy `_modify` is emitted for public resilient storage only on an ABI-stable
// platform, so that check rides along under a second prefix.  On other platforms
// OLD-ABI-unstable simply goes unused, which is allowed because the invocation's
// other prefix still contributes directives.
// RUN: %target-swift-emit-silgen %s                          \
// RUN:     -enable-experimental-feature CoroutineAccessors   \
// RUN:     -enable-library-evolution                         \
// RUN:     -module-name m                                    \
// RUN:   | %FileCheck %s --check-prefixes=RESILIENT,OLD-ABI-%target-abi-stability

// Internal storage has no ABI to keep stable, so it gets no legacy accessor even
// under library evolution (separate invocation for the lone CHECK-NOT).
// RUN: %target-swift-emit-silgen %s                          \
// RUN:     -enable-experimental-feature CoroutineAccessors   \
// RUN:     -enable-library-evolution                         \
// RUN:     -module-name m                                    \
// RUN:   | %FileCheck %s --check-prefix=NO-OLD-ABI-RESILIENT

// REQUIRES: swift_feature_CoroutineAccessors

@propertyWrapper
struct Wrapper<Value> {
  var wrappedValue: Value
}

struct Fragile {
  @Wrapper var value: Int = 0
}

func modifyFragile(_ f: inout Fragile) {
  f.value += 1
}

@propertyWrapper
public struct PublicWrapper<Value> {
  public var wrappedValue: Value
  public init(wrappedValue: Value) { self.wrappedValue = wrappedValue }
}

public struct Resilient {
  @PublicWrapper public var value: Int = 0
}

public func modifyResilient(_ r: inout Resilient) {
  r.value += 1
}

// The wrapper's synthesized write coroutine is `yielding mutate`.
// FRAGILE-DAG: sil{{.*}} @$s1m7FragileV5valueSivx :
// FRAGILE-DAG: sil{{.*}} @$s1m9ResilientV5valueSivx :

// A read-modify-write of the wrapped property goes through that coroutine.  The
// yielded-result arity varies with the lowered convention, so only the callee is
// pinned here.
// CALLSITE-LABEL: sil hidden [ossa] @$s1m13modifyFragileyyAA0B0VzF :
// CALLSITE:         [[MUTATE:%.*]] = function_ref @$s1m7FragileV5valueSivx :
// CALLSITE:         begin_apply [[MUTATE]]

// Non-resilient: no legacy accessor for either property.
// NO-OLD-ABI-NOT: @$s1m7FragileV5valueSivM
// NO-OLD-ABI-NOT: @$s1m9ResilientV5valueSivM

// Resilient: the new coroutine is still the written one for both properties.
// RESILIENT-DAG: sil{{.*}} @$s1m7FragileV5valueSivx :
// RESILIENT-DAG: sil{{.*}} @$s1m9ResilientV5valueSivx :

// Additive legacy `_modify`, ABI-stable platforms only.
// OLD-ABI-stable-DAG: sil{{.*}} @$s1m9ResilientV5valueSivM :

// NO-OLD-ABI-RESILIENT-NOT: @$s1m7FragileV5valueSivM
