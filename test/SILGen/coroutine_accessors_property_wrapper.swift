// Property-wrapper-backed storage gets a synthesized write coroutine.  Under
// CoroutineAccessors that coroutine is `yielding mutate` (yield_once_2); the
// legacy `_modify` (yield_once) is only emitted additively for storage that
// needs a stable ABI, so a read-modify-write must not be lowered through
// `_modify` in general.  Naming it unconditionally used to crash SILGen, which
// dereferenced the absent accessor while computing the base access kind.

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

// RUN: %target-swift-emit-silgen %s                          \
// RUN:     -enable-experimental-feature CoroutineAccessors   \
// RUN:     -enable-library-evolution                         \
// RUN:     -module-name m                                    \
// RUN:   | %FileCheck %s --check-prefixes=RESILIENT,RESILIENT-%target-abi-stability

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
// FRAGILE-DAG: sil{{.*}} @$s1m7FragileV5valueSivx : $@yield_once_2
// FRAGILE-DAG: sil{{.*}} @$s1m9ResilientV5valueSivx : $@yield_once_2

// A read-modify-write of the wrapped property goes through that coroutine.
// CALLSITE-LABEL: sil hidden [ossa] @$s1m13modifyFragileyyAA0B0VzF :
// CALLSITE:         [[MUTATE:%.*]] = function_ref @$s1m7FragileV5valueSivx : $@yield_once_2
// CALLSITE:         ({{%.*}}, {{%.*}}, {{%.*}}) = begin_apply [[MUTATE]]

// Non-resilient: no legacy yield_once accessor for either property.
// NO-OLD-ABI-NOT: @$s1m7FragileV5valueSivM
// NO-OLD-ABI-NOT: @$s1m9ResilientV5valueSivM

// Resilient: the new coroutine is still the written one for both properties.
// RESILIENT-DAG: sil{{.*}} @$s1m7FragileV5valueSivx : $@yield_once_2
// RESILIENT-DAG: sil{{.*}} @$s1m9ResilientV5valueSivx : $@yield_once_2

// The legacy `_modify` is emitted additively for the public property, but only
// on an ABI-stable platform.
// RESILIENT-stable-DAG: sil{{.*}} @$s1m9ResilientV5valueSivM : $@yield_once @convention

// ...whereas internal storage never gets one.
// NO-OLD-ABI-RESILIENT-NOT: @$s1m7FragileV5valueSivM

// On a non-ABI-stable platform this invocation has nothing extra to verify;
// give the "unstable" prefix a trivial match so FileCheck doesn't reject an
// entirely-unmatched --check-prefix.
// RESILIENT-unstable-DAG: sil{{.*}} @$s1m9ResilientV5valueSivx : $@yield_once_2
