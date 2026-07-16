// RUN: %target-swift-emit-irgen  \
// RUN:    -enable-experimental-feature CoroutineAccessors  \
// RUN:    %s                     \
// RUN: | %IRGenFileCheck %s

// Also verify the old `_modify` (yield_once_1) accessor is NOT emitted.
// RUN: %target-swift-emit-irgen  \
// RUN:    -enable-experimental-feature CoroutineAccessors  \
// RUN:    %s                     \
// RUN: | %FileCheck %s --check-prefix=NO-OLD-ABI

// REQUIRES: swift_feature_CoroutineAccessors

// REQUIRES: OS_FAMILY=darwin || OS=linux-gnu
// REQUIRES: PTRSIZE=64

// Simple struct that predates the CoroutineAccessors feature, but built WITHOUT
// library evolution: there is no stable ABI to preserve, so we emit only the new
// `yielding mutate` (yield_once_2) accessor and NOT the old `_modify`
// (yield_once_1) one.  See coroutine_accessors_past_resilient.swift for the
// library-evolution counterpart, which must keep the old `_modify`.

public struct OhOh {
    public var g: Int = 0
}

// Verify that we emit a get, set, and `yielding mutate` implementation:

// Variable initialization expression
// CHECK: define{{.*}} swiftcc i64 @"$s24coroutine_accessors_past02OhD0V1gSivpfi"() #0

// Getter definition
// CHECK: define{{.*}} swiftcc i64 @"$s24coroutine_accessors_past02OhD0V1gSivg"(i64 %0) #0

// Setter definition
// CHECK: define{{.*}} swiftcc void @"$s24coroutine_accessors_past02OhD0V1gSivs"(i64 %0, ptr swiftself captures(none) dereferenceable(8) %1) #0

// yielding mutate definition
// CHECK: define{{.*}} {{(swiftcc|swiftcorocc)}} { ptr, ptr } @"$s24coroutine_accessors_past02OhD0V1gSivx"(ptr noalias %0, ptr{{( swiftcoro)?}} %1, ptr swiftself captures(none) dereferenceable(8) %2) #1

// The old `_modify` (yield_once_1) accessor must not appear anywhere.
// NO-OLD-ABI-NOT: @"$s24coroutine_accessors_past02OhD0V1gSivM"
