// RUN: %target-swift-emit-irgen  \
// RUN:    -enable-experimental-feature CoroutineAccessors  \
// RUN:    -enable-library-evolution  \
// RUN:    %s                     \
// RUN: | %IRGenFileCheck %s

// REQUIRES: swift_feature_CoroutineAccessors

// REQUIRES: OS_FAMILY=darwin || OS=linux-gnu
// REQUIRES: PTRSIZE=64

// The library-evolution counterpart of coroutine_accessors_past.swift.  Here the
// struct predates the CoroutineAccessors feature AND has a stable ABI to
// preserve, so we must emit both the old `_modify` (yield_once_1) accessor to
// support old clients and the new `yielding mutate` (yield_once_2) accessor so
// clients can migrate to the more efficient form.

public struct OhOh {
    public var g: Int = 0
}

// Verify that we emit a get, set, `_modify`, and `yielding mutate` implementation.
// (Note the resilient signatures pass `self` opaquely by pointer.)

// Getter definition
// CHECK: define{{.*}} swiftcc i64 @"$s34coroutine_accessors_past_resilient02OhE0V1gSivg"(ptr noalias swiftself captures(none) dereferenceable(8) %0) #{{[0-9]+}}

// Setter definition
// CHECK: define{{.*}} swiftcc void @"$s34coroutine_accessors_past_resilient02OhE0V1gSivs"(i64 %0, ptr swiftself captures(none) dereferenceable(8) %1) #{{[0-9]+}}

// _modify definition (old yield_once_1 ABI, kept for ABI stability)
// CHECK: define{{.*}} swiftcc { ptr, ptr } @"$s34coroutine_accessors_past_resilient02OhE0V1gSivM"(ptr noalias dereferenceable(32) %0, ptr swiftself captures(none) dereferenceable(8) %1) #{{[0-9]+}}

// yielding mutate definition (new yield_once_2 ABI)
// CHECK: define{{.*}} {{(swiftcc|swiftcorocc)}} { ptr, ptr } @"$s34coroutine_accessors_past_resilient02OhE0V1gSivx"(ptr noalias %0, ptr{{( swiftcoro)?}} %1, ptr swiftself captures(none) dereferenceable(8) %2) #{{[0-9]+}}
