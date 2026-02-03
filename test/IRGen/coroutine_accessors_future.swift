// RUN: %target-swift-emit-irgen  \
// RUN:    %s                     \
// RUN: | %IRGenFileCheck %s

// Simple struct available well after CoroutineAccessors feature is present.
// We don't need old `_modify` accessors for this because no older
// client can possibly be trying to access it:

@available(macOS 999.99, *)
public struct OhOh {
    public var g: Int = 0
}

// Verify that we emit a get, set, and `yielding mutate` implementation:

// Getter definition
// CHECK: define swiftcc i64 @"$s26coroutine_accessors_future02OhD0V1gSivg"(i64 %0) #0

// Setter definition
// CHECK: define swiftcc void @"$s26coroutine_accessors_future02OhD0V1gSivs"(i64 %0, ptr swiftself captures(none) dereferenceable(8) %1) #0

// yielding mutate definition
// CHECK: define swiftcorocc { ptr, ptr } @"$s26coroutine_accessors_future02OhD0V1gSivx"(ptr noalias %0, ptr swiftcoro %1, ptr swiftself captures(none) dereferenceable(8) %2) #1

