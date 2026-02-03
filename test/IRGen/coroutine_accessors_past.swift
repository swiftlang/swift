// RUN: %target-swift-emit-irgen  \
// RUN:    %s                     \
// RUN: | %IRGenFileCheck %s

// Simple struct available before CoroutineAccessors feature is present:

// We have to emit a `_modify` to support old clients, and
// a `yielding mutate` so we can migrate to the more efficient form.

public struct OhOh {
    public var g: Int = 0
}

// Verify that we emit a get, set, `_modify`, and `yielding mutate` implementation:

// Variable initialization expression
// CHECK: define swiftcc i64 @"$s24coroutine_accessors_past02OhD0V1gSivpfi"() #0

// Getter definition
// CHECK: define swiftcc i64 @"$s24coroutine_accessors_past02OhD0V1gSivg"(i64 %0) #0

// Setter definition
// CHECK: define swiftcc void @"$s24coroutine_accessors_past02OhD0V1gSivs"(i64 %0, ptr swiftself captures(none) dereferenceable(8) %1) #0

// _modify definition
// CHECK: define swiftcc { ptr, ptr } @"$s24coroutine_accessors_past02OhD0V1gSivM"(ptr noalias dereferenceable(32) %0, ptr swiftself captures(none) dereferenceable(8) %1) #1

// yielding mutate definition
// CHECK: define swiftcorocc { ptr, ptr } @"$s24coroutine_accessors_past02OhD0V1gSivx"(ptr noalias %0, ptr swiftcoro %1, ptr swiftself captures(none) dereferenceable(8) %2) #1

