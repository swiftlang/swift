// RUN: %target-swift-frontend \
// RUN:     -c \
// RUN:     -verify \
// RUN:     -disable-availability-checking \
// RUN:     -swift-version 6 \
// RUN:     %s

typealias A = InlineArray<2, UInt64>
func mix(_ a: inout A) {
    swap(&a[0], &a[1]) // expected-error{{overlapping accesses}}
                       // expected-note@-1{{conflicting access}}
}
