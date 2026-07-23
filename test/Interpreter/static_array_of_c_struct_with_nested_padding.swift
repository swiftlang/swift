// RUN: %empty-directory(%t)
// RUN: split-file %s %t --leading-lines

// RUN: %target-build-swift -O -wmo %t/main.swift -import-objc-header %t/Types.h -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test

//--- Types.h
#include <stdint.h>

// `Nested` needs 4 bytes of trailing padding to round its 12 bytes of fields
// up to a multiple of its own 8-byte alignment (sizeof: 16). IRGen models that
// padding as an explicit `[4 x i8]` field in the storage type, but the type's
// explosion schema lists only the two scalar leaves.
typedef struct {
    int64_t a;
    int32_t b;
} Nested;

// `Outer` embeds `Nested` as its first field, so `c` starts at offset 16,
// right after `Nested`'s trailing padding. A schema-based constant emits only
// `Nested`'s 12 bytes of scalar leaves, dropping that padding and shifting all
// following fields down by 4 bytes.
typedef struct {
    Nested nested;
    uint32_t c;
    uint16_t d;
    uint16_t e;
    int8_t f;
    int8_t g;
    uint8_t h;
} Outer;

//--- main.swift

func makeOuter(c: UInt32, d: UInt16) -> Outer {
    Outer(nested: Nested(), c: c, d: d, e: 0, f: 0, g: 0, h: 0)
}

// A separate, non-inlinable function so the values are actually loaded from the
// static array rather than constant-folded at the use site.
@inline(never)
func consume(_ items: [Outer]) -> (UInt32, UInt16) {
    (items[0].c, items[0].d)
}

let items = [
    makeOuter(c: 72, d: 45),
    makeOuter(c: 0, d: 30),
    makeOuter(c: 65, d: 0),
]

let (c, d) = consume(items)
print("c=\(c) d=\(d)")
// CHECK: c=72 d=45
