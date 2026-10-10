// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_SafeInteropImplementations
// REQUIRES: executable_test

// XFAIL: swift_test_mode_optimize_none_with_opaque_values

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-build-swift -strict-memory-safety %t/impl.swift -I %t -target %target-swift-6.2-abi-triple \
// RUN:   -enable-experimental-feature SafeInteropImplementations -plugin-path %swift-plugin-dir -parse-as-library -c -o %t/impl.o
// RUN: %target-build-swift -strict-memory-safety %t/caller.swift -I %t -target %target-swift-6.2-abi-triple %t/impl.o -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out

// Call the generated C entry points directly from C

// RUN: %target-clang -x c -fexperimental-bounds-safety-attributes -I %t -c %t/caller.c -o %t/caller.o
// RUN: %target-build-swift %t/caller.o %t/impl.o -target %target-swift-6.2-abi-triple -o %t/c-caller.out
// RUN: %target-codesign %t/c-caller.out
// RUN: %target-run %t/c-caller.out | %FileCheck %s --check-prefix=C-CALLER

// C-CALLER: all C entry point checks passed

// DEFINE: %{expect-trap} = env SWIFT_BACKTRACE=enable=no %{python} %S/../../../Inputs/not.py

// C-TRAP-NEG: Precondition failed: buffer with negative count
// RUN: %{expect-trap} "%target-run %t/c-caller.out span-neg" 2>&1 | %FileCheck %s --check-prefix=C-TRAP-NEG
// RUN: %{expect-trap} "%target-run %t/c-caller.out span-nullable-neg" 2>&1 | %FileCheck %s --check-prefix=C-TRAP-NEG
// RUN: %{expect-trap} "%target-run %t/c-caller.out ubp-neg" 2>&1 | %FileCheck %s --check-prefix=C-TRAP-NEG
// RUN: %{expect-trap} "%target-run %t/c-caller.out ubp-nullable-nil-neg" 2>&1 | %FileCheck %s --check-prefix=C-TRAP-NEG
// RUN: %{expect-trap} "%target-run %t/c-caller.out mspan-neg" 2>&1 | %FileCheck %s --check-prefix=C-TRAP-NEG
// RUN: %{expect-trap} "%target-run %t/c-caller.out mubp-neg" 2>&1 | %FileCheck %s --check-prefix=C-TRAP-NEG
// RUN: %{expect-trap} "%target-run %t/c-caller.out raw-sum-neg" 2>&1 | %FileCheck %s --check-prefix=C-TRAP-NEG
// RUN: %{expect-trap} "%target-run %t/c-caller.out raw-fill-neg" 2>&1 | %FileCheck %s --check-prefix=C-TRAP-NEG
// RUN: %{expect-trap} "%target-run %t/c-caller.out ubp-nullable-nonnil-neg" 2>&1 | %FileCheck %s --check-prefix=C-TRAP-NEG

// C-TRAP-OR-NULL-NEG: Precondition failed: non-null buffer with negative count
// RUN: %{expect-trap} "%target-run %t/c-caller.out ornull-nonnil-neg" 2>&1 | %FileCheck %s --check-prefix=C-TRAP-OR-NULL-NEG

// C-TRAP-NULL-POS: Precondition failed: null buffer with non-zero count
// RUN: %{expect-trap} "%target-run %t/c-caller.out ubp-nullable-nil-pos" 2>&1 | %FileCheck %s --check-prefix=C-TRAP-NULL-POS

//--- module.modulemap
module CHeader {
  header "header.h"
}

//--- header.h
#define __counted_by(x) __attribute__((__counted_by__(x)))
#define __counted_by_or_null(x) __attribute__((__counted_by_or_null__(x)))
#define __sized_by(x) __attribute__((__sized_by__(x)))
#define __noescape __attribute__((__noescape__))

int span_sum(const int * _Nonnull __counted_by(len) __noescape x, int len);
int span_sum_nullable(const int * _Nullable __counted_by(len) __noescape x, int len);
int ubp_sum(const int * _Nonnull __counted_by(len) x, int len);
int ubp_sum_nullable(const int * _Nullable __counted_by(len) x, int len);
void mspan_fill(int * _Nonnull __counted_by(len) __noescape x, int len, int val);
void mubp_fill(int * _Nonnull __counted_by(len) x, int len, int val);
int ornull_ubp_sum(const int * _Nullable __counted_by_or_null(len) x, int len);
int raw_sum(const void * _Nonnull __sized_by(size) __noescape p, int size);
void raw_fill(void * _Nonnull __sized_by(size) __noescape p, int size, int val);

//--- impl.swift
import CHeader

@c @implementation
public func span_sum(_ x: Span<CInt>) -> CInt {
  var total: CInt = 0
  for i in x.indices { total += x[i] }
  return total
}

@c @implementation
public func span_sum_nullable(_ x: Span<CInt>) -> CInt {
  var total: CInt = 0
  for i in x.indices { total += x[i] }
  return total
}

@c @implementation
public func ubp_sum(_ x: UnsafeBufferPointer<CInt>) -> CInt {
  var total: CInt = 0
  for i in x.indices { total += unsafe x[i] }
  return total
}

@c @implementation
public func ubp_sum_nullable(_ x: UnsafeBufferPointer<CInt>) -> CInt {
  var total: CInt = 0
  for i in x.indices { total += unsafe x[i] }
  return total
}

@c @implementation
public func mspan_fill(_ x: inout MutableSpan<CInt>, _ val: CInt) {
  for i in x.indices { x[i] = val }
}

@c @implementation
public func mubp_fill(_ x: UnsafeMutableBufferPointer<CInt>, _ val: CInt) {
  for i in x.indices { unsafe x[i] = val }
}

@c @implementation
public func ornull_ubp_sum(_ x: UnsafeBufferPointer<CInt>?) -> CInt {
  guard let x = unsafe x else { return -1 }
  var total: CInt = 0
  for i in x.indices { total += unsafe x[i] }
  return total
}

@c @implementation
public func raw_sum(_ p: RawSpan) -> CInt {
  var total: CInt = 0
  p.withUnsafeBytes { bytes in
    for i in 0..<bytes.count { total += CInt(unsafe bytes[i]) }
  }
  return total
}

@c @implementation
public func raw_fill(_ p: inout MutableRawSpan, _ val: CInt) {
  p.withUnsafeMutableBytes { bytes in
    for i in 0..<bytes.count { unsafe bytes[i] = UInt8(truncatingIfNeeded: val) }
  }
}

//--- caller.swift
// Only imports the C header, but links against the exposed C function from
// impl.swift to rountrip: safe wrapper -> unsafe wrapper -> safe impl.

import StdlibUnittest
import CHeader

var Suite = TestSuite("SafeImplementationRuntime")

Suite.test("Span/basic") {
  let arr: [CInt] = [1, 2, 3, 4, 5]
  let result = arr.withUnsafeBufferPointer { buf -> CInt in
    unsafe CHeader.span_sum(buf.baseAddress!, CInt(buf.count))
  }
  expectEqual(result, 15)
}

Suite.test("Span/basic/via-wrapper") {
  let arr: [CInt] = [1, 2, 3, 4, 5]
  let result = CHeader.span_sum(arr.span)
  expectEqual(result, 15)
}

Suite.test("Span/empty") {
  let arr: [CInt] = []
  let result = arr.withUnsafeBufferPointer { buf -> CInt in
    unsafe CHeader.span_sum(buf.baseAddress!, 0)
  }
  expectEqual(result, 0)
}

Suite.test("Span/empty/via-wrapper") {
  let arr: [CInt] = []
  let result = CHeader.span_sum(arr.span)
  expectEqual(result, 0)
}

Suite.test("Span/empty/via-wrapper/InlineArray") {
  let arr: [0 of CInt] = []
  let result = CHeader.span_sum_nullable(arr.span)
  expectEqual(result, 0)
}

Suite.test("UBP/basic") {
  let arr: [CInt] = [1, 2, 3]
  let result = arr.withUnsafeBufferPointer { buf -> CInt in
    unsafe CHeader.ubp_sum(buf.baseAddress!, CInt(buf.count))
  }
  expectEqual(result, 6)
}

Suite.test("UBP/nullable/basic") {
  let arr: [CInt] = [5, 5, 5]
  let result = arr.withUnsafeBufferPointer { buf -> CInt in
    unsafe CHeader.ubp_sum_nullable(buf.baseAddress, CInt(buf.count))
  }
  expectEqual(result, 15)
}

Suite.test("UBP/nullable/empty") {
  let result: CInt = CHeader.ubp_sum_nullable(nil, 0)
  expectEqual(result, 0)
}

Suite.test("UBP/nullable/neg") {
  expectCrash {
    let _: CInt = CHeader.ubp_sum_nullable(nil, -1)
  }
}

Suite.test("MutableSpan/fill") {
  var arr: [CInt] = [0, 0, 0, 0]
  arr.withUnsafeMutableBufferPointer { buf in
    unsafe CHeader.mspan_fill(buf.baseAddress!, CInt(buf.count), 42)
  }
  expectEqual(arr, [42, 42, 42, 42])
}

Suite.test("MutableSpan/fill/via-wrapper") {
  var arr: [CInt] = [0, 0, 0, 0]
  var mspan = arr.mutableSpan
  CHeader.mspan_fill(&mspan, 42)
  expectEqual(arr, [42, 42, 42, 42])
}

Suite.test("MUBP/fill") {
  var arr: [CInt] = [0, 0, 0]
  arr.withUnsafeMutableBufferPointer { buf in
    unsafe CHeader.mubp_fill(buf.baseAddress!, CInt(buf.count), 11)
  }
  expectEqual(arr, [11, 11, 11])
}

Suite.test("countedByOrNull/UBP/basic") {
  let arr: [CInt] = [3, 3, 3]
  let result = arr.withUnsafeBufferPointer { buf -> CInt in
    unsafe CHeader.ornull_ubp_sum(buf.baseAddress, CInt(buf.count))
  }
  expectEqual(result, 9)
}

Suite.test("countedByOrNull/UBP/nil") {
  let result: CInt = CHeader.ornull_ubp_sum(nil, 0)
  expectEqual(result, -1)
}

Suite.test("countedByOrNull/UBP/nil/neg") {
  // A `_or_null` pointer may be null with any count
  let result: CInt = CHeader.ornull_ubp_sum(nil, -11)
  expectEqual(result, -1)
}

Suite.test("countedByOrNull/UBP/nonnil/neg") {
  let arr: [CInt] = [3, 3, 3]
  expectCrash {
    _ = arr.withUnsafeBufferPointer { unsafe CHeader.ornull_ubp_sum($0.baseAddress, -11) }
  }
}

Suite.test("Span/nonnil/neg") {
  // A negative count on the non-null Span path traps in the generated peer
  let arr: [CInt] = [1, 2, 3]
  expectCrash {
    _ = arr.withUnsafeBufferPointer { unsafe CHeader.span_sum($0.baseAddress!, -1) }
  }
}

Suite.test("RawSpan/basic") {
  let arr: [UInt8] = [10, 20, 30]
  let result = arr.withUnsafeBytes { raw -> CInt in
    unsafe CHeader.raw_sum(raw.baseAddress!, CInt(raw.count))
  }
  expectEqual(result, 60)
}

Suite.test("MutableRawSpan/fill") {
  var arr: [UInt8] = [0, 0, 0, 0]
  arr.withUnsafeMutableBytes { raw in
    unsafe CHeader.raw_fill(raw.baseAddress!, CInt(raw.count), 7)
  }
  expectEqual(arr, [7, 7, 7, 7])
}

runAllTests()

//--- caller.c
// Calls the macro-generated C entry points directly

#include "header.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static int failures = 0;

#define CHECK_EQ(actual, expected)                                            \
  do {                                                                        \
    long long a_ = (actual), e_ = (expected);                                 \
    if (a_ != e_) {                                                           \
      fprintf(stderr, "%s:%d: %s: expected %lld, got %lld\n", __FILE__,       \
              __LINE__, #actual, e_, a_);                                     \
      ++failures;                                                             \
    }                                                                         \
  } while (0)

// Invalid calls that should trap inside the wrapper. Each is run in its
// own process by a RUN line that expects a crash.
static void runTrappingCall(const char *mode) {
  int ints[5] = {1, 2, 3, 4, 5};
  unsigned char bytes[5] = {1, 2, 3, 4, 5};

  if (!strcmp(mode, "span-neg"))
    span_sum(ints, -1);
  else if (!strcmp(mode, "span-nullable-neg"))
    span_sum_nullable(ints, -1);
  else if (!strcmp(mode, "ubp-neg"))
    ubp_sum(ints, -1);
  else if (!strcmp(mode, "ubp-nullable-nonnil-neg"))
    ubp_sum_nullable(ints, -1);
  else if (!strcmp(mode, "ubp-nullable-nil-neg"))
    ubp_sum_nullable(NULL, -1);
  else if (!strcmp(mode, "ubp-nullable-nil-pos"))
    ubp_sum_nullable(NULL, 1);
  else if (!strcmp(mode, "ornull-nonnil-neg"))
    ornull_ubp_sum(ints, -11);
  else if (!strcmp(mode, "mspan-neg"))
    mspan_fill(ints, -1, 0);
  else if (!strcmp(mode, "mubp-neg"))
    mubp_fill(ints, -1, 0);
  else if (!strcmp(mode, "raw-sum-neg"))
    raw_sum(bytes, -1);
  else if (!strcmp(mode, "raw-fill-neg"))
    raw_fill(bytes, -1, 0);
  else {
    fprintf(stderr, "unknown mode '%s'\n", mode);
    exit(EXIT_FAILURE);
  }

  // Reaching here means the call did not trap.
  fprintf(stderr, "call did not trap: %s\n", mode);
  exit(EXIT_SUCCESS);
}

int main(int argc, char **argv) {
  if (argc > 1)
    runTrappingCall(argv[1]);

  int ints[5] = {1, 2, 3, 4, 5};

  // __counted_by + __noescape (Span).
  CHECK_EQ(span_sum(ints, 5), 15);
  CHECK_EQ(span_sum(ints, 2), 3);
  CHECK_EQ(span_sum(ints, 0), 0);

  // Nullable Span: a null pointer becomes an empty span.
  CHECK_EQ(span_sum_nullable(ints, 3), 6);
  CHECK_EQ(span_sum_nullable(NULL, 0), 0);

  // __counted_by (UnsafeBufferPointer).
  CHECK_EQ(ubp_sum(ints, 5), 15);
  CHECK_EQ(ubp_sum(ints, 0), 0);
  CHECK_EQ(ubp_sum_nullable(ints, 4), 10);
  CHECK_EQ(ubp_sum_nullable(NULL, 0), 0);

  // __counted_by_or_null: a null pointer is valid with any count, and the
  // implementation observes `nil`.
  CHECK_EQ(ornull_ubp_sum(ints, 5), 15);
  CHECK_EQ(ornull_ubp_sum(NULL, 0), -1);
  CHECK_EQ(ornull_ubp_sum(NULL, -11), -1);

  // Mutable buffers must write exactly `len` elements and nothing beyond.
  {
    int buf[6] = {0, 0, 0, 0, -1, -1};
    mspan_fill(buf, 4, 42);
    for (int i = 0; i < 4; ++i)
      CHECK_EQ(buf[i], 42);
    CHECK_EQ(buf[4], -1);
    CHECK_EQ(buf[5], -1);
  }
  {
    int buf[5] = {0, 0, 0, -1, -1};
    mubp_fill(buf, 3, 11);
    for (int i = 0; i < 3; ++i)
      CHECK_EQ(buf[i], 11);
    CHECK_EQ(buf[3], -1);
    CHECK_EQ(buf[4], -1);
  }
  {
    int buf[3] = {-1, -1, -1};
    mspan_fill(buf, 0, 9);
    mubp_fill(buf, 0, 9);
    for (int i = 0; i < 3; ++i)
      CHECK_EQ(buf[i], -1);
  }

  // __sized_by: the size is in bytes.
  {
    unsigned char bytes[4] = {10, 20, 30, 40};
    CHECK_EQ(raw_sum(bytes, 3), 60);
    CHECK_EQ(raw_sum(bytes, 4), 100);
    CHECK_EQ(raw_sum(bytes, 0), 0);
  }
  {
    unsigned char bytes[5] = {0, 0, 0, 0xFF, 0xFF};
    raw_fill(bytes, 3, 7);
    for (int i = 0; i < 3; ++i)
      CHECK_EQ(bytes[i], 7);
    CHECK_EQ(bytes[3], 0xFF);
    CHECK_EQ(bytes[4], 0xFF);
  }

  if (failures)
    return EXIT_FAILURE;
  printf("all C entry point checks passed\n");
  return EXIT_SUCCESS;
}
