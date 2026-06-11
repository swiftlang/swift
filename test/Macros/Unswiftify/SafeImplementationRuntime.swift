// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_SafeInteropImplementations
// REQUIRES: swift_feature_SafeInteropWrappers
// REQUIRES: executable_test

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Step 1: Compile the Swift implementation into an object file. This file
//         contains the @c @implementation(safe) functions whose @_Unswiftify
//         peers emit the C symbols declared in the header.
// RUN: %target-build-swift %t/impl.swift -I %t%{fs-sep}Inputs -plugin-path %swift-plugin-dir -target %target-swift-6.2-abi-triple -enable-experimental-feature SafeInteropImplementations -enable-experimental-feature SafeInteropWrappers -parse-as-library -c -o %t/impl.o

// Step 2: Compile and link the caller, which imports only the C header (not
//         the Swift module). It calls the C functions through the imported
//         unsafe interface. At link time the calls resolve to the
//         @c @implementation peers emitted by step 1.
// RUN: %target-build-swift %t/caller.swift -I %t%{fs-sep}Inputs -target %target-swift-6.2-abi-triple -enable-experimental-feature SafeInteropWrappers %t/impl.o -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out

//--- Inputs/module.modulemap
module CHeader {
  header "header.h"
}

//--- Inputs/header.h
#define __counted_by(x) __attribute__((__counted_by__(x)))
#define __counted_by_or_null(x) __attribute__((__counted_by_or_null__(x)))
#define __noescape __attribute__((__noescape__))

int span_sum(const int * _Nonnull __counted_by(len) __noescape x, int len);
int span_sum_nullable(const int * _Nullable __counted_by(len) __noescape x, int len);
int ubp_sum(const int * _Nonnull __counted_by(len) x, int len);
int ubp_sum_nullable(const int * __counted_by(len) x, int len);
void mspan_fill(int * _Nonnull __counted_by(len) __noescape x, int len, int val);
void mubp_fill(int * _Nonnull __counted_by(len) x, int len, int val);
int ornull_ubp_sum(const int * _Nullable __counted_by_or_null(len) x, int len);

//--- impl.swift
// Safe implementations. The @_Unswiftify macro-generated peers provide the
// C symbols for the functions declared in header.h.

import CHeader

@c @implementation(safe)
public func span_sum(_ x: Span<CInt>) -> CInt {
  var total: CInt = 0
  for i in x.indices { total += x[i] }
  return total
}

@c @implementation(safe)
public func span_sum_nullable(_ x: Span<CInt>?) -> CInt {
  guard let x = x else {
    return 0
  }
  var total: CInt = 0
  for i in x.indices { total += x[i] }
  return total
}

@c @implementation(safe)
public func ubp_sum(_ x: UnsafeBufferPointer<CInt>) -> CInt {
  var total: CInt = 0
  for i in x.indices { total += unsafe x[i] }
  return total
}

@c @implementation(safe)
public func ubp_sum_nullable(_ x: UnsafeBufferPointer<CInt>) -> CInt {
  var total: CInt = 0
  for i in x.indices { total += unsafe x[i] }
  return total
}

@c @implementation(safe)
public func mspan_fill(_ x: inout MutableSpan<CInt>, _ val: CInt) {
  for i in x.indices { x[i] = val }
}

@c @implementation(safe)
public func mubp_fill(_ x: UnsafeMutableBufferPointer<CInt>, _ val: CInt) {
  for i in x.indices { unsafe x[i] = val }
}

@c @implementation(safe)
public func ornull_ubp_sum(_ x: UnsafeBufferPointer<CInt>?) -> CInt {
  guard let x else { return -1 }
  var total: CInt = 0
  for i in x.indices { total += unsafe x[i] }
  return total
}

//--- caller.swift
// This file imports only the C header — it has no visibility into the Swift
// implementation module. Every call here goes through the C-imported unsafe
// interface (pointer + count), which the linker resolves to the
// @c @implementation peers emitted from impl.swift. This exercises the full
// bridge: C caller → macro-generated peer → safe Swift function.

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
  let result: CInt = unsafe CHeader.ubp_sum_nullable(nil, 0)
  expectEqual(result, 0)
}

Suite.test("UBP/nullable/neg") {
  expectCrash {
    let _: CInt = unsafe CHeader.ubp_sum_nullable(nil, -1)
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
  let result: CInt = unsafe CHeader.ornull_ubp_sum(nil, 0)
  expectEqual(result, -1)
}

Suite.test("countedByOrNull/UBP/nil/neg") {
  let result: CInt = unsafe CHeader.ornull_ubp_sum(nil, -11)
  expectEqual(result, -1)
}

Suite.test("countedByOrNull/UBP/nonnil/neg") {
  let arr: [CInt] = [3, 3, 3]
  let result = arr.withUnsafeBufferPointer { buf -> CInt in
    expectCrash {
      unsafe CHeader.ornull_ubp_sum(buf.baseAddress, -11)
    }
  }
}

runAllTests()
