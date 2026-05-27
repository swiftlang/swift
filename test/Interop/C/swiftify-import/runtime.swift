// REQUIRES: swift_feature_SafeInteropWrappers
// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-run-simple-swift-split-file(test.swift -I %t%{fs-sep}Inputs -target %target-swift-6.2-abi-triple -enable-experimental-feature SafeInteropWrappers)
//
// REQUIRES: executable_test

//--- Inputs/module.modulemap
module Test {
  header "header.h"
}

//--- Inputs/header.h
#define __counted_by(x) __attribute__((__counted_by__(x)))
#define __counted_by_or_null(x) __attribute__((__counted_by_or_null__(x)))
#define __noescape __attribute__((__noescape__))
#define __lifetimebound __attribute__((__lifetimebound__))
#include <stdlib.h>

static inline void interop_counted_noescape(const int* __counted_by(count) _Nonnull in __noescape, int * __counted_by(count) _Nonnull out __noescape, int count) {
  for(int i = 0; i < count; i++)
    out[i] = in[i];
}

static inline void interop_counted(const int* __counted_by(count) _Nonnull in, int * __counted_by(count) _Nonnull out, int count) {
  for(int i = 0; i < count; i++)
    out[i] = in[i];
}

static inline void interop_counted_noescape_nullable(const int* __counted_by(count) _Nullable in __noescape, int * __counted_by(count) _Nullable out __noescape, int count) {
  for(int i = 0; i < count; i++)
    out[i] = in[i];
}

static inline void interop_counted_nullable(const int* __counted_by(count) _Nullable in, int * __counted_by(count) _Nullable out, int count) {
  for(int i = 0; i < count; i++)
    out[i] = in[i];
}

static inline const int* __counted_by(count) _Nonnull interop_counted_return_lifetimebound(const int* __counted_by(count) _Nonnull in __lifetimebound, int count) {
  int *out = malloc(count * sizeof(int));
  for(int i = 0; i < count; i++)
    out[i] = in[i];
  return out;
}

static inline const int* __counted_by(count) _Nonnull interop_counted_return(const int* __counted_by(count) _Nonnull in, int count) {
  int *out = malloc(count * sizeof(int));
  for(int i = 0; i < count; i++)
    out[i] = in[i];
  return out;
}

static inline const int* __counted_by(count) _Nullable interop_counted_return_lifetimebound_nullable(const int* __counted_by(count) _Nullable in __lifetimebound, int count) {
  int *out = malloc(count * sizeof(int));
  for(int i = 0; i < count; i++)
    out[i] = in[i];
  return out;
}

static inline const int* __counted_by(count) _Nullable interop_counted_return_nullable(const int* __counted_by(count) _Nullable in, int count) {
  int *out = malloc(count * sizeof(int));
  for(int i = 0; i < count; i++)
    out[i] = in[i];
  return out;
}

static inline void interop_mixed_param_param(const int* __counted_by_or_null(count) _Nullable in_or_null __noescape,
                                             int * __counted_by(count) _Nonnull out __noescape,
                                             int count) {
  if (in_or_null) {
    for(int i = 0; i < count; i++)
      out[i] = in_or_null[i];
  } else {
    for(int i = 0; i < count; i++)
      out[i] = 0;
  }
}

static inline void interop_mixed_three(const int* __counted_by_or_null(count) _Nullable in_or_null_a __noescape,
                                       int * __counted_by(count) _Nonnull out __noescape,
                                       const int* __counted_by_or_null(count) _Nullable in_or_null_b __noescape,
                                       int count) {
  for(int i = 0; i < count; i++)
    out[i] = (in_or_null_a ? in_or_null_a[i] : 0) + (in_or_null_b ? in_or_null_b[i] : 0);
}

static inline const int* __counted_by(count) _Nonnull
interop_mixed_param_return(const int* __counted_by_or_null(count) _Nullable in_or_null,
                           int count) {
  // this intentionally always allocates *something*. The wrapping function should
  // handle empty buffers with non-null pointers.
  int *out = malloc(count * sizeof(int) > 0 ? count * sizeof(int) : 1);
  if (in_or_null) {
    for(int i = 0; i < count; i++) out[i] = in_or_null[i];
  } else {
    for(int i = 0; i < count; i++) out[i] = 0;
  }
  return out;
}

//--- test.swift
import StdlibUnittest
import Test

extension InlineArray: Equatable where Element: Equatable {
  public static func == (lhs: Self, rhs: Self) -> Bool {
    for i in lhs.indices {
        guard lhs[i] == rhs[i] else { return false }
    }
    return true
  }
}

extension UnsafeBufferPointer: Equatable where Element: Equatable {
  public static func == (lhs: Self, rhs: Self) -> Bool {
    for i in lhs.indices {
        guard unsafe lhs[i] == rhs[i] else { return false }
    }
    return true
  }
}

extension Span: Equatable where Element: Equatable {
  public static func == (lhs: Self, rhs: Self) -> Bool {
    for i in lhs.indices {
        guard lhs[i] == rhs[i] else { return false }
    }
    return true
  }
}

func expectEqual<T: Equatable & ~Escapable>(_ a: T?, _ b: T?) {
  guard let a = a else {
    return expectTrue(b == nil)
  }
  guard let b = b else {
    return expectTrue(false)
  }
  expectTrue(a == b)
}

var Suite = TestSuite("Safe wrappers")

Suite.test("Empty array with counted_by and noescape") {
    let emptyArr: [Int32] = []
    var emptyArrOut: [Int32] = []
    var spanOut = emptyArrOut.mutableSpan
    interop_counted_noescape(emptyArr.span, &spanOut)
    expectEqual(emptyArr, emptyArrOut)
}

Suite.test("Empty array with counted_by") {
    let emptyArr: [Int32] = []
    var emptyArrOut: [Int32] = []
    emptyArr.withUnsafeBufferPointer { buf in
      emptyArrOut.withUnsafeMutableBufferPointer { bufOut in
        unsafe interop_counted(buf, bufOut)
      }
    }
    expectEqual(emptyArr, emptyArrOut)
}

Suite.test("Non-empty array with counted_by and noescape") {
    let arr: [Int32] = [1, 2, 3]
    var arrOut: [Int32] = [3, 2, 1]
    var spanOut = arrOut.mutableSpan
    interop_counted_noescape(arr.span, &spanOut)
    expectEqual(arr, arrOut)
}

Suite.test("Non-empty array with counted_by") {
    let arr: [Int32] = [1, 2, 3]
    var arrOut: [Int32] = [3, 2, 1]
    arr.withUnsafeBufferPointer { buf in
      arrOut.withUnsafeMutableBufferPointer { bufOut in
        unsafe interop_counted(buf, bufOut)
      }
    }
    expectEqual(arr, arrOut)
}

Suite.test("Empty inline array with counted_by and noescape") {
    let emptyArr: [0 of Int32] = []
    var emptyArrOut: [0 of Int32] = []
    var spanOut = emptyArrOut.mutableSpan
    // empty inline array pointer is null
    expectCrash { interop_counted_noescape(emptyArr.span, &spanOut) }
}

Suite.test("Empty inline array with counted_by") {
    let emptyArr: [0 of Int32] = []
    var emptyArrOut: [0 of Int32] = []
    var spanOut = emptyArrOut.mutableSpan
    emptyArr.span.withUnsafeBufferPointer { buf in
      spanOut.withUnsafeMutableBufferPointer { bufOut in
        // empty inline array pointer is null
        expectCrash { unsafe interop_counted(buf, bufOut) }
      }
    }
}

Suite.test("Default span with counted_by and noescape") {
    let emptySpan = Span<Int32>()
    var emptySpanOut = MutableSpan<Int32>()
    // default span pointer is null
    expectCrash { interop_counted_noescape(emptySpan, &emptySpanOut) }
}

Suite.test("Default buffer with counted_by") {
    let emptyBuf = UnsafeBufferPointer<Int32>(_empty:())
    let emptyBufOut = UnsafeMutableBufferPointer<Int32>(_empty:())
    // default buffer pointer is null
    expectCrash { unsafe interop_counted(emptyBuf, emptyBufOut) }
}

Suite.test("Non-empty inline array with counted_by and noescape") {
    let arr: [3 of Int32] = [1, 2, 3]
    var arrOut: [3 of Int32] = [3, 2, 1]
    var spanOut = arrOut.mutableSpan
    interop_counted_noescape(arr.span, &spanOut)
    expectEqual(arr, arrOut)
}

Suite.test("Non-empty inline array with counted_by") {
    let arr: [3 of Int32] = [1, 2, 3]
    var arrOut: [3 of Int32] = [3, 2, 1]
    var spanOut = arrOut.mutableSpan
    arr.span.withUnsafeBufferPointer { buf in
      spanOut.withUnsafeMutableBufferPointer { bufOut in
        unsafe interop_counted(buf, bufOut)
      }
    }
    expectEqual(arr, arrOut)
}

Suite.test("Empty array with counted_by_nullable and noescape") {
    let emptyArr: [Int32] = []
    var emptyArrOut: [Int32] = []
    var spanOut = emptyArrOut.mutableSpan
    interop_counted_noescape_nullable(emptyArr.span, &spanOut)
    expectEqual(emptyArr, emptyArrOut)
}

Suite.test("Empty array with counted_by_nullable") {
    let emptyArr: [Int32] = []
    var emptyArrOut: [Int32] = []
    emptyArr.withUnsafeBufferPointer { buf in
      emptyArrOut.withUnsafeMutableBufferPointer { bufOut in
        unsafe interop_counted_nullable(buf, bufOut)
      }
    }
    expectEqual(emptyArr, emptyArrOut)
}

Suite.test("Non-empty array with counted_by_nullable and noescape") {
    let arr: [Int32] = [1, 2, 3]
    var arrOut: [Int32] = [3, 2, 1]
    var spanOut = arrOut.mutableSpan
    interop_counted_noescape_nullable(arr.span, &spanOut)
    expectEqual(arr, arrOut)
}

Suite.test("Non-empty array with counted_by_nullable") {
    let arr: [Int32] = [1, 2, 3]
    var arrOut: [Int32] = [3, 2, 1]
    arr.withUnsafeBufferPointer { buf in
      arrOut.withUnsafeMutableBufferPointer { bufOut in
        unsafe interop_counted_nullable(buf, bufOut)
      }
    }
    expectEqual(arr, arrOut)
}

Suite.test("Empty inline array with counted_by_nullable and noescape") {
    let emptyArr: [0 of Int32] = []
    var emptyArrOut: [0 of Int32] = []
    var spanOut = emptyArrOut.mutableSpan
    interop_counted_noescape_nullable(emptyArr.span, &spanOut)
    expectEqual(emptyArr, emptyArrOut)
}

Suite.test("Empty inline array with counted_by_nullable") {
    let emptyArr: [0 of Int32] = []
    var emptyArrOut: [0 of Int32] = []
    var spanOut = emptyArrOut.mutableSpan
    emptyArr.span.withUnsafeBufferPointer { buf in
      spanOut.withUnsafeMutableBufferPointer { bufOut in
        unsafe interop_counted_nullable(buf, bufOut)
      }
    }
    expectEqual(emptyArr, emptyArrOut)
}

Suite.test("Non-empty inline array with counted_by_nullable and noescape") {
    let arr: [3 of Int32] = [1, 2, 3]
    var arrOut: [3 of Int32] = [3, 2, 1]
    var spanOut = arrOut.mutableSpan
    interop_counted_noescape_nullable(arr.span, &spanOut)
    expectEqual(arr, arrOut)
}

Suite.test("Non-empty inline array with counted_by_nullable") {
    let arr: [3 of Int32] = [1, 2, 3]
    var arrOut: [3 of Int32] = [3, 2, 1]
    var spanOut = arrOut.mutableSpan
    arr.span.withUnsafeBufferPointer { buf in
      spanOut.withUnsafeMutableBufferPointer { bufOut in
        unsafe interop_counted_nullable(buf, bufOut)
      }
    }
    expectEqual(arr, arrOut)
}

Suite.test("Empty array with return_lifetimebound") {
    let emptyArr: [Int32] = []
    let result = interop_counted_return_lifetimebound(emptyArr.span)
    expectEqual(emptyArr.span, result)
}

Suite.test("Non-empty array with return_lifetimebound") {
    let arr: [Int32] = [1, 2, 3]
    let result = interop_counted_return_lifetimebound(arr.span)
    expectEqual(arr.span, result)
}

Suite.test("Empty array with return") {
    let emptyArr: [Int32] = []
    emptyArr.withUnsafeBufferPointer { buf in
        let result = unsafe interop_counted_return(buf)
        unsafe expectEqual(buf, result)
    }
}

Suite.test("Non-empty array with return") {
    let arr: [Int32] = [1, 2, 3]
    arr.withUnsafeBufferPointer { buf in
        let result = unsafe interop_counted_return(buf)
        unsafe expectEqual(buf, result)
    }
}

Suite.test("Empty inline array with return_lifetimebound") {
    let emptyArr: [0 of Int32] = []
    // empty inline array pointer is null
    expectCrash { let _ = interop_counted_return_lifetimebound(emptyArr.span) }
}

Suite.test("Non-empty inline array with return_lifetimebound") {
    let arr: [3 of Int32] = [1, 2, 3]
    let result = interop_counted_return_lifetimebound(arr.span)
    expectEqual(arr.span, result)
}

Suite.test("Default span with return_lifetimebound") {
    // default span pointer is null
    expectCrash { let _ = interop_counted_return_lifetimebound(Span()) }
}

Suite.test("Empty inline array with return") {
    let emptyArr: [0 of Int32] = []
    emptyArr.span.withUnsafeBufferPointer { buf in
      // empty inline array pointer is null
      expectCrash { let _ = unsafe interop_counted_return(buf) }
    }
}

Suite.test("Non-empty inline array with return") {
    let arr: [3 of Int32] = [1, 2, 3]
    arr.span.withUnsafeBufferPointer { buf in
      let result = unsafe interop_counted_return(buf)
      unsafe expectEqual(buf, result)
    }
}

Suite.test("Default buffer pointer with return") {
    // default buffer pointer is null
    expectCrash { let _ = unsafe interop_counted_return(UnsafeBufferPointer(_empty:())) }
}

Suite.test("Empty array with return_lifetimebound_nullable") {
    let emptyArr: [Int32] = []
    let result = interop_counted_return_lifetimebound_nullable(emptyArr.span)
    expectEqual(emptyArr.span, result)
}

Suite.test("Non-empty array with return_lifetimebound_nullable") {
    let arr: [Int32] = [1, 2, 3]
    let result = interop_counted_return_lifetimebound_nullable(arr.span)
    expectEqual(arr.span, result)
}

Suite.test("Empty array with return_nullable") {
    let emptyArr: [Int32] = []
    emptyArr.withUnsafeBufferPointer { buf in
        let result = unsafe interop_counted_return_nullable(buf)
        unsafe expectEqual(buf, result)
    }
}

Suite.test("Non-empty array with return_nullable") {
    let arr: [Int32] = [1, 2, 3]
    arr.withUnsafeBufferPointer { buf in
        let result = unsafe interop_counted_return_nullable(buf)
        unsafe expectEqual(buf, result)
    }
}

Suite.test("Empty inline array with return_lifetimebound_nullable") {
    let emptyArr: [0 of Int32] = []
    let result = interop_counted_return_lifetimebound_nullable(emptyArr.span)
    expectEqual(emptyArr.span, result)
}

Suite.test("Non-empty inline array with return_lifetimebound_nullable") {
    let arr: [3 of Int32] = [1, 2, 3]
    let result = interop_counted_return_lifetimebound_nullable(arr.span)
    expectEqual(arr.span, result)
}

Suite.test("Default span with return_lifetimebound_nullable") {
    let span = Span<Int32>()
    let result = interop_counted_return_lifetimebound_nullable(span)
    expectEqual(span, result)
}

Suite.test("Empty inline array with return_nullable") {
    let emptyArr: [0 of Int32] = []
    emptyArr.span.withUnsafeBufferPointer { buf in
      let result = unsafe interop_counted_return_nullable(buf)
      unsafe expectEqual(buf, result)
    }
}

Suite.test("Non-empty inline array with return_nullable") {
    let arr: [3 of Int32] = [1, 2, 3]
    arr.span.withUnsafeBufferPointer { buf in
      let result = unsafe interop_counted_return_nullable(buf)
      unsafe expectEqual(buf, result)
    }
}

Suite.test("Default buffer with return_nullable") {
    let buf = UnsafeBufferPointer<Int32>(_empty:())
    let result = unsafe interop_counted_return_nullable(buf)
    unsafe expectEqual(buf, result)
}

Suite.test("Mismatching lengths span") {
    let emptyArr: [Int32] = [1]
    var emptyArrOut: [Int32] = []
    var spanOut = emptyArrOut.mutableSpan
    expectCrash { interop_counted_noescape(emptyArr.span, &spanOut) }
}

Suite.test("Mismatching lengths span nullable") {
    let emptyArr: [Int32] = [1]
    var emptyArrOut: [Int32] = []
    var spanOut = emptyArrOut.mutableSpan
    expectCrash { interop_counted_noescape_nullable(emptyArr.span, &spanOut) }
}

// --- Mixed nullability shared-count tests ---

Suite.test("Mixed shared count: param+param, OrNull nil") {
    var arrOut: [Int32] = [9, 9, 9]
    var spanOut = arrOut.mutableSpan
    interop_mixed_param_param(nil, &spanOut)
    expectEqual([0, 0, 0] as [Int32], arrOut)
}

Suite.test("Mixed shared count: param+param, OrNull non-nil matching") {
    let arrIn: [Int32] = [1, 2, 3]
    var arrOut: [Int32] = [9, 9, 9]
    var spanOut = arrOut.mutableSpan
    interop_mixed_param_param(Optional(arrIn.span), &spanOut)
    expectEqual(arrIn, arrOut)
}

Suite.test("Mixed shared count: param+param, mismatched non-nil") {
    let arrIn: [Int32] = [1, 2, 3, 4]
    var arrOut: [Int32] = [9, 9, 9]
    var spanOut = arrOut.mutableSpan
    expectCrash { interop_mixed_param_param(Optional(arrIn.span), &spanOut) }
}

Suite.test("Mixed shared count: param+param, empty + nil") {
    var arrOut: [Int32] = []
    var spanOut = arrOut.mutableSpan
    interop_mixed_param_param(nil, &spanOut)
    expectEqual([] as [Int32], arrOut)
}

Suite.test("Mixed shared count: three params, both OrNull nil") {
    var arrOut: [Int32] = [7, 7]
    var spanOut = arrOut.mutableSpan
    interop_mixed_three(nil, &spanOut, nil)
    expectEqual([0, 0] as [Int32], arrOut)
}

Suite.test("Mixed shared count: three params, one OrNull non-nil one nil") {
    let arrA: [Int32] = [10, 20]
    var arrOut: [Int32] = [0, 0]
    var spanOut = arrOut.mutableSpan
    interop_mixed_three(Optional(arrA.span), &spanOut, nil)
    expectEqual([10, 20] as [Int32], arrOut)
}

Suite.test("Mixed shared count: three params, OrNull mismatched") {
    let arrA: [Int32] = [10, 20, 30]
    var arrOut: [Int32] = [0, 0]
    var spanOut = arrOut.mutableSpan
    expectCrash { interop_mixed_three(Optional(arrA.span), &spanOut, nil) }
}

Suite.test("Mixed shared count: param+return, OrNull nil") {
    let buf = unsafe interop_mixed_param_return(nil)
    expectEqual(0, buf.count)
}

Suite.test("Mixed shared count: param+return, OrNull non-nil") {
    let arrIn: [Int32] = [4, 5, 6]
    arrIn.withUnsafeBufferPointer { bufIn in
      let buf = unsafe interop_mixed_param_return(bufIn)
      expectEqual(3, buf.count)
      for i in 0..<3 { unsafe expectTrue(arrIn[i] == buf[i]) }
    }
}

runAllTests()
