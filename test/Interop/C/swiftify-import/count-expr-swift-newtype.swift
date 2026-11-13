// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -I %t -strict-memory-safety \
// RUN:   %t/test.swift -verify -verify-additional-file %t%{fs-sep}test.h -Rmacro-expansions -suppress-notes -eager-macro-checking

// Check that no safe wrapper is generated for a function with a count
// expression that refers to a value whose type is a `swift_newtype` typedef.
// Swift imports such a typedef as a struct wrapping the underlying value, which
// count expressions don't support yet.
//
// FIXME: Support `swift_newtype` when generating wrappers
// (https://github.com/swiftlang/swift/issues/94222).

//--- test.h
#pragma once

#include <ptrcheck.h>
#include <stddef.h>

#define __noescape __attribute__((noescape))

// Imported as a struct with `rawValue: CInt`.
typedef int MyCount __attribute__((swift_newtype(struct)));
// Imported as a struct with `rawValue: Int`.
typedef size_t MySize __attribute__((swift_newtype(struct)));

void paramBareCount(const int * __counted_by(n) __noescape p, MyCount n);
void paramBareSize(const int * __counted_by(n) __noescape p, MySize n);

void paramCompoundCount(const int * __counted_by(n + 1) __noescape p, MyCount n);
void paramCompoundSize(const int * __counted_by(n * 2) __noescape p, MySize n);

int * __counted_by(n) returnBareCount(MyCount n);
int * __counted_by(n) returnBareSize(MySize n);

int * __counted_by(n + 1) returnCompoundCount(MyCount n);
int * __counted_by(n * 2) returnCompoundSize(MySize n);

// Imported as methods of `MySize`, where `n` is `self`, and also as obsoleted
// global functions that point to them, where `n` is an ordinary parameter.
void MySizeSelfBare(MySize n, const int * __counted_by(n) __noescape p)
    __attribute__((swift_name("MySize.selfBare(self:_:)")));
void MySizeSelfCompound(MySize n, const int * __counted_by(n * 2) __noescape p)
    __attribute__((swift_name("MySize.selfCompound(self:_:)")));

// Imported as `MySize.default`.
extern const MySize kMySizeDefault;
void globalCompoundSize(const int * __counted_by(kMySizeDefault + 1) __noescape p);

// Swift doesn't import this global at all, but its type is still a
// `swift_newtype`.
extern const MySize kMySize;
void unimportedGlobalCompoundSize(const int * __counted_by(kMySize + 1) __noescape p);

// Only `b`'s count uses a `swift_newtype` value, but the whole wrapper is still
// left out, rather than generating one that only handles `a`.
void twoPointers(const int * __counted_by(len) __noescape a, size_t len,
                 const int * __counted_by(n) __noescape b, MySize n);

//--- module.modulemap
module Test {
  header "test.h"
}

//--- test.swift
import Test

func callParamBare(_ p: UnsafePointer<CInt>, _ count: MyCount, _ size: MySize) {
  unsafe paramBareCount(p, count)
  unsafe paramBareSize(p, size)
}

func callParamCompound(_ p: UnsafePointer<CInt>, _ count: MyCount, _ size: MySize) {
  unsafe paramCompoundCount(p, count)
  unsafe paramCompoundSize(p, size)
}

func callReturnBare(_ count: MyCount, _ size: MySize) {
  let _ = unsafe returnBareCount(count)
  let _ = unsafe returnBareSize(size)
}

func callReturnCompound(_ count: MyCount, _ size: MySize) {
  let _ = unsafe returnCompoundCount(count)
  let _ = unsafe returnCompoundSize(size)
}

func callSelf(_ p: UnsafePointer<CInt>, _ size: MySize) {
  unsafe size.selfBare(p)
  unsafe size.selfCompound(p)
}

func callGlobalCompoundSize(_ p: UnsafePointer<CInt>) {
  unsafe globalCompoundSize(p)
}

func callUnimportedGlobalCompoundSize(_ p: UnsafePointer<CInt>) {
  unsafe unimportedGlobalCompoundSize(p)
}

func callTwoPointers(_ a: UnsafePointer<CInt>, _ len: Int, _ b: UnsafePointer<CInt>, _ n: MySize) {
  unsafe twoPointers(a, len, b, n)
}
