// REQUIRES: swift_feature_StabilizedSafeInteropWrappers

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Prevent target SDK decl of memcmp (which may or may not have __sized_by annotations)
// from being picked up as the canonical decl.
// RUN: %empty-directory(%t/sdk)

// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -strict-memory-safety -sdk %t/sdk \
// RUN:   -Xcc -Werror %t%{fs-sep}test.swift -import-objc-header %t%{fs-sep}test.h -verify -verify-additional-file %t%{fs-sep}test.h -Rmacro-expansions

// Check that ClangImporter does not try to apply _SwiftifyImport to functions in SwiftShims,
// as it does not import the standard library types.

//--- test.swift
public func callMemCmp1(_ p1: UnsafeMutableRawBufferPointer, _ p2: UnsafeMutableRawBufferPointer) {
  // expected-error@+2{{missing argument for parameter #3 in call}}
  // expected-error@+1 2{{cannot convert value of type 'UnsafeMutableRawBufferPointer' to expected argument type 'UnsafeRawPointer'}}
  let _ = unsafe memcmp(p1, p2)
}

public func callMemCmp2(_ p1: UnsafeMutableRawPointer, _ p2: UnsafeMutableRawPointer) {
  let _ = unsafe memcmp(p1, p2, 13)
}

//--- test.h
#include <stddef.h>
#define __sized_by(x) __attribute__((__sized_by__(x)))

// expected-note@+1{{'memcmp' declared here}}
int memcmp(const void * _Nullable __sized_by(n) s1, const void * _Nullable __sized_by(n) s2, size_t n);
