// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -I %t -strict-memory-safety \
// RUN:   %t/test.swift -verify -verify-additional-file %t%{fs-sep}test.h -Rmacro-expansions -suppress-notes -eager-macro-checking

// Check count expressions that are just the name of a `const` global. Count
// expressions in function declarations can only refer to parameters and
// `const` globals, and a `const` global with no visible initializer isn't
// folded to a literal, so the count stays a reference to the global.
//
// FIXME: _SwiftifyImport assumes that a count that is just a name refers to a
// parameter, which it removes from the wrapper. There is no such parameter, so
// expanding the macro fails with an error. That error is reported for any use
// of the function, even a call to the original C function, so the function
// can't be used from Swift. Once this is fixed, each function should get a
// wrapper taking a `Span` that checks its count against the global.
// (https://github.com/swiftlang/swift/issues/94219)

//--- test.h
#pragma once

#include <ptrcheck.h>
#include <stddef.h>

#define __noescape __attribute__((noescape))

extern const size_t kSizeCount;
extern const int kIntCount;

// expected-expansion@+6:6{{
//   expected-error@1{{no parameter with name 'kSizeCount' in '_ p: UnsafePointer<CInt>!' (from macro '_SwiftifyImport')}}
// }}
// expected-expansion@+3:66{{
//   expected-remark@1{{macro content: ||}}
// }}
void sizeGlobal(const int * __counted_by(kSizeCount) __noescape p);

// expected-expansion@+6:6{{
//   expected-error@1{{no parameter with name 'kIntCount' in '_ p: UnsafePointer<CInt>!' (from macro '_SwiftifyImport')}}
// }}
// expected-expansion@+3:64{{
//   expected-remark@1{{macro content: ||}}
// }}
void intGlobal(const int * __counted_by(kIntCount) __noescape p);

//--- module.modulemap
module Test {
  header "test.h"
}

//--- test.swift
import Test

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
func callSizeGlobal(_ p: Span<CInt>) {
  // expected-error@+1{{cannot convert value of type 'Span<CInt>' (aka 'Span<Int32>') to expected argument type 'UnsafePointer<CInt>' (aka 'UnsafePointer<Int32>')}}
  sizeGlobal(p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
func callIntGlobal(_ p: Span<CInt>) {
  // expected-error@+1{{cannot convert value of type 'Span<CInt>' (aka 'Span<Int32>') to expected argument type 'UnsafePointer<CInt>' (aka 'UnsafePointer<Int32>')}}
  intGlobal(p)
}
