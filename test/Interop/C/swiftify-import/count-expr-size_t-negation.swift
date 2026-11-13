// The expected expansions spell out the canonical type of `size_t`, which is
// `unsigned long long` on Windows and `unsigned int` on 32-bit targets.
// UNSUPPORTED: OS=windows-msvc
// UNSUPPORTED: PTRSIZE=32

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -I %t -strict-memory-safety \
// RUN:   %t/test.swift -verify -verify-additional-file %t%{fs-sep}test.h -Rmacro-expansions -suppress-notes -eager-macro-checking

// Check a count expression that negates a `size_t` parameter.
//
// FIXME: A reference to a `size_t` parameter is converted to `CUnsignedLong`,
// which has no unary minus, so the wrapper doesn't compile. Before references
// were converted, the count was emitted as `(a - -b)` on `Int`s, which compiled
// (https://github.com/swiftlang/swift/issues/94243).

//--- test.h
#pragma once

#include <ptrcheck.h>
#include <stddef.h>

#define __noescape __attribute__((noescape))

// expected-expansion@+25:84{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func negatedParam(_ p: Span<CInt>, _ a: Int, _ b: Int) {|}}
//   expected-error@3{{unary operator '-' cannot be applied to an operand of type 'CUnsignedLong' (aka 'UInt')}}
//   expected-error@3{{binary operator '-' cannot be applied to operands of type 'CUnsignedLong' (aka 'UInt')}}
//   expected-remark@3{{macro content: |    if p.count != (CUnsignedLong(a) - -CUnsignedLong(b)) {|}}
//   expected-remark@4{{macro content: |      @inline(never) func _boundsCheckFailure<E: BinaryInteger, A: BinaryInteger>(_ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@5{{macro content: |        @inline(never) func _fail(_ function: StaticString, _ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@6{{macro content: |          fatalError("bounds check failure in \\(function): expected \\(expected) but got \\(actual)")|}}
//   expected-remark@7{{macro content: |        }|}}
//   expected-remark@8{{macro content: |        _fail("negatedParam", expected, actual)|}}
//   expected-remark@9{{macro content: |      }|}}
//   expected-error@10{{unary operator '-' cannot be applied to an operand of type 'CUnsignedLong' (aka 'UInt')}}
//   expected-error@10{{binary operator '-' cannot be applied to operands of type 'CUnsignedLong' (aka 'UInt')}}
//   expected-remark@10{{macro content: |      _boundsCheckFailure((CUnsignedLong(a) - -CUnsignedLong(b)), p.count)|}}
//   expected-remark@11{{macro content: |    }|}}
//   expected-remark@12{{macro content: |    let _pPtr = p.withUnsafeBufferPointer {|}}
//   expected-remark@13{{macro content: |        unsafe $0|}}
//   expected-remark@14{{macro content: |    }|}}
//   expected-remark@15{{macro content: |    defer {|}}
//   expected-remark@16{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@17{{macro content: |    }|}}
//   expected-remark@18{{macro content: |    return unsafe negatedParam(_pPtr.baseAddress, a, b)|}}
//   expected-remark@19{{macro content: |}|}}
// }}
void negatedParam(const int * __counted_by(a - -b) __noescape p, size_t a, size_t b);

//--- module.modulemap
module Test {
  header "test.h"
}

//--- test.swift
import Test

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
func callNegatedParam(_ p: Span<CInt>, _ a: Int, _ b: Int) {
  negatedParam(p, a, b)
}
