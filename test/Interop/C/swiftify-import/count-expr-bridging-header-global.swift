// `NSUInteger` comes from the Objective-C runtime headers in Apple SDKs.
// REQUIRES: VENDOR=apple
// The expected expansions spell out the canonical type of `NSUInteger`, which
// is `unsigned int` on 32-bit targets.
// UNSUPPORTED: PTRSIZE=32

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -strict-memory-safety \
// RUN:   %t/test.swift -import-bridging-header %t/bridging.h -verify -verify-additional-file %t%{fs-sep}bridging.h \
// RUN:   -Rmacro-expansions -suppress-notes -eager-macro-checking

// Check count expressions that use globals declared in a bridging header. The
// importer puts bridging header declarations in a module of their own, which
// isn't a system module. So a `size_t` global is converted as usual, and an
// `NSUInteger` global is imported as `UInt` and doesn't need converting.

//--- bridging.h
#pragma once

#include <objc/NSObjCRuntime.h>
#include <ptrcheck.h>
#include <stddef.h>

#define __noescape __attribute__((noescape))

extern const size_t kSizeCount;
// expected-expansion@+21:88{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func sizeGlobalCompound(_ p: Span<CInt>, _ n: Int) {|}}
//   expected-remark@3{{macro content: |    if p.count != (CUnsignedLong(kSizeCount) + CUnsignedLong(n)) {|}}
//   expected-remark@4{{macro content: |      @inline(never) func _boundsCheckFailure<E: BinaryInteger, A: BinaryInteger>(_ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@5{{macro content: |        @inline(never) func _fail(_ function: StaticString, _ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@6{{macro content: |          fatalError("bounds check failure in \\(function): expected \\(expected) but got \\(actual)")|}}
//   expected-remark@7{{macro content: |        }|}}
//   expected-remark@8{{macro content: |        _fail("sizeGlobalCompound", expected, actual)|}}
//   expected-remark@9{{macro content: |      }|}}
//   expected-remark@10{{macro content: |      _boundsCheckFailure((CUnsignedLong(kSizeCount) + CUnsignedLong(n)), p.count)|}}
//   expected-remark@11{{macro content: |    }|}}
//   expected-remark@12{{macro content: |    let _pPtr = p.withUnsafeBufferPointer {|}}
//   expected-remark@13{{macro content: |        unsafe $0|}}
//   expected-remark@14{{macro content: |    }|}}
//   expected-remark@15{{macro content: |    defer {|}}
//   expected-remark@16{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@17{{macro content: |    }|}}
//   expected-remark@18{{macro content: |    return unsafe sizeGlobalCompound(_pPtr.baseAddress, n)|}}
//   expected-remark@19{{macro content: |}|}}
// }}
void sizeGlobalCompound(const int * __counted_by(kSizeCount + n) __noescape p, size_t n);

extern const NSUInteger kNSUIntegerCount;
// expected-expansion@+21:104{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func nsuintegerGlobalCompound(_ p: Span<CInt>, _ n: UInt) {|}}
//   expected-remark@3{{macro content: |    if p.count != (kNSUIntegerCount + n) {|}}
//   expected-remark@4{{macro content: |      @inline(never) func _boundsCheckFailure<E: BinaryInteger, A: BinaryInteger>(_ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@5{{macro content: |        @inline(never) func _fail(_ function: StaticString, _ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@6{{macro content: |          fatalError("bounds check failure in \\(function): expected \\(expected) but got \\(actual)")|}}
//   expected-remark@7{{macro content: |        }|}}
//   expected-remark@8{{macro content: |        _fail("nsuintegerGlobalCompound", expected, actual)|}}
//   expected-remark@9{{macro content: |      }|}}
//   expected-remark@10{{macro content: |      _boundsCheckFailure((kNSUIntegerCount + n), p.count)|}}
//   expected-remark@11{{macro content: |    }|}}
//   expected-remark@12{{macro content: |    let _pPtr = p.withUnsafeBufferPointer {|}}
//   expected-remark@13{{macro content: |        unsafe $0|}}
//   expected-remark@14{{macro content: |    }|}}
//   expected-remark@15{{macro content: |    defer {|}}
//   expected-remark@16{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@17{{macro content: |    }|}}
//   expected-remark@18{{macro content: |    return unsafe nsuintegerGlobalCompound(_pPtr.baseAddress, n)|}}
//   expected-remark@19{{macro content: |}|}}
// }}
void nsuintegerGlobalCompound(const int * __counted_by(kNSUIntegerCount + n) __noescape p, NSUInteger n);

//--- test.swift
@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
func callSizeGlobalCompound(_ p: Span<CInt>, _ n: Int) {
  sizeGlobalCompound(p, n)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
func callNSUIntegerGlobalCompound(_ p: Span<CInt>, _ n: UInt) {
  nsuintegerGlobalCompound(p, n)
}
