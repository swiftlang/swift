// `NSUInteger` comes from the Objective-C runtime headers in Apple SDKs.
// REQUIRES: VENDOR=apple
// The expected expansions spell out the canonical type of `NSUInteger`, which
// is `unsigned int` on 32-bit targets.
// UNSUPPORTED: PTRSIZE=32

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -I %t -strict-memory-safety \
// RUN:   %t/test.swift -verify -verify-additional-file %t%{fs-sep}normal.h -verify-additional-file %t%{fs-sep}system.h \
// RUN:   -Rmacro-expansions -suppress-notes -eager-macro-checking

// Check count expressions that use an `NSUInteger` parameter. `NSUInteger` is a
// typedef for `unsigned long`, but Swift imports it as `Int` in a system
// module, which is how SDK headers are imported, and as `UInt` otherwise. (It
// is also imported as `UInt` if the function's name contains "unsigned".)
// So references to it only need converting to `CUnsignedLong` in the system
// module.

//--- normal.h
#pragma once

#include <objc/NSObjCRuntime.h>
#include <ptrcheck.h>
#include "system.h"

#define __noescape __attribute__((noescape))

// expected-expansion@+13:76{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func normalParamBare(_ p: Span<CInt>) {|}}
//   expected-remark@3{{macro content: |    let n = UInt(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    let _pPtr = p.withUnsafeBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe normalParamBare(_pPtr.baseAddress, n)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
void normalParamBare(const int * __counted_by(n) __noescape p, NSUInteger n);

// expected-expansion@+21:84{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func normalParamCompound(_ p: Span<CInt>, _ n: UInt) {|}}
//   expected-remark@3{{macro content: |    if p.count != (n + CUnsignedLong(CInt(1))) {|}}
//   expected-remark@4{{macro content: |      @inline(never) func _boundsCheckFailure<E: BinaryInteger, A: BinaryInteger>(_ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@5{{macro content: |        @inline(never) func _fail(_ function: StaticString, _ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@6{{macro content: |          fatalError("bounds check failure in \\(function): expected \\(expected) but got \\(actual)")|}}
//   expected-remark@7{{macro content: |        }|}}
//   expected-remark@8{{macro content: |        _fail("normalParamCompound", expected, actual)|}}
//   expected-remark@9{{macro content: |      }|}}
//   expected-remark@10{{macro content: |      _boundsCheckFailure((n + CUnsignedLong(CInt(1))), p.count)|}}
//   expected-remark@11{{macro content: |    }|}}
//   expected-remark@12{{macro content: |    let _pPtr = p.withUnsafeBufferPointer {|}}
//   expected-remark@13{{macro content: |        unsafe $0|}}
//   expected-remark@14{{macro content: |    }|}}
//   expected-remark@15{{macro content: |    defer {|}}
//   expected-remark@16{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@17{{macro content: |    }|}}
//   expected-remark@18{{macro content: |    return unsafe normalParamCompound(_pPtr.baseAddress, n)|}}
//   expected-remark@19{{macro content: |}|}}
// }}
void normalParamCompound(const int * __counted_by(n + 1) __noescape p, NSUInteger n);

// expected-expansion@+6:52{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @_disfavoredOverload public func normalReturnBare(_ n: UInt) -> UnsafeMutableBufferPointer<CInt> {|}}
//   expected-remark@3{{macro content: |    return unsafe UnsafeMutableBufferPointer<CInt>(start: unsafe normalReturnBare(n), count: Int(n))|}}
//   expected-remark@4{{macro content: |}|}}
// }}
int * __counted_by(n) normalReturnBare(NSUInteger n);

// expected-expansion@+6:60{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @_disfavoredOverload public func normalReturnCompound(_ n: UInt) -> UnsafeMutableBufferPointer<CInt> {|}}
//   expected-remark@3{{macro content: |    return unsafe UnsafeMutableBufferPointer<CInt>(start: unsafe normalReturnCompound(n), count: Int((n + CUnsignedLong(CInt(1)))))|}}
//   expected-remark@4{{macro content: |}|}}
// }}
int * __counted_by(n + 1) normalReturnCompound(NSUInteger n);

// Imported as `UInt`, like `NSUInteger` parameters in this module.
extern const NSUInteger kNormalCount;
// expected-expansion@+21:96{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func normalGlobalCompound(_ p: Span<CInt>, _ n: UInt) {|}}
//   expected-remark@3{{macro content: |    if p.count != (kNormalCount + n) {|}}
//   expected-remark@4{{macro content: |      @inline(never) func _boundsCheckFailure<E: BinaryInteger, A: BinaryInteger>(_ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@5{{macro content: |        @inline(never) func _fail(_ function: StaticString, _ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@6{{macro content: |          fatalError("bounds check failure in \\(function): expected \\(expected) but got \\(actual)")|}}
//   expected-remark@7{{macro content: |        }|}}
//   expected-remark@8{{macro content: |        _fail("normalGlobalCompound", expected, actual)|}}
//   expected-remark@9{{macro content: |      }|}}
//   expected-remark@10{{macro content: |      _boundsCheckFailure((kNormalCount + n), p.count)|}}
//   expected-remark@11{{macro content: |    }|}}
//   expected-remark@12{{macro content: |    let _pPtr = p.withUnsafeBufferPointer {|}}
//   expected-remark@13{{macro content: |        unsafe $0|}}
//   expected-remark@14{{macro content: |    }|}}
//   expected-remark@15{{macro content: |    defer {|}}
//   expected-remark@16{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@17{{macro content: |    }|}}
//   expected-remark@18{{macro content: |    return unsafe normalGlobalCompound(_pPtr.baseAddress, n)|}}
//   expected-remark@19{{macro content: |}|}}
// }}
void normalGlobalCompound(const int * __counted_by(kNormalCount + n) __noescape p, NSUInteger n);

// `kSystemCount` is declared in the system module, so it's imported as `Int`
// and converted, even though this function and `n` are in the normal module.
// expected-expansion@+21:98{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func normalUsesSystemGlobal(_ p: Span<CInt>, _ n: UInt) {|}}
//   expected-remark@3{{macro content: |    if p.count != (CUnsignedLong(kSystemCount) + n) {|}}
//   expected-remark@4{{macro content: |      @inline(never) func _boundsCheckFailure<E: BinaryInteger, A: BinaryInteger>(_ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@5{{macro content: |        @inline(never) func _fail(_ function: StaticString, _ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@6{{macro content: |          fatalError("bounds check failure in \\(function): expected \\(expected) but got \\(actual)")|}}
//   expected-remark@7{{macro content: |        }|}}
//   expected-remark@8{{macro content: |        _fail("normalUsesSystemGlobal", expected, actual)|}}
//   expected-remark@9{{macro content: |      }|}}
//   expected-remark@10{{macro content: |      _boundsCheckFailure((CUnsignedLong(kSystemCount) + n), p.count)|}}
//   expected-remark@11{{macro content: |    }|}}
//   expected-remark@12{{macro content: |    let _pPtr = p.withUnsafeBufferPointer {|}}
//   expected-remark@13{{macro content: |        unsafe $0|}}
//   expected-remark@14{{macro content: |    }|}}
//   expected-remark@15{{macro content: |    defer {|}}
//   expected-remark@16{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@17{{macro content: |    }|}}
//   expected-remark@18{{macro content: |    return unsafe normalUsesSystemGlobal(_pPtr.baseAddress, n)|}}
//   expected-remark@19{{macro content: |}|}}
// }}
void normalUsesSystemGlobal(const int * __counted_by(kSystemCount + n) __noescape p, NSUInteger n);

//--- system.h
#pragma once

#include <objc/NSObjCRuntime.h>
#include <ptrcheck.h>

#define __noescape __attribute__((noescape))

// expected-expansion@+13:76{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func systemParamBare(_ p: Span<CInt>) {|}}
//   expected-remark@3{{macro content: |    let n = p.count|}}
//   expected-remark@4{{macro content: |    let _pPtr = p.withUnsafeBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe systemParamBare(_pPtr.baseAddress, n)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
void systemParamBare(const int * __counted_by(n) __noescape p, NSUInteger n);

// expected-expansion@+21:84{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func systemParamCompound(_ p: Span<CInt>, _ n: Int) {|}}
//   expected-remark@3{{macro content: |    if p.count != (CUnsignedLong(n) + CUnsignedLong(CInt(1))) {|}}
//   expected-remark@4{{macro content: |      @inline(never) func _boundsCheckFailure<E: BinaryInteger, A: BinaryInteger>(_ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@5{{macro content: |        @inline(never) func _fail(_ function: StaticString, _ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@6{{macro content: |          fatalError("bounds check failure in \\(function): expected \\(expected) but got \\(actual)")|}}
//   expected-remark@7{{macro content: |        }|}}
//   expected-remark@8{{macro content: |        _fail("systemParamCompound", expected, actual)|}}
//   expected-remark@9{{macro content: |      }|}}
//   expected-remark@10{{macro content: |      _boundsCheckFailure((CUnsignedLong(n) + CUnsignedLong(CInt(1))), p.count)|}}
//   expected-remark@11{{macro content: |    }|}}
//   expected-remark@12{{macro content: |    let _pPtr = p.withUnsafeBufferPointer {|}}
//   expected-remark@13{{macro content: |        unsafe $0|}}
//   expected-remark@14{{macro content: |    }|}}
//   expected-remark@15{{macro content: |    defer {|}}
//   expected-remark@16{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@17{{macro content: |    }|}}
//   expected-remark@18{{macro content: |    return unsafe systemParamCompound(_pPtr.baseAddress, n)|}}
//   expected-remark@19{{macro content: |}|}}
// }}
void systemParamCompound(const int * __counted_by(n + 1) __noescape p, NSUInteger n);

// expected-expansion@+6:52{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @_disfavoredOverload public func systemReturnBare(_ n: Int) -> UnsafeMutableBufferPointer<CInt> {|}}
//   expected-remark@3{{macro content: |    return unsafe UnsafeMutableBufferPointer<CInt>(start: unsafe systemReturnBare(n), count: Int(n))|}}
//   expected-remark@4{{macro content: |}|}}
// }}
int * __counted_by(n) systemReturnBare(NSUInteger n);

// expected-expansion@+6:60{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @_disfavoredOverload public func systemReturnCompound(_ n: Int) -> UnsafeMutableBufferPointer<CInt> {|}}
//   expected-remark@3{{macro content: |    return unsafe UnsafeMutableBufferPointer<CInt>(start: unsafe systemReturnCompound(n), count: Int((CUnsignedLong(n) + CUnsignedLong(CInt(1)))))|}}
//   expected-remark@4{{macro content: |}|}}
// }}
int * __counted_by(n + 1) systemReturnCompound(NSUInteger n);

// Imported as `Int`, like `NSUInteger` parameters in this module.
extern const NSUInteger kSystemCount;
// expected-expansion@+21:96{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func systemGlobalCompound(_ p: Span<CInt>, _ n: Int) {|}}
//   expected-remark@3{{macro content: |    if p.count != (CUnsignedLong(kSystemCount) + CUnsignedLong(n)) {|}}
//   expected-remark@4{{macro content: |      @inline(never) func _boundsCheckFailure<E: BinaryInteger, A: BinaryInteger>(_ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@5{{macro content: |        @inline(never) func _fail(_ function: StaticString, _ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@6{{macro content: |          fatalError("bounds check failure in \\(function): expected \\(expected) but got \\(actual)")|}}
//   expected-remark@7{{macro content: |        }|}}
//   expected-remark@8{{macro content: |        _fail("systemGlobalCompound", expected, actual)|}}
//   expected-remark@9{{macro content: |      }|}}
//   expected-remark@10{{macro content: |      _boundsCheckFailure((CUnsignedLong(kSystemCount) + CUnsignedLong(n)), p.count)|}}
//   expected-remark@11{{macro content: |    }|}}
//   expected-remark@12{{macro content: |    let _pPtr = p.withUnsafeBufferPointer {|}}
//   expected-remark@13{{macro content: |        unsafe $0|}}
//   expected-remark@14{{macro content: |    }|}}
//   expected-remark@15{{macro content: |    defer {|}}
//   expected-remark@16{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@17{{macro content: |    }|}}
//   expected-remark@18{{macro content: |    return unsafe systemGlobalCompound(_pPtr.baseAddress, n)|}}
//   expected-remark@19{{macro content: |}|}}
// }}
void systemGlobalCompound(const int * __counted_by(kSystemCount + n) __noescape p, NSUInteger n);

//--- module.modulemap
module NormalModule {
  header "normal.h"
}

module SystemModule [system] {
  header "system.h"
}

//--- test.swift
import NormalModule
import SystemModule

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
func callNormalParamBare(_ p: Span<CInt>) {
  normalParamBare(p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
func callNormalParamCompound(_ p: Span<CInt>, _ n: UInt) {
  normalParamCompound(p, n)
}

func callNormalReturnBare(_ n: UInt) -> UnsafeMutableBufferPointer<CInt> {
  return unsafe normalReturnBare(n)
}

func callNormalReturnCompound(_ n: UInt) -> UnsafeMutableBufferPointer<CInt> {
  return unsafe normalReturnCompound(n)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
func callNormalGlobalCompound(_ p: Span<CInt>, _ n: UInt) {
  normalGlobalCompound(p, n)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
func callNormalUsesSystemGlobal(_ p: Span<CInt>, _ n: UInt) {
  normalUsesSystemGlobal(p, n)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
func callSystemParamBare(_ p: Span<CInt>) {
  systemParamBare(p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
func callSystemParamCompound(_ p: Span<CInt>, _ n: Int) {
  systemParamCompound(p, n)
}

func callSystemReturnBare(_ n: Int) -> UnsafeMutableBufferPointer<CInt> {
  return unsafe systemReturnBare(n)
}

func callSystemReturnCompound(_ n: Int) -> UnsafeMutableBufferPointer<CInt> {
  return unsafe systemReturnCompound(n)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
func callSystemGlobalCompound(_ p: Span<CInt>, _ n: Int) {
  systemGlobalCompound(p, n)
}
