// The expected expansions spell out the canonical type of `size_t`, which is
// `unsigned long long` on Windows and `unsigned int` on 32-bit targets.
// UNSUPPORTED: OS=windows-msvc
// UNSUPPORTED: PTRSIZE=32

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -I %t -strict-memory-safety \
// RUN:   %t/test.swift -verify -verify-additional-file %t%{fs-sep}test.h -Rmacro-expansions -suppress-notes -eager-macro-checking

// Check that count expressions mixing `size_t` (imported as `Int`) with values
// that C implicitly converts to `size_t` produce wrappers that type-check.

//--- test.h
#pragma once

#include <ptrcheck.h>
#include <stddef.h>

#define __noescape __attribute__((noescape))

// expected-expansion@+21:113{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func stridedLiteral(_ p: RawSpan, _ count: Int, _ stride: Int) {|}}
//   expected-remark@3{{macro content: |    if p.byteCount != ((((CUnsignedLong(count) - CUnsignedLong(CInt(1)))) * CUnsignedLong(stride)) + CUnsignedLong(CInt(12))) {|}}
//   expected-remark@4{{macro content: |      @inline(never) func _boundsCheckFailure<E: BinaryInteger, A: BinaryInteger>(_ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@5{{macro content: |        @inline(never) func _fail(_ function: StaticString, _ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@6{{macro content: |          fatalError("bounds check failure in \\(function): expected \\(expected) but got \\(actual)")|}}
//   expected-remark@7{{macro content: |        }|}}
//   expected-remark@8{{macro content: |        _fail("stridedLiteral", expected, actual)|}}
//   expected-remark@9{{macro content: |      }|}}
//   expected-remark@10{{macro content: |      _boundsCheckFailure(((((CUnsignedLong(count) - CUnsignedLong(CInt(1)))) * CUnsignedLong(stride)) + CUnsignedLong(CInt(12))), p.byteCount)|}}
//   expected-remark@11{{macro content: |    }|}}
//   expected-remark@12{{macro content: |    let _pPtr = p.withUnsafeBytes {|}}
//   expected-remark@13{{macro content: |        unsafe $0|}}
//   expected-remark@14{{macro content: |    }|}}
//   expected-remark@15{{macro content: |    defer {|}}
//   expected-remark@16{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@17{{macro content: |    }|}}
//   expected-remark@18{{macro content: |    return unsafe stridedLiteral(_pPtr.baseAddress, count, stride)|}}
//   expected-remark@19{{macro content: |}|}}
// }}
void stridedLiteral(const void * __sized_by((count - 1) * stride + 12) __noescape p, size_t count, size_t stride);

// expected-expansion@+21:126{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func stridedParam(_ p: RawSpan, _ count: Int, _ stride: Int, _ size: Int) {|}}
//   expected-remark@3{{macro content: |    if p.byteCount != ((((CUnsignedLong(count) - CUnsignedLong(CInt(1)))) * CUnsignedLong(stride)) + CUnsignedLong(size)) {|}}
//   expected-remark@4{{macro content: |      @inline(never) func _boundsCheckFailure<E: BinaryInteger, A: BinaryInteger>(_ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@5{{macro content: |        @inline(never) func _fail(_ function: StaticString, _ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@6{{macro content: |          fatalError("bounds check failure in \\(function): expected \\(expected) but got \\(actual)")|}}
//   expected-remark@7{{macro content: |        }|}}
//   expected-remark@8{{macro content: |        _fail("stridedParam", expected, actual)|}}
//   expected-remark@9{{macro content: |      }|}}
//   expected-remark@10{{macro content: |      _boundsCheckFailure(((((CUnsignedLong(count) - CUnsignedLong(CInt(1)))) * CUnsignedLong(stride)) + CUnsignedLong(size)), p.byteCount)|}}
//   expected-remark@11{{macro content: |    }|}}
//   expected-remark@12{{macro content: |    let _pPtr = p.withUnsafeBytes {|}}
//   expected-remark@13{{macro content: |        unsafe $0|}}
//   expected-remark@14{{macro content: |    }|}}
//   expected-remark@15{{macro content: |    defer {|}}
//   expected-remark@16{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@17{{macro content: |    }|}}
//   expected-remark@18{{macro content: |    return unsafe stridedParam(_pPtr.baseAddress, count, stride, size)|}}
//   expected-remark@19{{macro content: |}|}}
// }}
void stridedParam(const void * __sized_by((count - 1) * stride + size) __noescape p, size_t count, size_t stride, size_t size);

// `unsigned long` is imported as `CUnsignedLong`, which is also the Swift type
// of `size_t`'s canonical type, so only `count` is converted.
// expected-expansion@+21:113{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func sizeTTimesUnsignedLong(_ p: RawSpan, _ count: Int, _ size: CUnsignedLong) {|}}
//   expected-remark@3{{macro content: |    if p.byteCount != (CUnsignedLong(count) * size) {|}}
//   expected-remark@4{{macro content: |      @inline(never) func _boundsCheckFailure<E: BinaryInteger, A: BinaryInteger>(_ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@5{{macro content: |        @inline(never) func _fail(_ function: StaticString, _ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@6{{macro content: |          fatalError("bounds check failure in \\(function): expected \\(expected) but got \\(actual)")|}}
//   expected-remark@7{{macro content: |        }|}}
//   expected-remark@8{{macro content: |        _fail("sizeTTimesUnsignedLong", expected, actual)|}}
//   expected-remark@9{{macro content: |      }|}}
//   expected-remark@10{{macro content: |      _boundsCheckFailure((CUnsignedLong(count) * size), p.byteCount)|}}
//   expected-remark@11{{macro content: |    }|}}
//   expected-remark@12{{macro content: |    let _pPtr = p.withUnsafeBytes {|}}
//   expected-remark@13{{macro content: |        unsafe $0|}}
//   expected-remark@14{{macro content: |    }|}}
//   expected-remark@15{{macro content: |    defer {|}}
//   expected-remark@16{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@17{{macro content: |    }|}}
//   expected-remark@18{{macro content: |    return unsafe sizeTTimesUnsignedLong(_pPtr.baseAddress, count, size)|}}
//   expected-remark@19{{macro content: |}|}}
// }}
void sizeTTimesUnsignedLong(const void * __sized_by(count * size) __noescape p, size_t count, unsigned long size);

// expected-expansion@+21:79{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func sizeofMul(_ p: RawSpan, _ n: Int) {|}}
//   expected-remark@3{{macro content: |    if p.byteCount != (CUnsignedLong(n) * CUnsignedLong(4)) {|}}
//   expected-remark@4{{macro content: |      @inline(never) func _boundsCheckFailure<E: BinaryInteger, A: BinaryInteger>(_ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@5{{macro content: |        @inline(never) func _fail(_ function: StaticString, _ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@6{{macro content: |          fatalError("bounds check failure in \\(function): expected \\(expected) but got \\(actual)")|}}
//   expected-remark@7{{macro content: |        }|}}
//   expected-remark@8{{macro content: |        _fail("sizeofMul", expected, actual)|}}
//   expected-remark@9{{macro content: |      }|}}
//   expected-remark@10{{macro content: |      _boundsCheckFailure((CUnsignedLong(n) * CUnsignedLong(4)), p.byteCount)|}}
//   expected-remark@11{{macro content: |    }|}}
//   expected-remark@12{{macro content: |    let _pPtr = p.withUnsafeBytes {|}}
//   expected-remark@13{{macro content: |        unsafe $0|}}
//   expected-remark@14{{macro content: |    }|}}
//   expected-remark@15{{macro content: |    defer {|}}
//   expected-remark@16{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@17{{macro content: |    }|}}
//   expected-remark@18{{macro content: |    return unsafe sizeofMul(_pPtr.baseAddress, n)|}}
//   expected-remark@19{{macro content: |}|}}
// }}
void sizeofMul(const void * __sized_by(n * sizeof(int)) __noescape p, size_t n);

// expected-expansion@+21:83{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func mixedParams(_ p: Span<CInt>, _ len: CInt, _ n: Int) {|}}
//   expected-remark@3{{macro content: |    if p.count != (CUnsignedLong(len) + CUnsignedLong(n)) {|}}
//   expected-remark@4{{macro content: |      @inline(never) func _boundsCheckFailure<E: BinaryInteger, A: BinaryInteger>(_ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@5{{macro content: |        @inline(never) func _fail(_ function: StaticString, _ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@6{{macro content: |          fatalError("bounds check failure in \\(function): expected \\(expected) but got \\(actual)")|}}
//   expected-remark@7{{macro content: |        }|}}
//   expected-remark@8{{macro content: |        _fail("mixedParams", expected, actual)|}}
//   expected-remark@9{{macro content: |      }|}}
//   expected-remark@10{{macro content: |      _boundsCheckFailure((CUnsignedLong(len) + CUnsignedLong(n)), p.count)|}}
//   expected-remark@11{{macro content: |    }|}}
//   expected-remark@12{{macro content: |    let _pPtr = p.withUnsafeBufferPointer {|}}
//   expected-remark@13{{macro content: |        unsafe $0|}}
//   expected-remark@14{{macro content: |    }|}}
//   expected-remark@15{{macro content: |    defer {|}}
//   expected-remark@16{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@17{{macro content: |    }|}}
//   expected-remark@18{{macro content: |    return unsafe mixedParams(_pPtr.baseAddress, len, n)|}}
//   expected-remark@19{{macro content: |}|}}
// }}
void mixedParams(const int * __counted_by(len + n) __noescape p, int len, size_t n);

// expected-expansion@+21:88{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func explicitCast(_ p: Span<CInt>, _ n: Int, _ m: CInt) {|}}
//   expected-remark@3{{macro content: |    if p.count != (CUnsignedLong(n) + CUnsignedLong(truncatingIfNeeded: m)) {|}}
//   expected-remark@4{{macro content: |      @inline(never) func _boundsCheckFailure<E: BinaryInteger, A: BinaryInteger>(_ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@5{{macro content: |        @inline(never) func _fail(_ function: StaticString, _ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@6{{macro content: |          fatalError("bounds check failure in \\(function): expected \\(expected) but got \\(actual)")|}}
//   expected-remark@7{{macro content: |        }|}}
//   expected-remark@8{{macro content: |        _fail("explicitCast", expected, actual)|}}
//   expected-remark@9{{macro content: |      }|}}
//   expected-remark@10{{macro content: |      _boundsCheckFailure((CUnsignedLong(n) + CUnsignedLong(truncatingIfNeeded: m)), p.count)|}}
//   expected-remark@11{{macro content: |    }|}}
//   expected-remark@12{{macro content: |    let _pPtr = p.withUnsafeBufferPointer {|}}
//   expected-remark@13{{macro content: |        unsafe $0|}}
//   expected-remark@14{{macro content: |    }|}}
//   expected-remark@15{{macro content: |    defer {|}}
//   expected-remark@16{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@17{{macro content: |    }|}}
//   expected-remark@18{{macro content: |    return unsafe explicitCast(_pPtr.baseAddress, n, m)|}}
//   expected-remark@19{{macro content: |}|}}
// }}
void explicitCast(const int * __counted_by(n + (size_t)m) __noescape p, size_t n, int m);

struct S { int x; };

// Imported as a method of `S`, where `n` is the second Swift parameter rather
// than the third, and also as an obsoleted global function that points to it,
// where `n` is still the third.
// expected-expansion@+44:18{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@available(swift, obsoleted: 3, renamed: "S.selfInMiddle(_:self:_:)") @_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload|}}
//   expected-remark@3{{macro content: |public func selfInMiddle(_ p: Span<CInt>, _ s: UnsafeMutablePointer<S>!, _ n: Int) {|}}
//   expected-remark@4{{macro content: |    if p.count != (CUnsignedLong(n) + CUnsignedLong(CInt(1))) {|}}
//   expected-remark@5{{macro content: |      @inline(never) func _boundsCheckFailure<E: BinaryInteger, A: BinaryInteger>(_ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@6{{macro content: |        @inline(never) func _fail(_ function: StaticString, _ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@7{{macro content: |          fatalError("bounds check failure in \\(function): expected \\(expected) but got \\(actual)")|}}
//   expected-remark@8{{macro content: |        }|}}
//   expected-remark@9{{macro content: |        _fail("selfInMiddle", expected, actual)|}}
//   expected-remark@10{{macro content: |      }|}}
//   expected-remark@11{{macro content: |      _boundsCheckFailure((CUnsignedLong(n) + CUnsignedLong(CInt(1))), p.count)|}}
//   expected-remark@12{{macro content: |    }|}}
//   expected-remark@13{{macro content: |    let _pPtr = p.withUnsafeBufferPointer {|}}
//   expected-remark@14{{macro content: |        unsafe $0|}}
//   expected-remark@15{{macro content: |    }|}}
//   expected-remark@16{{macro content: |    defer {|}}
//   expected-remark@17{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@18{{macro content: |    }|}}
//   expected-remark@19{{macro content: |    return unsafe selfInMiddle(_pPtr.baseAddress, s, n)|}}
//   expected-remark@20{{macro content: |}|}}
// }}
// expected-expansion@+22:86{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload|}}
//   expected-remark@3{{macro content: |public mutating func selfInMiddle(_ p: Span<CInt>, _ n: Int) {|}}
//   expected-remark@4{{macro content: |    if p.count != (CUnsignedLong(n) + CUnsignedLong(CInt(1))) {|}}
//   expected-remark@5{{macro content: |      @inline(never) func _boundsCheckFailure<E: BinaryInteger, A: BinaryInteger>(_ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@6{{macro content: |        @inline(never) func _fail(_ function: StaticString, _ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@7{{macro content: |          fatalError("bounds check failure in \\(function): expected \\(expected) but got \\(actual)")|}}
//   expected-remark@8{{macro content: |        }|}}
//   expected-remark@9{{macro content: |        _fail("selfInMiddle", expected, actual)|}}
//   expected-remark@10{{macro content: |      }|}}
//   expected-remark@11{{macro content: |      _boundsCheckFailure((CUnsignedLong(n) + CUnsignedLong(CInt(1))), p.count)|}}
//   expected-remark@12{{macro content: |    }|}}
//   expected-remark@13{{macro content: |    let _pPtr = p.withUnsafeBufferPointer {|}}
//   expected-remark@14{{macro content: |        unsafe $0|}}
//   expected-remark@15{{macro content: |    }|}}
//   expected-remark@16{{macro content: |    defer {|}}
//   expected-remark@17{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@18{{macro content: |    }|}}
//   expected-remark@19{{macro content: |    return unsafe selfInMiddle(_pPtr.baseAddress, n)|}}
//   expected-remark@20{{macro content: |}|}}
// }}
void selfInMiddle(const int * __counted_by(n + 1) __noescape p, struct S *s, size_t n) __attribute__((swift_name("S.selfInMiddle(_:self:_:)")));

// A count that is just the name of a parameter is emitted without a
// conversion, so that _SwiftifyImport can remove `n` from the wrapper and get
// the count from `p`.
// expected-expansion@+13:66{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func bareParam(_ p: Span<CInt>) {|}}
//   expected-remark@3{{macro content: |    let n = p.count|}}
//   expected-remark@4{{macro content: |    let _pPtr = p.withUnsafeBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe bareParam(_pPtr.baseAddress, n)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
void bareParam(const int * __counted_by(n) __noescape p, size_t n);

// An explicit cast that doesn't change the type, here because `n` is already a
// `size_t`, still leaves a count that is just the name of the parameter. So
// `n` is removed from the wrapper, as for `bareParam`.
// expected-expansion@+13:77{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func noopCastBare(_ p: Span<CInt>) {|}}
//   expected-remark@3{{macro content: |    let n = p.count|}}
//   expected-remark@4{{macro content: |    let _pPtr = p.withUnsafeBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe noopCastBare(_pPtr.baseAddress, n)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
void noopCastBare(const int * __counted_by((size_t)n) __noescape p, size_t n);

// A cast that changes the type, here from `unsigned int` to `size_t`, isn't
// just the name of the parameter. So `n` stays a parameter of the wrapper, and
// the count is checked against `p`.
// expected-expansion@+21:87{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func integralCastBare(_ p: Span<CInt>, _ n: CUnsignedInt) {|}}
//   expected-remark@3{{macro content: |    if p.count != CUnsignedLong(truncatingIfNeeded: n) {|}}
//   expected-remark@4{{macro content: |      @inline(never) func _boundsCheckFailure<E: BinaryInteger, A: BinaryInteger>(_ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@5{{macro content: |        @inline(never) func _fail(_ function: StaticString, _ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@6{{macro content: |          fatalError("bounds check failure in \\(function): expected \\(expected) but got \\(actual)")|}}
//   expected-remark@7{{macro content: |        }|}}
//   expected-remark@8{{macro content: |        _fail("integralCastBare", expected, actual)|}}
//   expected-remark@9{{macro content: |      }|}}
//   expected-remark@10{{macro content: |      _boundsCheckFailure(CUnsignedLong(truncatingIfNeeded: n), p.count)|}}
//   expected-remark@11{{macro content: |    }|}}
//   expected-remark@12{{macro content: |    let _pPtr = p.withUnsafeBufferPointer {|}}
//   expected-remark@13{{macro content: |        unsafe $0|}}
//   expected-remark@14{{macro content: |    }|}}
//   expected-remark@15{{macro content: |    defer {|}}
//   expected-remark@16{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@17{{macro content: |    }|}}
//   expected-remark@18{{macro content: |    return unsafe integralCastBare(_pPtr.baseAddress, n)|}}
//   expected-remark@19{{macro content: |}|}}
// }}
void integralCastBare(const int * __counted_by((size_t)n) __noescape p, unsigned int n);

// A narrowing cast of a `size_t`, here to `int`, isn't just the name of the
// parameter either. The cast truncates like C does, but `n` is first converted
// to `size_t`'s canonical type like any other reference, so a negative `n`
// traps.
// expected-expansion@+21:79{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func narrowingCastBare(_ p: Span<CInt>, _ n: Int) {|}}
//   expected-remark@3{{macro content: |    if p.count != CInt(truncatingIfNeeded: CUnsignedLong(n)) {|}}
//   expected-remark@4{{macro content: |      @inline(never) func _boundsCheckFailure<E: BinaryInteger, A: BinaryInteger>(_ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@5{{macro content: |        @inline(never) func _fail(_ function: StaticString, _ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@6{{macro content: |          fatalError("bounds check failure in \\(function): expected \\(expected) but got \\(actual)")|}}
//   expected-remark@7{{macro content: |        }|}}
//   expected-remark@8{{macro content: |        _fail("narrowingCastBare", expected, actual)|}}
//   expected-remark@9{{macro content: |      }|}}
//   expected-remark@10{{macro content: |      _boundsCheckFailure(CInt(truncatingIfNeeded: CUnsignedLong(n)), p.count)|}}
//   expected-remark@11{{macro content: |    }|}}
//   expected-remark@12{{macro content: |    let _pPtr = p.withUnsafeBufferPointer {|}}
//   expected-remark@13{{macro content: |        unsafe $0|}}
//   expected-remark@14{{macro content: |    }|}}
//   expected-remark@15{{macro content: |    defer {|}}
//   expected-remark@16{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@17{{macro content: |    }|}}
//   expected-remark@18{{macro content: |    return unsafe narrowingCastBare(_pPtr.baseAddress, n)|}}
//   expected-remark@19{{macro content: |}|}}
// }}
void narrowingCastBare(const int * __counted_by((int)n) __noescape p, size_t n);


// FIXME: A bare count like this one isn't converted at all. So in an optimized
// build, a negative `len` gives the returned buffer a negative count, because
// `UnsafeMutableBufferPointer` only checks for one in debug builds.
// (https://github.com/swiftlang/swift/issues/94221)
// expected-expansion@+6:46{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @_disfavoredOverload public func bareReturn(_ len: Int) -> UnsafeMutableBufferPointer<CInt> {|}}
//   expected-remark@3{{macro content: |    return unsafe UnsafeMutableBufferPointer<CInt>(start: unsafe bareReturn(len), count: Int(len))|}}
//   expected-remark@4{{macro content: |}|}}
// }}
int * __counted_by(len) bareReturn(size_t len);

// FIXME: The conversion traps on a negative `len`, but only after C has been
// called. (https://github.com/swiftlang/swift/issues/94218)
// expected-expansion@+6:54{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @_disfavoredOverload public func compoundReturn(_ len: Int) -> UnsafeMutableBufferPointer<CInt> {|}}
//   expected-remark@3{{macro content: |    return unsafe UnsafeMutableBufferPointer<CInt>(start: unsafe compoundReturn(len), count: Int((CUnsignedLong(len) + CUnsignedLong(CInt(1)))))|}}
//   expected-remark@4{{macro content: |}|}}
// }}
int * __counted_by(len + 1) compoundReturn(size_t len);

// Count expressions can refer to a `const` global, but not a mutable one. With
// no initializer it isn't folded to a literal, and Swift imports it as `Int`.
extern const size_t kExternCount;

// expected-expansion@+21:91{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func externConstCompound(_ p: Span<CInt>, _ n: Int) {|}}
//   expected-remark@3{{macro content: |    if p.count != (CUnsignedLong(kExternCount) + CUnsignedLong(n)) {|}}
//   expected-remark@4{{macro content: |      @inline(never) func _boundsCheckFailure<E: BinaryInteger, A: BinaryInteger>(_ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@5{{macro content: |        @inline(never) func _fail(_ function: StaticString, _ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@6{{macro content: |          fatalError("bounds check failure in \\(function): expected \\(expected) but got \\(actual)")|}}
//   expected-remark@7{{macro content: |        }|}}
//   expected-remark@8{{macro content: |        _fail("externConstCompound", expected, actual)|}}
//   expected-remark@9{{macro content: |      }|}}
//   expected-remark@10{{macro content: |      _boundsCheckFailure((CUnsignedLong(kExternCount) + CUnsignedLong(n)), p.count)|}}
//   expected-remark@11{{macro content: |    }|}}
//   expected-remark@12{{macro content: |    let _pPtr = p.withUnsafeBufferPointer {|}}
//   expected-remark@13{{macro content: |        unsafe $0|}}
//   expected-remark@14{{macro content: |    }|}}
//   expected-remark@15{{macro content: |    defer {|}}
//   expected-remark@16{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@17{{macro content: |    }|}}
//   expected-remark@18{{macro content: |    return unsafe externConstCompound(_pPtr.baseAddress, n)|}}
//   expected-remark@19{{macro content: |}|}}
// }}
void externConstCompound(const int * __counted_by(kExternCount + n) __noescape p, size_t n);

// A global whose type is written using a call to another function. Getting the
// global's type doesn't depend on importing that function. The test calls both
// functions, starting with `typeofGlobalCompound`, so that `kTypeofCount`'s type
// is needed before `typeofHelper` has been imported.
// expected-expansion@+13:71{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func typeofHelper(_ p: Span<CInt>) -> Int {|}}
//   expected-remark@3{{macro content: |    let n = p.count|}}
//   expected-remark@4{{macro content: |    let _pPtr = p.withUnsafeBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe typeofHelper(_pPtr.baseAddress, n)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
size_t typeofHelper(const int * __counted_by(n) __noescape p, size_t n);
extern const __typeof__(typeofHelper(0, 0)) kTypeofCount;

// expected-expansion@+21:92{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func typeofGlobalCompound(_ p: Span<CInt>, _ n: Int) {|}}
//   expected-remark@3{{macro content: |    if p.count != (CUnsignedLong(kTypeofCount) + CUnsignedLong(n)) {|}}
//   expected-remark@4{{macro content: |      @inline(never) func _boundsCheckFailure<E: BinaryInteger, A: BinaryInteger>(_ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@5{{macro content: |        @inline(never) func _fail(_ function: StaticString, _ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@6{{macro content: |          fatalError("bounds check failure in \\(function): expected \\(expected) but got \\(actual)")|}}
//   expected-remark@7{{macro content: |        }|}}
//   expected-remark@8{{macro content: |        _fail("typeofGlobalCompound", expected, actual)|}}
//   expected-remark@9{{macro content: |      }|}}
//   expected-remark@10{{macro content: |      _boundsCheckFailure((CUnsignedLong(kTypeofCount) + CUnsignedLong(n)), p.count)|}}
//   expected-remark@11{{macro content: |    }|}}
//   expected-remark@12{{macro content: |    let _pPtr = p.withUnsafeBufferPointer {|}}
//   expected-remark@13{{macro content: |        unsafe $0|}}
//   expected-remark@14{{macro content: |    }|}}
//   expected-remark@15{{macro content: |    defer {|}}
//   expected-remark@16{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@17{{macro content: |    }|}}
//   expected-remark@18{{macro content: |    return unsafe typeofGlobalCompound(_pPtr.baseAddress, n)|}}
//   expected-remark@19{{macro content: |}|}}
// }}
void typeofGlobalCompound(const int * __counted_by(kTypeofCount + n) __noescape p, size_t n);

// A `const` global with an initializer is folded to a literal by clang, so the
// emitter never sees a reference to it.
static const size_t kStaticCount = 4;

// expected-expansion@+21:73{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func staticConstBare(_ p: Span<CInt>) {|}}
//   expected-remark@3{{macro content: |    if p.count != CUnsignedLong(4) {|}}
//   expected-remark@4{{macro content: |      @inline(never) func _boundsCheckFailure<E: BinaryInteger, A: BinaryInteger>(_ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@5{{macro content: |        @inline(never) func _fail(_ function: StaticString, _ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@6{{macro content: |          fatalError("bounds check failure in \\(function): expected \\(expected) but got \\(actual)")|}}
//   expected-remark@7{{macro content: |        }|}}
//   expected-remark@8{{macro content: |        _fail("staticConstBare", expected, actual)|}}
//   expected-remark@9{{macro content: |      }|}}
//   expected-remark@10{{macro content: |      _boundsCheckFailure(CUnsignedLong(4), p.count)|}}
//   expected-remark@11{{macro content: |    }|}}
//   expected-remark@12{{macro content: |    let _pPtr = p.withUnsafeBufferPointer {|}}
//   expected-remark@13{{macro content: |        unsafe $0|}}
//   expected-remark@14{{macro content: |    }|}}
//   expected-remark@15{{macro content: |    defer {|}}
//   expected-remark@16{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@17{{macro content: |    }|}}
//   expected-remark@18{{macro content: |    return unsafe staticConstBare(_pPtr.baseAddress)|}}
//   expected-remark@19{{macro content: |}|}}
// }}
void staticConstBare(const int * __counted_by(kStaticCount) __noescape p);

// expected-expansion@+21:91{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func staticConstCompound(_ p: Span<CInt>, _ n: Int) {|}}
//   expected-remark@3{{macro content: |    if p.count != (CUnsignedLong(4) + CUnsignedLong(n)) {|}}
//   expected-remark@4{{macro content: |      @inline(never) func _boundsCheckFailure<E: BinaryInteger, A: BinaryInteger>(_ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@5{{macro content: |        @inline(never) func _fail(_ function: StaticString, _ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@6{{macro content: |          fatalError("bounds check failure in \\(function): expected \\(expected) but got \\(actual)")|}}
//   expected-remark@7{{macro content: |        }|}}
//   expected-remark@8{{macro content: |        _fail("staticConstCompound", expected, actual)|}}
//   expected-remark@9{{macro content: |      }|}}
//   expected-remark@10{{macro content: |      _boundsCheckFailure((CUnsignedLong(4) + CUnsignedLong(n)), p.count)|}}
//   expected-remark@11{{macro content: |    }|}}
//   expected-remark@12{{macro content: |    let _pPtr = p.withUnsafeBufferPointer {|}}
//   expected-remark@13{{macro content: |        unsafe $0|}}
//   expected-remark@14{{macro content: |    }|}}
//   expected-remark@15{{macro content: |    defer {|}}
//   expected-remark@16{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@17{{macro content: |    }|}}
//   expected-remark@18{{macro content: |    return unsafe staticConstCompound(_pPtr.baseAddress, n)|}}
//   expected-remark@19{{macro content: |}|}}
// }}
void staticConstCompound(const int * __counted_by(kStaticCount + n) __noescape p, size_t n);

//--- module.modulemap
module Test {
  header "test.h"
}

//--- test.swift
import Test

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
func callStridedLiteral(_ p: RawSpan, _ count: Int, _ stride: Int) {
  stridedLiteral(p, count, stride)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
func callStridedParam(_ p: RawSpan, _ count: Int, _ stride: Int, _ size: Int) {
  stridedParam(p, count, stride, size)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
func callSizeTTimesUnsignedLong(_ p: RawSpan, _ count: Int, _ size: CUnsignedLong) {
  sizeTTimesUnsignedLong(p, count, size)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
func callSizeofMul(_ p: RawSpan, _ n: Int) {
  sizeofMul(p, n)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
func callMixedParams(_ p: Span<CInt>, _ len: CInt, _ n: Int) {
  mixedParams(p, len, n)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
func callExplicitCast(_ p: Span<CInt>, _ n: Int, _ m: CInt) {
  explicitCast(p, n, m)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
func callSelfInMiddle(_ s: inout S, _ p: Span<CInt>, _ n: Int) {
  s.selfInMiddle(p, n)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
func callBareParam(_ p: Span<CInt>) {
  bareParam(p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
func callNoopCastBare(_ p: Span<CInt>) {
  noopCastBare(p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
func callIntegralCastBare(_ p: Span<CInt>, _ n: CUnsignedInt) {
  integralCastBare(p, n)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
func callNarrowingCastBare(_ p: Span<CInt>, _ n: Int) {
  narrowingCastBare(p, n)
}

func callBareReturn(_ len: Int) -> UnsafeMutableBufferPointer<CInt> {
  return unsafe bareReturn(len)
}

func callCompoundReturn(_ len: Int) -> UnsafeMutableBufferPointer<CInt> {
  return unsafe compoundReturn(len)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
func callExternConstCompound(_ p: Span<CInt>, _ n: Int) {
  externConstCompound(p, n)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
func callTypeofGlobalCompound(_ p: Span<CInt>, _ n: Int) {
  typeofGlobalCompound(p, n)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
func callTypeofHelper(_ p: Span<CInt>) -> Int {
  return typeofHelper(p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
func callStaticConstBare(_ p: Span<CInt>) {
  staticConstBare(p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
func callStaticConstCompound(_ p: Span<CInt>, _ n: Int) {
  staticConstCompound(p, n)
}
