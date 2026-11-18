// `NS_ENUM` and `NS_CLOSED_ENUM` come from Foundation in Apple SDKs.
// REQUIRES: VENDOR=apple
// The expected expansions spell out `NSInteger`'s type, which is `int` on
// 32-bit targets.
// UNSUPPORTED: PTRSIZE=32

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -I %t -strict-memory-safety \
// RUN:   %t%{fs-sep}test.swift -verify -verify-additional-file %t%{fs-sep}test.h -Rmacro-expansions -suppress-notes -eager-macro-checking

// FIXME: None of these wrappers compile, because their counts use an `NS_ENUM`
// or `NS_CLOSED_ENUM` parameter as if it were an integer. This test emits a
// module, so it type-checks the wrappers' bodies and expects the errors. These
// enums are imported as a Swift `enum`, but the wrappers convert the value with
// `CLong(e)` for a count like `e + 1`, and compute it with
// `OpenEnum(exactly: p.count)!` for a count that is just `e`. Neither
// initializer exists for an enum
// (https://github.com/swiftlang/swift/issues/94246).

//--- test.h
#pragma once

#include <Foundation/NSObjCRuntime.h>
#include <lifetimebound.h>
#include <ptrcheck.h>

typedef NS_ENUM(NSInteger, OpenEnum) { OpenEnum0, OpenEnum1 };
typedef NS_CLOSED_ENUM(NSInteger, ClosedEnum) { ClosedEnum0, ClosedEnum1 };

void nsEnumBare(const int * __counted_by(e) __noescape p, OpenEnum e);
// expected-expansion@-1:69{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func nsEnumBare(_ p: Span<CInt>) {|}}
//   expected-error@3{{incorrect argument label in call (have 'exactly:', expected 'rawValue:')}}
//   expected-remark@3{{macro content: |    let e = OpenEnum(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    let _pPtr = p.withUnsafeBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe nsEnumBare(_pPtr.baseAddress, e)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
void nsEnumCompound(const int * __counted_by(e + 1) __noescape p, OpenEnum e);
// expected-expansion@-1:77{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func nsEnumCompound(_ p: Span<CInt>, _ e: OpenEnum) {|}}
//   expected-error@3{{initializer 'init(_:)' requires that 'OpenEnum' conform to}}
//   expected-remark@3{{macro content: |    if p.count != (CLong(e) + CLong(CInt(1))) {|}}
//   expected-remark@4{{macro content: |      @inline(never) func _boundsCheckFailure<E: BinaryInteger, A: BinaryInteger>(_ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@5{{macro content: |        @inline(never) func _fail(_ function: StaticString, _ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@6{{macro content: |          fatalError("bounds check failure in \\(function): expected \\(expected) but got \\(actual)")|}}
//   expected-remark@7{{macro content: |        }|}}
//   expected-remark@8{{macro content: |        _fail("nsEnumCompound", expected, actual)|}}
//   expected-remark@9{{macro content: |      }|}}
//   expected-error@10{{initializer 'init(_:)' requires that 'OpenEnum' conform to}}
//   expected-remark@10{{macro content: |      _boundsCheckFailure((CLong(e) + CLong(CInt(1))), p.count)|}}
//   expected-remark@11{{macro content: |    }|}}
//   expected-remark@12{{macro content: |    let _pPtr = p.withUnsafeBufferPointer {|}}
//   expected-remark@13{{macro content: |        unsafe $0|}}
//   expected-remark@14{{macro content: |    }|}}
//   expected-remark@15{{macro content: |    defer {|}}
//   expected-remark@16{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@17{{macro content: |    }|}}
//   expected-remark@18{{macro content: |    return unsafe nsEnumCompound(_pPtr.baseAddress, e)|}}
//   expected-remark@19{{macro content: |}|}}
// }}
void nsClosedEnumBare(const int * __counted_by(e) __noescape p, ClosedEnum e);
// expected-expansion@-1:77{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func nsClosedEnumBare(_ p: Span<CInt>) {|}}
//   expected-error@3{{incorrect argument label in call (have 'exactly:', expected 'rawValue:')}}
//   expected-remark@3{{macro content: |    let e = ClosedEnum(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    let _pPtr = p.withUnsafeBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe nsClosedEnumBare(_pPtr.baseAddress, e)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
void nsClosedEnumCompound(const int * __counted_by(e + 1) __noescape p, ClosedEnum e);
// expected-expansion@-1:85{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func nsClosedEnumCompound(_ p: Span<CInt>, _ e: ClosedEnum) {|}}
//   expected-error@3{{initializer 'init(_:)' requires that 'ClosedEnum' conform to}}
//   expected-remark@3{{macro content: |    if p.count != (CLong(e) + CLong(CInt(1))) {|}}
//   expected-remark@4{{macro content: |      @inline(never) func _boundsCheckFailure<E: BinaryInteger, A: BinaryInteger>(_ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@5{{macro content: |        @inline(never) func _fail(_ function: StaticString, _ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@6{{macro content: |          fatalError("bounds check failure in \\(function): expected \\(expected) but got \\(actual)")|}}
//   expected-remark@7{{macro content: |        }|}}
//   expected-remark@8{{macro content: |        _fail("nsClosedEnumCompound", expected, actual)|}}
//   expected-remark@9{{macro content: |      }|}}
//   expected-error@10{{initializer 'init(_:)' requires that 'ClosedEnum' conform to}}
//   expected-remark@10{{macro content: |      _boundsCheckFailure((CLong(e) + CLong(CInt(1))), p.count)|}}
//   expected-remark@11{{macro content: |    }|}}
//   expected-remark@12{{macro content: |    let _pPtr = p.withUnsafeBufferPointer {|}}
//   expected-remark@13{{macro content: |        unsafe $0|}}
//   expected-remark@14{{macro content: |    }|}}
//   expected-remark@15{{macro content: |    defer {|}}
//   expected-remark@16{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@17{{macro content: |    }|}}
//   expected-remark@18{{macro content: |    return unsafe nsClosedEnumCompound(_pPtr.baseAddress, e)|}}
//   expected-remark@19{{macro content: |}|}}
// }}

//--- module.modulemap
module Test {
  header "test.h"
}

//--- test.swift
import Test

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
func callNSEnumBare(_ p: Span<CInt>) {
  nsEnumBare(p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
func callNSEnumCompound(_ p: Span<CInt>, _ e: OpenEnum) {
  nsEnumCompound(p, e)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
func callNSClosedEnumBare(_ p: Span<CInt>) {
  nsClosedEnumBare(p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
func callNSClosedEnumCompound(_ p: Span<CInt>, _ e: ClosedEnum) {
  nsClosedEnumCompound(p, e)
}
