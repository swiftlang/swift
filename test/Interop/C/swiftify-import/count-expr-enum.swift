// The expected expansions spell out the type of a C enum, which is `int`
// rather than `unsigned int` on Windows.
// UNSUPPORTED: OS=windows-msvc

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -I %t -strict-memory-safety \
// RUN:   %t%{fs-sep}test.swift -verify -verify-additional-file %t%{fs-sep}test.h -Rmacro-expansions -suppress-notes -eager-macro-checking

// FIXME: None of these wrappers compile, because their counts use a C enum
// parameter as if it were an integer. This test emits a module, so it
// type-checks the wrappers' bodies and expects the errors. A C enum is imported
// as a struct with a `rawValue`, or as a Swift `enum` if it's closed, but the
// wrappers convert it with `CUnsignedInt(e)` for a count like `e + 1`, and
// compute it with `E(exactly: p.count)!` for a count that is just `e`. Neither
// initializer exists for an enum
// (https://github.com/swiftlang/swift/issues/94246).

//--- test.h
#pragma once

#include <lifetimebound.h>
#include <ptrcheck.h>

enum E { E0, E1 };
enum __attribute__((enum_extensibility(closed))) CE { CE0, CE1 };

void enumBare(const int * __counted_by(e) __noescape p, enum E e);
// expected-expansion@-1:65{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func enumBare(_ p: Span<CInt>) {|}}
//   expected-error@3{{cannot force unwrap value of non-optional type 'E'}}
//   expected-error@3{{cannot convert value of type 'Int' to expected argument type}}
//   expected-error@3{{extraneous argument label 'exactly:' in call}}
//   expected-remark@3{{macro content: |    let e = E(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    let _pPtr = p.withUnsafeBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe enumBare(_pPtr.baseAddress, e)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
void enumCompound(const int * __counted_by(e + 1) __noescape p, enum E e);
// expected-expansion@-1:73{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func enumCompound(_ p: Span<CInt>, _ e: E) {|}}
//   expected-error@3{{initializer 'init(_:)' requires that 'E' conform to}}
//   expected-remark@3{{macro content: |    if p.count != (CUnsignedInt(e) + CUnsignedInt(CInt(1))) {|}}
//   expected-remark@4{{macro content: |      @inline(never) func _boundsCheckFailure<E: BinaryInteger, A: BinaryInteger>(_ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@5{{macro content: |        @inline(never) func _fail(_ function: StaticString, _ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@6{{macro content: |          fatalError("bounds check failure in \\(function): expected \\(expected) but got \\(actual)")|}}
//   expected-remark@7{{macro content: |        }|}}
//   expected-remark@8{{macro content: |        _fail("enumCompound", expected, actual)|}}
//   expected-remark@9{{macro content: |      }|}}
//   expected-error@10{{initializer 'init(_:)' requires that 'E' conform to}}
//   expected-remark@10{{macro content: |      _boundsCheckFailure((CUnsignedInt(e) + CUnsignedInt(CInt(1))), p.count)|}}
//   expected-remark@11{{macro content: |    }|}}
//   expected-remark@12{{macro content: |    let _pPtr = p.withUnsafeBufferPointer {|}}
//   expected-remark@13{{macro content: |        unsafe $0|}}
//   expected-remark@14{{macro content: |    }|}}
//   expected-remark@15{{macro content: |    defer {|}}
//   expected-remark@16{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@17{{macro content: |    }|}}
//   expected-remark@18{{macro content: |    return unsafe enumCompound(_pPtr.baseAddress, e)|}}
//   expected-remark@19{{macro content: |}|}}
// }}

// A closed enum is imported as a Swift `enum`.
void closedEnumBare(const int * __counted_by(e) __noescape p, enum CE e);
// expected-expansion@-1:72{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func closedEnumBare(_ p: Span<CInt>) {|}}
//   expected-error@3{{cannot convert value of type 'Int' to expected argument type}}
//   expected-error@3{{incorrect argument label in call (have 'exactly:', expected 'rawValue:')}}
//   expected-remark@3{{macro content: |    let e = CE(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    let _pPtr = p.withUnsafeBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe closedEnumBare(_pPtr.baseAddress, e)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
void closedEnumCompound(const int * __counted_by(e + 1) __noescape p, enum CE e);
// expected-expansion@-1:80{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func closedEnumCompound(_ p: Span<CInt>, _ e: CE) {|}}
//   expected-error@3{{initializer 'init(_:)' requires that 'CE' conform to}}
//   expected-remark@3{{macro content: |    if p.count != (CUnsignedInt(e) + CUnsignedInt(CInt(1))) {|}}
//   expected-remark@4{{macro content: |      @inline(never) func _boundsCheckFailure<E: BinaryInteger, A: BinaryInteger>(_ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@5{{macro content: |        @inline(never) func _fail(_ function: StaticString, _ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@6{{macro content: |          fatalError("bounds check failure in \\(function): expected \\(expected) but got \\(actual)")|}}
//   expected-remark@7{{macro content: |        }|}}
//   expected-remark@8{{macro content: |        _fail("closedEnumCompound", expected, actual)|}}
//   expected-remark@9{{macro content: |      }|}}
//   expected-error@10{{initializer 'init(_:)' requires that 'CE' conform to}}
//   expected-remark@10{{macro content: |      _boundsCheckFailure((CUnsignedInt(e) + CUnsignedInt(CInt(1))), p.count)|}}
//   expected-remark@11{{macro content: |    }|}}
//   expected-remark@12{{macro content: |    let _pPtr = p.withUnsafeBufferPointer {|}}
//   expected-remark@13{{macro content: |        unsafe $0|}}
//   expected-remark@14{{macro content: |    }|}}
//   expected-remark@15{{macro content: |    defer {|}}
//   expected-remark@16{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@17{{macro content: |    }|}}
//   expected-remark@18{{macro content: |    return unsafe closedEnumCompound(_pPtr.baseAddress, e)|}}
//   expected-remark@19{{macro content: |}|}}
// }}

//--- module.modulemap
module Test {
  header "test.h"
}

//--- test.swift
import Test

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
func callEnumBare(_ p: Span<CInt>) {
  enumBare(p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
func callEnumCompound(_ p: Span<CInt>, _ e: E) {
  enumCompound(p, e)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
func callClosedEnumBare(_ p: Span<CInt>) {
  closedEnumBare(p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
func callClosedEnumCompound(_ p: Span<CInt>, _ e: CE) {
  closedEnumCompound(p, e)
}
