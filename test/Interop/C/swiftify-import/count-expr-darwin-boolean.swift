// `Boolean` comes from `MacTypes.h` in Apple SDKs.
// REQUIRES: VENDOR=apple

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -I %t -strict-memory-safety \
// RUN:   %t%{fs-sep}test.swift -verify -verify-additional-file %t%{fs-sep}test.h -Rmacro-expansions -suppress-notes -eager-macro-checking

// FIXME: Neither wrapper compiles, because their counts use a `Boolean`
// parameter as if it were an integer. This test emits a module, so it
// type-checks the wrappers' bodies and expects the errors. `Boolean` is a
// typedef for `unsigned char`, but Swift imports it as `Bool` when the `Darwin`
// module is loaded, as it is here. The wrappers convert it with `CInt(b)` for
// `b + 1`, and compute it with `Bool(exactly: p.count)!` for a count that is
// just `b`. There's no `CInt` initializer that takes a `Bool`, and no
// `Bool(exactly:)` that takes an `Int`
// (https://github.com/swiftlang/swift/issues/94420).

//--- test.h
#pragma once

#include <MacTypes.h>
#include <lifetimebound.h>
#include <ptrcheck.h>

void booleanBare(const int * __counted_by(b) __noescape p, Boolean b);
// expected-expansion@-1:69{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func booleanBare(_ p: Span<CInt>) {|}}
//   expected-error@3{{extraneous argument label 'exactly:' in call}}
//   expected-error@3{{cannot convert value of type 'Int' to expected argument type}}
//   expected-remark@3{{macro content: |    let b = Bool(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    let _pPtr = p.withUnsafeBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe booleanBare(_pPtr.baseAddress, b)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
void booleanCompound(const int * __counted_by(b + 1) __noescape p, Boolean b);
// expected-expansion@-1:77{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func booleanCompound(_ p: Span<CInt>, _ b: Bool) {|}}
//   expected-error@3{{no exact matches in call to initializer}}
//   expected-remark@3{{macro content: |    if p.count != (CInt(b) + CInt(1)) {|}}
//   expected-remark@4{{macro content: |      @inline(never) func _boundsCheckFailure<E: BinaryInteger, A: BinaryInteger>(_ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@5{{macro content: |        @inline(never) func _fail(_ function: StaticString, _ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@6{{macro content: |          fatalError("bounds check failure in \\(function): expected \\(expected) but got \\(actual)")|}}
//   expected-remark@7{{macro content: |        }|}}
//   expected-remark@8{{macro content: |        _fail("booleanCompound", expected, actual)|}}
//   expected-remark@9{{macro content: |      }|}}
//   expected-error@10{{no exact matches in call to initializer}}
//   expected-remark@10{{macro content: |      _boundsCheckFailure((CInt(b) + CInt(1)), p.count)|}}
//   expected-remark@11{{macro content: |    }|}}
//   expected-remark@12{{macro content: |    let _pPtr = p.withUnsafeBufferPointer {|}}
//   expected-remark@13{{macro content: |        unsafe $0|}}
//   expected-remark@14{{macro content: |    }|}}
//   expected-remark@15{{macro content: |    defer {|}}
//   expected-remark@16{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@17{{macro content: |    }|}}
//   expected-remark@18{{macro content: |    return unsafe booleanCompound(_pPtr.baseAddress, b)|}}
//   expected-remark@19{{macro content: |}|}}
// }}

//--- module.modulemap
module Test {
  header "test.h"
}

//--- test.swift
import Darwin
import Test

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
func callBooleanBare(_ p: Span<CInt>) {
  booleanBare(p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
func callBooleanCompound(_ p: Span<CInt>, _ b: Bool) {
  booleanCompound(p, b)
}
