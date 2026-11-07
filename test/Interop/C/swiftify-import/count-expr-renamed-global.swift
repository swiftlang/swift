// The expected expansions spell out the canonical type of `size_t`, which is
// `unsigned long long` on Windows and `unsigned int` on 32-bit targets.
// UNSUPPORTED: OS=windows-msvc
// UNSUPPORTED: PTRSIZE=32

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck -plugin-path %swift-plugin-dir -I %t -strict-memory-safety \
// RUN:   %t/test.swift -verify -verify-additional-file %t%{fs-sep}test.h -Rmacro-expansions -suppress-notes -eager-macro-checking

// Check a count expression that refers to a global that `swift_name` imports as
// a member of a type.
// FIXME: The wrapper refers to the global by its C name, which
// doesn't name it in Swift, so the wrapper doesn't compile. This test uses
// `-typecheck`, which doesn't type-check the wrapper's body, so that error isn't
// reported here (https://github.com/swiftlang/swift/issues/94220).

//--- test.h
#pragma once

#include <ptrcheck.h>
#include <stddef.h>

#define __noescape __attribute__((noescape))

struct S {
  int x;
};

extern const size_t kCount __attribute__((swift_name("S.count")));

// Imported as the method `S.compound(_:_:)`, and also as an obsoleted `SCompound`
// function that points to it, so there are two wrappers.
// FIXME: The wrapper refers to `kCount` by its C name, but in Swift it needs to
// be written `S.count`. (https://github.com/swiftlang/swift/issues/94220)
// expected-expansion@+44:15{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@available(swift, obsoleted: 3, renamed: "S.compound(self:_:_:)") @_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload|}}
//   expected-remark@3{{macro content: |public func SCompound(_ s: UnsafeMutablePointer<S>!, _ p: Span<CInt>, _ n: Int) {|}}
//   expected-remark@4{{macro content: |    if p.count != (CUnsignedLong(kCount) + CUnsignedLong(n)) {|}}
//   expected-remark@5{{macro content: |      @inline(never) func _boundsCheckFailure<E: BinaryInteger, A: BinaryInteger>(_ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@6{{macro content: |        @inline(never) func _fail(_ function: StaticString, _ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@7{{macro content: |          fatalError("bounds check failure in \\(function): expected \\(expected) but got \\(actual)")|}}
//   expected-remark@8{{macro content: |        }|}}
//   expected-remark@9{{macro content: |        _fail("SCompound", expected, actual)|}}
//   expected-remark@10{{macro content: |      }|}}
//   expected-remark@11{{macro content: |      _boundsCheckFailure((CUnsignedLong(kCount) + CUnsignedLong(n)), p.count)|}}
//   expected-remark@12{{macro content: |    }|}}
//   expected-remark@13{{macro content: |    let _pPtr = p.withUnsafeBufferPointer {|}}
//   expected-remark@14{{macro content: |        unsafe $0|}}
//   expected-remark@15{{macro content: |    }|}}
//   expected-remark@16{{macro content: |    defer {|}}
//   expected-remark@17{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@18{{macro content: |    }|}}
//   expected-remark@19{{macro content: |    return unsafe SCompound(s, _pPtr.baseAddress, n)|}}
//   expected-remark@20{{macro content: |}|}}
// }}
// expected-expansion@+22:88{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload|}}
//   expected-remark@3{{macro content: |public mutating func compound(_ p: Span<CInt>, _ n: Int) {|}}
//   expected-remark@4{{macro content: |    if p.count != (CUnsignedLong(kCount) + CUnsignedLong(n)) {|}}
//   expected-remark@5{{macro content: |      @inline(never) func _boundsCheckFailure<E: BinaryInteger, A: BinaryInteger>(_ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@6{{macro content: |        @inline(never) func _fail(_ function: StaticString, _ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@7{{macro content: |          fatalError("bounds check failure in \\(function): expected \\(expected) but got \\(actual)")|}}
//   expected-remark@8{{macro content: |        }|}}
//   expected-remark@9{{macro content: |        _fail("compound", expected, actual)|}}
//   expected-remark@10{{macro content: |      }|}}
//   expected-remark@11{{macro content: |      _boundsCheckFailure((CUnsignedLong(kCount) + CUnsignedLong(n)), p.count)|}}
//   expected-remark@12{{macro content: |    }|}}
//   expected-remark@13{{macro content: |    let _pPtr = p.withUnsafeBufferPointer {|}}
//   expected-remark@14{{macro content: |        unsafe $0|}}
//   expected-remark@15{{macro content: |    }|}}
//   expected-remark@16{{macro content: |    defer {|}}
//   expected-remark@17{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@18{{macro content: |    }|}}
//   expected-remark@19{{macro content: |    return unsafe compound(_pPtr.baseAddress, n)|}}
//   expected-remark@20{{macro content: |}|}}
// }}
void SCompound(struct S *s, const int * __counted_by(kCount + n) __noescape p, size_t n)
    __attribute__((swift_name("S.compound(self:_:_:)")));

//--- module.modulemap
module Test {
  header "test.h"
}

//--- test.swift
import Test

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
func callCompound(_ s: inout S, _ p: Span<CInt>, _ n: Int) {
  s.compound(p, n)
}
