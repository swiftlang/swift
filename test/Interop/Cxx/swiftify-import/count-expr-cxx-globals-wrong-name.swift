// The expected expansions spell out the canonical type of `size_t`, which is
// `unsigned long long` on Windows and `unsigned int` on 32-bit targets.
// UNSUPPORTED: OS=windows-msvc
// UNSUPPORTED: PTRSIZE=32

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck -plugin-path %swift-plugin-dir -I %t -cxx-interoperability-mode=default \
// RUN:   %t/test.swift -verify -verify-additional-file %t%{fs-sep}test.h -Rmacro-expansions -verify-ignore-macro-note -eager-macro-checking

// Check count expressions that refer to C++ globals by names that the wrappers
// can't use.
//
// FIXME: The wrappers refer to the globals by their unqualified C++ names,
// which don't name them in Swift from where the wrappers are, so the wrappers
// don't compile. This test uses `-typecheck`, which doesn't type-check the
// wrappers' bodies, so those errors aren't reported here
// (https://github.com/swiftlang/swift/issues/94220).
//
// `S::f` refers to a static data member of another class that hasn't been
// imported yet. Getting the member's Swift type must not import the member:
// that first imports its class, which imports the member too, so the member
// would be imported twice.

//--- test.h
#pragma once

#include <ptrcheck.h>
#include <stddef.h>

struct T {
  static const size_t kCount;
};

struct S {
  // FIXME: The wrapper refers to `T::kCount` as just `kCount`, but in Swift it
  // needs to be written `T.kCount`.
  // (https://github.com/swiftlang/swift/issues/94220)
  // expected-expansion@+16:61{{
  //   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
  //   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @_disfavoredOverload|}}
  //   expected-remark@3{{macro content: |public mutating func f(_ p: UnsafeBufferPointer<CInt>, _ n: Int) {|}}
  //   expected-remark@4{{macro content: |    if p.count != (CUnsignedLong(kCount) + CUnsignedLong(n)) {|}}
  //   expected-remark@5{{macro content: |      @inline(never) func _boundsCheckFailure<E: BinaryInteger, A: BinaryInteger>(_ expected: E, _ actual: A) -> Never {|}}
  //   expected-remark@6{{macro content: |        @inline(never) func _fail(_ function: StaticString, _ expected: E, _ actual: A) -> Never {|}}
  //   expected-remark@7{{macro content: |          fatalError("bounds check failure in \\(function): expected \\(expected) but got \\(actual)")|}}
  //   expected-remark@8{{macro content: |        }|}}
  //   expected-remark@9{{macro content: |        _fail("f", expected, actual)|}}
  //   expected-remark@10{{macro content: |      }|}}
  //   expected-remark@11{{macro content: |      _boundsCheckFailure((CUnsignedLong(kCount) + CUnsignedLong(n)), p.count)|}}
  //   expected-remark@12{{macro content: |    }|}}
  //   expected-remark@13{{macro content: |    return unsafe f(p.baseAddress, n)|}}
  //   expected-remark@14{{macro content: |}|}}
  // }}
  void f(const int * __counted_by(T::kCount + n) p, size_t n);
};

struct U {
  static const size_t kCount;
  // FIXME: The wrapper refers to `U::kCount` as just `kCount`, but in an
  // instance method a static member needs to be written `Self.kCount` or
  // `U.kCount`. (https://github.com/swiftlang/swift/issues/94220)
  // expected-expansion@+16:58{{
  //   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
  //   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @_disfavoredOverload|}}
  //   expected-remark@3{{macro content: |public mutating func f(_ p: UnsafeBufferPointer<CInt>, _ n: Int) {|}}
  //   expected-remark@4{{macro content: |    if p.count != (CUnsignedLong(kCount) + CUnsignedLong(n)) {|}}
  //   expected-remark@5{{macro content: |      @inline(never) func _boundsCheckFailure<E: BinaryInteger, A: BinaryInteger>(_ expected: E, _ actual: A) -> Never {|}}
  //   expected-remark@6{{macro content: |        @inline(never) func _fail(_ function: StaticString, _ expected: E, _ actual: A) -> Never {|}}
  //   expected-remark@7{{macro content: |          fatalError("bounds check failure in \\(function): expected \\(expected) but got \\(actual)")|}}
  //   expected-remark@8{{macro content: |        }|}}
  //   expected-remark@9{{macro content: |        _fail("f", expected, actual)|}}
  //   expected-remark@10{{macro content: |      }|}}
  //   expected-remark@11{{macro content: |      _boundsCheckFailure((CUnsignedLong(kCount) + CUnsignedLong(n)), p.count)|}}
  //   expected-remark@12{{macro content: |    }|}}
  //   expected-remark@13{{macro content: |    return unsafe f(p.baseAddress, n)|}}
  //   expected-remark@14{{macro content: |}|}}
  // }}
  void f(const int * __counted_by(kCount + n) p, size_t n);
};

namespace ns {
extern const size_t kCount;
} // namespace ns

// FIXME: The wrapper refers to `ns::kCount` as just `kCount`, but outside the
// namespace it needs to be written `ns.kCount`.
// (https://github.com/swiftlang/swift/issues/94220)
// expected-expansion@+15:66{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @_disfavoredOverload public func outside(_ p: UnsafeBufferPointer<CInt>, _ n: Int) {|}}
//   expected-remark@3{{macro content: |    if p.count != (CUnsignedLong(kCount) + CUnsignedLong(n)) {|}}
//   expected-remark@4{{macro content: |      @inline(never) func _boundsCheckFailure<E: BinaryInteger, A: BinaryInteger>(_ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@5{{macro content: |        @inline(never) func _fail(_ function: StaticString, _ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@6{{macro content: |          fatalError("bounds check failure in \\(function): expected \\(expected) but got \\(actual)")|}}
//   expected-remark@7{{macro content: |        }|}}
//   expected-remark@8{{macro content: |        _fail("outside", expected, actual)|}}
//   expected-remark@9{{macro content: |      }|}}
//   expected-remark@10{{macro content: |      _boundsCheckFailure((CUnsignedLong(kCount) + CUnsignedLong(n)), p.count)|}}
//   expected-remark@11{{macro content: |    }|}}
//   expected-remark@12{{macro content: |    return unsafe outside(p.baseAddress, n)|}}
//   expected-remark@13{{macro content: |}|}}
// }}
void outside(const int * __counted_by(ns::kCount + n) p, size_t n);

//--- module.modulemap
module Test {
  header "test.h"
  requires cplusplus
}

//--- test.swift
import Test

// Only `S` is used here, so `T` hasn't been imported yet when `S::f` is.
func callF(_ s: inout S, _ p: UnsafeBufferPointer<CInt>, _ n: Int) {
  unsafe s.f(p, n)
}

func callSameClass(_ u: inout U, _ p: UnsafeBufferPointer<CInt>, _ n: Int) {
  unsafe u.f(p, n)
}

// Nothing else uses `ns`, so `outside` is the first thing that needs
// `ns::kCount`.
func callOutside(_ p: UnsafeBufferPointer<CInt>, _ n: Int) {
  unsafe outside(p, n)
}
