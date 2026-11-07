// The expected expansions spell out the canonical type of `size_t`, which is
// `unsigned long long` on Windows and `unsigned int` on 32-bit targets.
// UNSUPPORTED: OS=windows-msvc
// UNSUPPORTED: PTRSIZE=32

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -I %t -cxx-interoperability-mode=default \
// RUN:   %t/test.swift -verify -verify-additional-file %t%{fs-sep}test.h -Rmacro-expansions -verify-ignore-macro-note -eager-macro-checking

// Check count expressions that use C++ rvalue reference parameters, which are
// imported as `consuming` parameters. This test emits a module, so the
// wrappers' bodies are type-checked too.
//
// FIXME: Swift doesn't implicitly copy a `consuming` parameter, and an
// initializer takes its argument as owned. So a count that passes the parameter
// to an initializer, e.g. `CUnsignedLong(n)`, consumes it, and the call to the
// original function then consumes it again, which is an error
// (https://github.com/swiftlang/swift/issues/94238).

//--- test.h
#pragma once

#include <ptrcheck.h>
#include <stddef.h>

// `n` is converted to `CUnsignedLong`, the Swift type of `size_t`'s canonical
// type, which consumes it.
// expected-expansion@+18:69{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-error@2{{'n' consumed more than once}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @_disfavoredOverload public func convertedCompound(_ p: UnsafeBufferPointer<CInt>, consuming n: consuming Int) {|}}
//   expected-note@3{{consumed here}}
//   expected-remark@3{{macro content: |    if p.count != (CUnsignedLong(n) + CUnsignedLong(CInt(1))) {|}}
//   expected-remark@4{{macro content: |      @inline(never) func _boundsCheckFailure<E: BinaryInteger, A: BinaryInteger>(_ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@5{{macro content: |        @inline(never) func _fail(_ function: StaticString, _ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@6{{macro content: |          fatalError("bounds check failure in \\(function): expected \\(expected) but got \\(actual)")|}}
//   expected-remark@7{{macro content: |        }|}}
//   expected-remark@8{{macro content: |        _fail("convertedCompound", expected, actual)|}}
//   expected-remark@9{{macro content: |      }|}}
//   expected-remark@10{{macro content: |      _boundsCheckFailure((CUnsignedLong(n) + CUnsignedLong(CInt(1))), p.count)|}}
//   expected-remark@11{{macro content: |    }|}}
//   expected-note@12{{consumed again here}}
//   expected-remark@12{{macro content: |    return unsafe convertedCompound(p.baseAddress, consuming: n)|}}
//   expected-remark@13{{macro content: |}|}}
// }}
void convertedCompound(const int * __counted_by(n + 1) p, size_t &&n);

// The cast is emitted as a `CInt` initializer, which consumes `n`.
// expected-expansion@+18:67{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-error@2{{'n' consumed more than once}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @_disfavoredOverload public func castCompound(_ p: UnsafeBufferPointer<CInt>, consuming n: consuming CLong) {|}}
//   expected-note@3{{consumed here}}
//   expected-remark@3{{macro content: |    if p.count != (CInt(n) + CInt(1)) {|}}
//   expected-remark@4{{macro content: |      @inline(never) func _boundsCheckFailure<E: BinaryInteger, A: BinaryInteger>(_ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@5{{macro content: |        @inline(never) func _fail(_ function: StaticString, _ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@6{{macro content: |          fatalError("bounds check failure in \\(function): expected \\(expected) but got \\(actual)")|}}
//   expected-remark@7{{macro content: |        }|}}
//   expected-remark@8{{macro content: |        _fail("castCompound", expected, actual)|}}
//   expected-remark@9{{macro content: |      }|}}
//   expected-remark@10{{macro content: |      _boundsCheckFailure((CInt(n) + CInt(1)), p.count)|}}
//   expected-remark@11{{macro content: |    }|}}
//   expected-note@12{{consumed again here}}
//   expected-remark@12{{macro content: |    return unsafe castCompound(p.baseAddress, consuming: n)|}}
//   expected-remark@13{{macro content: |}|}}
// }}
void castCompound(const int * __counted_by((int)n + 1) p, long &&n);

// `n` isn't converted, because `long` is imported as `CLong`, the Swift type of
// its canonical type. `+` only borrows `n`, so this wrapper compiles.
// expected-expansion@+15:69{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @_disfavoredOverload public func unconvertedCompound(_ p: UnsafeBufferPointer<CInt>, consuming n: consuming CLong) {|}}
//   expected-remark@3{{macro content: |    if p.count != (n + CLong(CInt(1))) {|}}
//   expected-remark@4{{macro content: |      @inline(never) func _boundsCheckFailure<E: BinaryInteger, A: BinaryInteger>(_ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@5{{macro content: |        @inline(never) func _fail(_ function: StaticString, _ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@6{{macro content: |          fatalError("bounds check failure in \\(function): expected \\(expected) but got \\(actual)")|}}
//   expected-remark@7{{macro content: |        }|}}
//   expected-remark@8{{macro content: |        _fail("unconvertedCompound", expected, actual)|}}
//   expected-remark@9{{macro content: |      }|}}
//   expected-remark@10{{macro content: |      _boundsCheckFailure((n + CLong(CInt(1))), p.count)|}}
//   expected-remark@11{{macro content: |    }|}}
//   expected-remark@12{{macro content: |    return unsafe unconvertedCompound(p.baseAddress, consuming: n)|}}
//   expected-remark@13{{macro content: |}|}}
// }}
void unconvertedCompound(const int * __counted_by(n + 1) p, long &&n);

//--- module.modulemap
module Test {
  header "test.h"
  requires cplusplus
}

//--- test.swift
import Test

func callConvertedCompound(_ p: UnsafeBufferPointer<CInt>, _ n: Int) {
  unsafe convertedCompound(p, consuming: n)
}

func callCastCompound(_ p: UnsafeBufferPointer<CInt>, _ n: CLong) {
  unsafe castCompound(p, consuming: n)
}

func callUnconvertedCompound(_ p: UnsafeBufferPointer<CInt>, _ n: CLong) {
  unsafe unconvertedCompound(p, consuming: n)
}
