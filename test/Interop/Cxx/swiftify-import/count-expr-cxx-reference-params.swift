// The expected expansions spell out the canonical type of `size_t`, which is
// `unsigned long long` on Windows and `unsigned int` on 32-bit targets.
// UNSUPPORTED: OS=windows-msvc
// UNSUPPORTED: PTRSIZE=32

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -I %t -cxx-interoperability-mode=default \
// RUN:   %t/test.swift -verify -verify-additional-file %t%{fs-sep}test.h -Rmacro-expansions -verify-ignore-macro-note -eager-macro-checking

// Check count expressions that use C++ reference parameters. A reference
// parameter is imported as the type it refers to, so `const size_t &` is
// imported as `Int`, and is converted like a `size_t` parameter. This test
// emits a module, so the wrappers' bodies are type-checked too. Rvalue
// references are checked in count-expr-cxx-rvalue-reference-params.swift.

//--- test.h
#pragma once

#include <ptrcheck.h>
#include <stddef.h>

// expected-expansion@+15:73{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @_disfavoredOverload public func constRefCompound(_ p: UnsafeBufferPointer<CInt>, _ n: Int) {|}}
//   expected-remark@3{{macro content: |    if p.count != (CUnsignedLong(n) + CUnsignedLong(CInt(1))) {|}}
//   expected-remark@4{{macro content: |      @inline(never) func _boundsCheckFailure<E: BinaryInteger, A: BinaryInteger>(_ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@5{{macro content: |        @inline(never) func _fail(_ function: StaticString, _ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@6{{macro content: |          fatalError("bounds check failure in \\(function): expected \\(expected) but got \\(actual)")|}}
//   expected-remark@7{{macro content: |        }|}}
//   expected-remark@8{{macro content: |        _fail("constRefCompound", expected, actual)|}}
//   expected-remark@9{{macro content: |      }|}}
//   expected-remark@10{{macro content: |      _boundsCheckFailure((CUnsignedLong(n) + CUnsignedLong(CInt(1))), p.count)|}}
//   expected-remark@11{{macro content: |    }|}}
//   expected-remark@12{{macro content: |    return unsafe constRefCompound(p.baseAddress, n)|}}
//   expected-remark@13{{macro content: |}|}}
// }}
void constRefCompound(const int * __counted_by(n + 1) p, const size_t &n);

// A mutable reference is imported as an `inout` parameter.
// expected-expansion@+15:62{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @_disfavoredOverload public func refCompound(_ p: UnsafeBufferPointer<CInt>, _ n: inout Int) {|}}
//   expected-remark@3{{macro content: |    if p.count != (CUnsignedLong(n) + CUnsignedLong(CInt(1))) {|}}
//   expected-remark@4{{macro content: |      @inline(never) func _boundsCheckFailure<E: BinaryInteger, A: BinaryInteger>(_ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@5{{macro content: |        @inline(never) func _fail(_ function: StaticString, _ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@6{{macro content: |          fatalError("bounds check failure in \\(function): expected \\(expected) but got \\(actual)")|}}
//   expected-remark@7{{macro content: |        }|}}
//   expected-remark@8{{macro content: |        _fail("refCompound", expected, actual)|}}
//   expected-remark@9{{macro content: |      }|}}
//   expected-remark@10{{macro content: |      _boundsCheckFailure((CUnsignedLong(n) + CUnsignedLong(CInt(1))), p.count)|}}
//   expected-remark@11{{macro content: |    }|}}
//   expected-remark@12{{macro content: |    return unsafe refCompound(p.baseAddress, &n)|}}
//   expected-remark@13{{macro content: |}|}}
// }}
void refCompound(const int * __counted_by(n + 1) p, size_t &n);

// A count that is just the name of the parameter isn't converted, so `n` is
// removed from the wrapper, as for a `size_t` parameter.
// expected-expansion@+7:65{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @_disfavoredOverload public func constRefBare(_ p: UnsafeBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    let n = p.count|}}
//   expected-remark@4{{macro content: |    return unsafe constRefBare(p.baseAddress, n)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
void constRefBare(const int * __counted_by(n) p, const size_t &n);

//--- module.modulemap
module Test {
  header "test.h"
  requires cplusplus
}

//--- test.swift
import Test

func callConstRefCompound(_ p: UnsafeBufferPointer<CInt>, _ n: Int) {
  unsafe constRefCompound(p, n)
}

func callRefCompound(_ p: UnsafeBufferPointer<CInt>, _ n: inout Int) {
  unsafe refCompound(p, &n)
}

func callConstRefBare(_ p: UnsafeBufferPointer<CInt>) {
  unsafe constRefBare(p)
}
