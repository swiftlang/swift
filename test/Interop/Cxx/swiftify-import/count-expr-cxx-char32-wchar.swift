// `wchar_t` is a 16-bit unsigned integer on Windows, where `CWideChar` is
// `UInt16`, so the `wchar_t` wrappers would compile there.
// UNSUPPORTED: OS=windows-msvc

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -I %t -cxx-interoperability-mode=default \
// RUN:   %t%{fs-sep}test.swift -verify -verify-additional-file %t%{fs-sep}test.h -Rmacro-expansions -suppress-notes -eager-macro-checking

// FIXME: None of these wrappers compile, because their counts use a C++
// `char32_t` or `wchar_t` parameter as if it were an integer. This test emits a
// module, so it type-checks the wrappers' bodies and expects the errors. Both
// are builtin types in C++, which Swift imports as `CChar32` and `CWideChar`,
// both `Unicode.Scalar`. The wrappers convert the value with `CInt(c)` for
// `c + 1`, and compute it with e.g. `CChar32(exactly: p.count)!` for a count
// that is just `c`. There's no `CInt` initializer that takes a
// `Unicode.Scalar`, and no `Unicode.Scalar(exactly:)` that takes an `Int`
// (https://github.com/swiftlang/swift/issues/94421).

//--- test.h
#pragma once

#include <ptrcheck.h>

void char32Bare(const int * __counted_by(c) p, char32_t c);
// expected-expansion@-1:58{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @_disfavoredOverload public func char32Bare(_ p: UnsafeBufferPointer<CInt>) {|}}
//   expected-error@3{{extraneous argument label 'exactly:' in call}}
//   expected-remark@3{{macro content: |    let c = CChar32(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe char32Bare(p.baseAddress, c)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
void wcharBare(const int * __counted_by(c) p, wchar_t c);
// expected-expansion@-1:56{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @_disfavoredOverload public func wcharBare(_ p: UnsafeBufferPointer<CInt>) {|}}
//   expected-error@3{{extraneous argument label 'exactly:' in call}}
//   expected-remark@3{{macro content: |    let c = CWideChar(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe wcharBare(p.baseAddress, c)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
void wcharCompound(const int * __counted_by(c + 1) p, wchar_t c);
// expected-expansion@-1:64{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @_disfavoredOverload public func wcharCompound(_ p: UnsafeBufferPointer<CInt>, _ c: CWideChar) {|}}
//   expected-error@3{{no exact matches in call to initializer}}
//   expected-remark@3{{macro content: |    if p.count != (CInt(c) + CInt(1)) {|}}
//   expected-remark@4{{macro content: |      @inline(never) func _boundsCheckFailure<E: BinaryInteger, A: BinaryInteger>(_ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@5{{macro content: |        @inline(never) func _fail(_ function: StaticString, _ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@6{{macro content: |          fatalError("bounds check failure in \\(function): expected \\(expected) but got \\(actual)")|}}
//   expected-remark@7{{macro content: |        }|}}
//   expected-remark@8{{macro content: |        _fail("wcharCompound", expected, actual)|}}
//   expected-remark@9{{macro content: |      }|}}
//   expected-error@10{{no exact matches in call to initializer}}
//   expected-remark@10{{macro content: |      _boundsCheckFailure((CInt(c) + CInt(1)), p.count)|}}
//   expected-remark@11{{macro content: |    }|}}
//   expected-remark@12{{macro content: |    return unsafe wcharCompound(p.baseAddress, c)|}}
//   expected-remark@13{{macro content: |}|}}
// }}

//--- module.modulemap
module Test {
  header "test.h"
  requires cplusplus
}

//--- test.swift
import Test

func callChar32Bare(_ p: UnsafeBufferPointer<CInt>) {
  unsafe char32Bare(p)
}

func callWCharBare(_ p: UnsafeBufferPointer<CInt>) {
  unsafe wcharBare(p)
}

func callWCharCompound(_ p: UnsafeBufferPointer<CInt>, _ c: CWideChar) {
  unsafe wcharCompound(p, c)
}
