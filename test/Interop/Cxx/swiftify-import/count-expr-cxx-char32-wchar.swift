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
// both `Unicode.Scalar`. For a count that is just `c`, the wrappers compute it
// from the buffer with `CChar32(exactly: p.count)!` or
// `CWideChar(exactly: p.count)!`, and `Unicode.Scalar` has no
// `init(exactly:)`. A compound `wchar_t` count like `c + 1` also fails where
// `wchar_t` is signed, e.g. on Apple platforms and x86_64 Linux, because it's
// converted with `CInt(c)`. That isn't checked here due to `wchar_t` being
// unsigned on some platforms (e.g. on ARM Linux and Android), it's converted with
// `CUnsignedInt(c)`, which compiles
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
