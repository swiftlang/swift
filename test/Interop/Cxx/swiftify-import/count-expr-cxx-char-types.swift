// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -I %t -cxx-interoperability-mode=default \
// RUN:   %t%{fs-sep}test.swift -verify -verify-additional-file %t%{fs-sep}test.h -Rmacro-expansions -suppress-notes -eager-macro-checking

// Check count expressions that use a C++ `char16_t` or `char32_t` parameter.
// `char16_t` is imported as `CChar16`, which is `UInt16`, so it's converted
// like any other integer. `char32_t` is imported as `CChar32`, which is
// `Unicode.Scalar`. A compound `char32_t` count still works, because it's
// converted with `CUnsignedInt(c)`, and `UInt32` has an initializer that takes
// a `Unicode.Scalar`. A bare `char32_t` count doesn't work yet
// (https://github.com/swiftlang/swift/issues/94421). This test emits a module,
// so the wrappers' bodies are type-checked too.

//--- test.h
#pragma once

#include <ptrcheck.h>

void char16Bare(const int * __counted_by(c) p, char16_t c);
// expected-expansion@-1:58{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @_disfavoredOverload public func char16Bare(_ p: UnsafeBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    let c = CChar16(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe char16Bare(p.baseAddress, c)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
void char16Compound(const int * __counted_by(c + 1) p, char16_t c);
// expected-expansion@-1:66{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @_disfavoredOverload public func char16Compound(_ p: UnsafeBufferPointer<CInt>, _ c: CChar16) {|}}
//   expected-remark@3{{macro content: |    if p.count != (CInt(c) + CInt(1)) {|}}
//   expected-remark@4{{macro content: |      @inline(never) func _boundsCheckFailure<E: BinaryInteger, A: BinaryInteger>(_ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@5{{macro content: |        @inline(never) func _fail(_ function: StaticString, _ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@6{{macro content: |          fatalError("bounds check failure in \\(function): expected \\(expected) but got \\(actual)")|}}
//   expected-remark@7{{macro content: |        }|}}
//   expected-remark@8{{macro content: |        _fail("char16Compound", expected, actual)|}}
//   expected-remark@9{{macro content: |      }|}}
//   expected-remark@10{{macro content: |      _boundsCheckFailure((CInt(c) + CInt(1)), p.count)|}}
//   expected-remark@11{{macro content: |    }|}}
//   expected-remark@12{{macro content: |    return unsafe char16Compound(p.baseAddress, c)|}}
//   expected-remark@13{{macro content: |}|}}
// }}
void char32Compound(const int * __counted_by(c + 1) p, char32_t c);
// expected-expansion@-1:66{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @_disfavoredOverload public func char32Compound(_ p: UnsafeBufferPointer<CInt>, _ c: CChar32) {|}}
//   expected-remark@3{{macro content: |    if p.count != (CUnsignedInt(c) + CUnsignedInt(CInt(1))) {|}}
//   expected-remark@4{{macro content: |      @inline(never) func _boundsCheckFailure<E: BinaryInteger, A: BinaryInteger>(_ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@5{{macro content: |        @inline(never) func _fail(_ function: StaticString, _ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@6{{macro content: |          fatalError("bounds check failure in \\(function): expected \\(expected) but got \\(actual)")|}}
//   expected-remark@7{{macro content: |        }|}}
//   expected-remark@8{{macro content: |        _fail("char32Compound", expected, actual)|}}
//   expected-remark@9{{macro content: |      }|}}
//   expected-remark@10{{macro content: |      _boundsCheckFailure((CUnsignedInt(c) + CUnsignedInt(CInt(1))), p.count)|}}
//   expected-remark@11{{macro content: |    }|}}
//   expected-remark@12{{macro content: |    return unsafe char32Compound(p.baseAddress, c)|}}
//   expected-remark@13{{macro content: |}|}}
// }}

//--- module.modulemap
module Test {
  header "test.h"
  requires cplusplus
}

//--- test.swift
import Test

func callChar16Bare(_ p: UnsafeBufferPointer<CInt>) {
  unsafe char16Bare(p)
}

func callChar16Compound(_ p: UnsafeBufferPointer<CInt>, _ c: CChar16) {
  unsafe char16Compound(p, c)
}

func callChar32Compound(_ p: UnsafeBufferPointer<CInt>, _ c: CChar32) {
  unsafe char32Compound(p, c)
}
