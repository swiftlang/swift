// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -I %t -cxx-interoperability-mode=default \
// RUN:   %t%{fs-sep}test.swift -verify -verify-additional-file %t%{fs-sep}test.h -Rmacro-expansions -suppress-notes -eager-macro-checking

// FIXME: None of these wrappers compile, because their counts use a C++ enum
// parameter as if it were an integer. This test emits a module, so it
// type-checks the wrappers' bodies and expects the errors. An unscoped enum is
// imported as a struct with a `rawValue`, and an `enum class` as a Swift
// `enum`, but the wrappers convert the value with `CInt(e)` for a count like
// `e + 1`, or with `CInt(truncatingIfNeeded: e)` for an explicit cast like
// `(int)e`, and compute it with `ScopedEnum(exactly: p.count)!` for a count
// that is just `e`. None of these initializers exist for an enum
// (https://github.com/swiftlang/swift/issues/94246).

//--- test.h
#pragma once

#include <ptrcheck.h>

enum UnscopedEnum { Unscoped0, Unscoped1 };
enum class ScopedEnum : int { A, B };

void unscopedEnumCompound(const int * __counted_by(e + 1) p, UnscopedEnum e);
// expected-expansion@-1:76{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @_disfavoredOverload public func unscopedEnumCompound(_ p: UnsafeBufferPointer<CInt>, _ e: UnscopedEnum) {|}}
//   expected-error@3{{initializer 'init(_:)' requires that 'UnscopedEnum' conform to}}
//   expected-remark@3{{macro content: |    if p.count != (CInt(e) + CInt(1)) {|}}
//   expected-remark@4{{macro content: |      @inline(never) func _boundsCheckFailure<E: BinaryInteger, A: BinaryInteger>(_ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@5{{macro content: |        @inline(never) func _fail(_ function: StaticString, _ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@6{{macro content: |          fatalError("bounds check failure in \\(function): expected \\(expected) but got \\(actual)")|}}
//   expected-remark@7{{macro content: |        }|}}
//   expected-remark@8{{macro content: |        _fail("unscopedEnumCompound", expected, actual)|}}
//   expected-remark@9{{macro content: |      }|}}
//   expected-error@10{{initializer 'init(_:)' requires that 'UnscopedEnum' conform to}}
//   expected-remark@10{{macro content: |      _boundsCheckFailure((CInt(e) + CInt(1)), p.count)|}}
//   expected-remark@11{{macro content: |    }|}}
//   expected-remark@12{{macro content: |    return unsafe unscopedEnumCompound(p.baseAddress, e)|}}
//   expected-remark@13{{macro content: |}|}}
// }}
void scopedEnumBare(const int * __counted_by(e) p, ScopedEnum e);
// expected-expansion@-1:64{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @_disfavoredOverload public func scopedEnumBare(_ p: UnsafeBufferPointer<CInt>) {|}}
//   expected-error@3{{cannot convert value of type 'Int' to expected argument type}}
//   expected-error@3{{incorrect argument label in call (have 'exactly:', expected 'rawValue:')}}
//   expected-remark@3{{macro content: |    let e = ScopedEnum(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe scopedEnumBare(p.baseAddress, e)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
void scopedEnumCast(const int * __counted_by((int)e + 1) p, ScopedEnum e);
// expected-expansion@-1:73{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @_disfavoredOverload public func scopedEnumCast(_ p: UnsafeBufferPointer<CInt>, _ e: ScopedEnum) {|}}
//   expected-error@3{{initializer 'init(truncatingIfNeeded:)' requires that 'ScopedEnum' conform to 'BinaryInteger'}}
//   expected-remark@3{{macro content: |    if p.count != (CInt(truncatingIfNeeded: e) + CInt(1)) {|}}
//   expected-remark@4{{macro content: |      @inline(never) func _boundsCheckFailure<E: BinaryInteger, A: BinaryInteger>(_ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@5{{macro content: |        @inline(never) func _fail(_ function: StaticString, _ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@6{{macro content: |          fatalError("bounds check failure in \\(function): expected \\(expected) but got \\(actual)")|}}
//   expected-remark@7{{macro content: |        }|}}
//   expected-remark@8{{macro content: |        _fail("scopedEnumCast", expected, actual)|}}
//   expected-remark@9{{macro content: |      }|}}
//   expected-error@10{{initializer 'init(truncatingIfNeeded:)' requires that 'ScopedEnum' conform to 'BinaryInteger'}}
//   expected-remark@10{{macro content: |      _boundsCheckFailure((CInt(truncatingIfNeeded: e) + CInt(1)), p.count)|}}
//   expected-remark@11{{macro content: |    }|}}
//   expected-remark@12{{macro content: |    return unsafe scopedEnumCast(p.baseAddress, e)|}}
//   expected-remark@13{{macro content: |}|}}
// }}

//--- module.modulemap
module Test {
  header "test.h"
  requires cplusplus
}

//--- test.swift
import Test

func callUnscopedEnumCompound(_ p: UnsafeBufferPointer<CInt>, _ e: UnscopedEnum) {
  unsafe unscopedEnumCompound(p, e)
}

func callScopedEnumBare(_ p: UnsafeBufferPointer<CInt>) {
  unsafe scopedEnumBare(p)
}

func callScopedEnumCast(_ p: UnsafeBufferPointer<CInt>, _ e: ScopedEnum) {
  unsafe scopedEnumCast(p, e)
}
