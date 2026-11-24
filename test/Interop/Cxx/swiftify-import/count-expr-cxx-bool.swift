// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -I %t -cxx-interoperability-mode=default \
// RUN:   %t%{fs-sep}test.swift -verify -verify-additional-file %t%{fs-sep}test.h -Rmacro-expansions -suppress-notes -eager-macro-checking

// FIXME: Neither wrapper compiles, because their counts use a C++ `bool`
// parameter as if it were an integer. This test emits a module, so it
// type-checks the wrappers' bodies and expects the errors. `bool` is imported
// as `Bool`, but the wrappers convert it with `CInt(b)` for `b + 1`, and
// compute it with `CBool(exactly: p.count)!` for a count that is just `b`.
// There's no `CInt` initializer that takes a `Bool`, and no `Bool(exactly:)`
// that takes an `Int` (https://github.com/swiftlang/swift/issues/94420).

//--- test.h
#pragma once

#include <ptrcheck.h>

void boolBare(const int * __counted_by(b) p, bool b);
// expected-expansion@-1:52{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @_disfavoredOverload public func boolBare(_ p: UnsafeBufferPointer<CInt>) {|}}
//   expected-error@3{{extraneous argument label 'exactly:' in call}}
//   expected-remark@3{{macro content: |    let b = CBool(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe boolBare(p.baseAddress, b)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
void boolCompound(const int * __counted_by(b + 1) p, bool b);
// expected-expansion@-1:60{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @_disfavoredOverload public func boolCompound(_ p: UnsafeBufferPointer<CInt>, _ b: CBool) {|}}
//   expected-error@3{{no exact matches in call to initializer}}
//   expected-remark@3{{macro content: |    if p.count != (CInt(b) + CInt(1)) {|}}
//   expected-remark@4{{macro content: |      @inline(never) func _boundsCheckFailure<E: BinaryInteger, A: BinaryInteger>(_ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@5{{macro content: |        @inline(never) func _fail(_ function: StaticString, _ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@6{{macro content: |          fatalError("bounds check failure in \\(function): expected \\(expected) but got \\(actual)")|}}
//   expected-remark@7{{macro content: |        }|}}
//   expected-remark@8{{macro content: |        _fail("boolCompound", expected, actual)|}}
//   expected-remark@9{{macro content: |      }|}}
//   expected-error@10{{no exact matches in call to initializer}}
//   expected-remark@10{{macro content: |      _boundsCheckFailure((CInt(b) + CInt(1)), p.count)|}}
//   expected-remark@11{{macro content: |    }|}}
//   expected-remark@12{{macro content: |    return unsafe boolCompound(p.baseAddress, b)|}}
//   expected-remark@13{{macro content: |}|}}
// }}

//--- module.modulemap
module Test {
  header "test.h"
  requires cplusplus
}

//--- test.swift
import Test

func callBoolBare(_ p: UnsafeBufferPointer<CInt>) {
  unsafe boolBare(p)
}

func callBoolCompound(_ p: UnsafeBufferPointer<CInt>, _ b: Bool) {
  unsafe boolCompound(p, b)
}
