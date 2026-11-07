// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -I %t -strict-memory-safety \
// RUN:   %t%{fs-sep}test.swift -verify -verify-additional-file %t%{fs-sep}test.h -Rmacro-expansions -suppress-notes -eager-macro-checking

// Check count expressions that use an enum constant. Clang replaces the
// constant with its value, so the count uses a literal, which is converted like
// any other literal. This test emits a module, so the wrappers' bodies are
// type-checked too.

//--- test.h
#pragma once

#include <lifetimebound.h>
#include <ptrcheck.h>

enum { kAnon = 4 };
enum E { E0, E1 };

void anonEnumConstant(const int * __counted_by(n + kAnon) __noescape p, int n);
// expected-expansion@-1:78{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func anonEnumConstant(_ p: Span<CInt>, _ n: CInt) {|}}
//   expected-remark@3{{macro content: |    if p.count != (n + CInt(4)) {|}}
//   expected-remark@4{{macro content: |      @inline(never) func _boundsCheckFailure<E: BinaryInteger, A: BinaryInteger>(_ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@5{{macro content: |        @inline(never) func _fail(_ function: StaticString, _ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@6{{macro content: |          fatalError("bounds check failure in \\(function): expected \\(expected) but got \\(actual)")|}}
//   expected-remark@7{{macro content: |        }|}}
//   expected-remark@8{{macro content: |        _fail("anonEnumConstant", expected, actual)|}}
//   expected-remark@9{{macro content: |      }|}}
//   expected-remark@10{{macro content: |      _boundsCheckFailure((n + CInt(4)), p.count)|}}
//   expected-remark@11{{macro content: |    }|}}
//   expected-remark@12{{macro content: |    let _pPtr = p.withUnsafeBufferPointer {|}}
//   expected-remark@13{{macro content: |        unsafe $0|}}
//   expected-remark@14{{macro content: |    }|}}
//   expected-remark@15{{macro content: |    defer {|}}
//   expected-remark@16{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@17{{macro content: |    }|}}
//   expected-remark@18{{macro content: |    return unsafe anonEnumConstant(_pPtr.baseAddress, n)|}}
//   expected-remark@19{{macro content: |}|}}
// }}
void namedEnumConstant(const int * __counted_by(n + E1) __noescape p, int n);
// expected-expansion@-1:76{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func namedEnumConstant(_ p: Span<CInt>, _ n: CInt) {|}}
//   expected-remark@3{{macro content: |    if p.count != (n + CInt(1)) {|}}
//   expected-remark@4{{macro content: |      @inline(never) func _boundsCheckFailure<E: BinaryInteger, A: BinaryInteger>(_ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@5{{macro content: |        @inline(never) func _fail(_ function: StaticString, _ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@6{{macro content: |          fatalError("bounds check failure in \\(function): expected \\(expected) but got \\(actual)")|}}
//   expected-remark@7{{macro content: |        }|}}
//   expected-remark@8{{macro content: |        _fail("namedEnumConstant", expected, actual)|}}
//   expected-remark@9{{macro content: |      }|}}
//   expected-remark@10{{macro content: |      _boundsCheckFailure((n + CInt(1)), p.count)|}}
//   expected-remark@11{{macro content: |    }|}}
//   expected-remark@12{{macro content: |    let _pPtr = p.withUnsafeBufferPointer {|}}
//   expected-remark@13{{macro content: |        unsafe $0|}}
//   expected-remark@14{{macro content: |    }|}}
//   expected-remark@15{{macro content: |    defer {|}}
//   expected-remark@16{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@17{{macro content: |    }|}}
//   expected-remark@18{{macro content: |    return unsafe namedEnumConstant(_pPtr.baseAddress, n)|}}
//   expected-remark@19{{macro content: |}|}}
// }}

//--- module.modulemap
module Test {
  header "test.h"
}

//--- test.swift
import Test

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
func callAnonEnumConstant(_ p: Span<CInt>, _ n: CInt) {
  anonEnumConstant(p, n)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
func callNamedEnumConstant(_ p: Span<CInt>, _ n: CInt) {
  namedEnumConstant(p, n)
}
