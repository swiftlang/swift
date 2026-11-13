// The expected expansions spell out the canonical type of `size_t`, which is
// `unsigned long long` on Windows and `unsigned int` on 32-bit targets.
// UNSUPPORTED: OS=windows-msvc
// UNSUPPORTED: PTRSIZE=32

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -I %t -cxx-interoperability-mode=default \
// RUN:   %t/test.swift -verify -verify-additional-file %t%{fs-sep}test.h -Rmacro-expansions -verify-ignore-macro-note -eager-macro-checking

// Check count expressions that refer to C++ globals by a name that the wrapper
// can also use: a static data member used in a static method of the same class,
// and a global used in a function in the same namespace. This test emits a
// module, so the wrappers' bodies are type-checked too.

//--- test.h
#pragma once

#include <ptrcheck.h>
#include <stddef.h>

struct S {
  static const size_t kCount;
  // expected-expansion@+16:65{{
  //   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
  //   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @_disfavoredOverload|}}
  //   expected-remark@3{{macro content: |public static func f(_ p: UnsafeBufferPointer<CInt>, _ n: Int) {|}}
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
  static void f(const int * __counted_by(kCount + n) p, size_t n);
};

namespace ns {
extern const size_t kCount;
// expected-expansion@+16:56{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @_disfavoredOverload|}}
//   expected-remark@3{{macro content: |public static func g(_ p: UnsafeBufferPointer<CInt>, _ n: Int) {|}}
//   expected-remark@4{{macro content: |    if p.count != (CUnsignedLong(kCount) + CUnsignedLong(n)) {|}}
//   expected-remark@5{{macro content: |      @inline(never) func _boundsCheckFailure<E: BinaryInteger, A: BinaryInteger>(_ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@6{{macro content: |        @inline(never) func _fail(_ function: StaticString, _ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@7{{macro content: |          fatalError("bounds check failure in \\(function): expected \\(expected) but got \\(actual)")|}}
//   expected-remark@8{{macro content: |        }|}}
//   expected-remark@9{{macro content: |        _fail("g", expected, actual)|}}
//   expected-remark@10{{macro content: |      }|}}
//   expected-remark@11{{macro content: |      _boundsCheckFailure((CUnsignedLong(kCount) + CUnsignedLong(n)), p.count)|}}
//   expected-remark@12{{macro content: |    }|}}
//   expected-remark@13{{macro content: |    return unsafe g(p.baseAddress, n)|}}
//   expected-remark@14{{macro content: |}|}}
// }}
void g(const int * __counted_by(kCount + n) p, size_t n);
} // namespace ns

//--- module.modulemap
module Test {
  header "test.h"
  requires cplusplus
}

//--- test.swift
import Test

func callStaticMethod(_ p: UnsafeBufferPointer<CInt>, _ n: Int) {
  unsafe S.f(p, n)
}

func callNamespaceFunction(_ p: UnsafeBufferPointer<CInt>, _ n: Int) {
  unsafe ns.g(p, n)
}
