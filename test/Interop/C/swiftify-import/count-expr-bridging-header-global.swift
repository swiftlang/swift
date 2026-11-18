// The expected expansions spell out the canonical type of `size_t`, which is
// `unsigned long long` on Windows and `unsigned int` on 32-bit targets.
// UNSUPPORTED: OS=windows-msvc
// UNSUPPORTED: PTRSIZE=32

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -strict-memory-safety \
// RUN:   %t%{fs-sep}test.swift -import-bridging-header %t%{fs-sep}bridging.h -verify -verify-additional-file %t%{fs-sep}bridging.h \
// RUN:   -Rmacro-expansions -suppress-notes -eager-macro-checking

// Check a count expression that uses a `size_t` global declared in a bridging
// header. The importer puts bridging header declarations in a module of their
// own, and the global is converted as usual.

//--- bridging.h
#pragma once

#include <lifetimebound.h>
#include <ptrcheck.h>
#include <stddef.h>

extern const size_t kSizeCount;
void sizeGlobalCompound(const int * __counted_by(kSizeCount + n) __noescape p, size_t n);
// expected-expansion@-1:88{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func sizeGlobalCompound(_ p: Span<CInt>, _ n: Int) {|}}
//   expected-remark@3{{macro content: |    if p.count != (CUnsignedLong(kSizeCount) + CUnsignedLong(n)) {|}}
//   expected-remark@4{{macro content: |      @inline(never) func _boundsCheckFailure<E: BinaryInteger, A: BinaryInteger>(_ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@5{{macro content: |        @inline(never) func _fail(_ function: StaticString, _ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@6{{macro content: |          fatalError("bounds check failure in \\(function): expected \\(expected) but got \\(actual)")|}}
//   expected-remark@7{{macro content: |        }|}}
//   expected-remark@8{{macro content: |        _fail("sizeGlobalCompound", expected, actual)|}}
//   expected-remark@9{{macro content: |      }|}}
//   expected-remark@10{{macro content: |      _boundsCheckFailure((CUnsignedLong(kSizeCount) + CUnsignedLong(n)), p.count)|}}
//   expected-remark@11{{macro content: |    }|}}
//   expected-remark@12{{macro content: |    let _pPtr = p.withUnsafeBufferPointer {|}}
//   expected-remark@13{{macro content: |        unsafe $0|}}
//   expected-remark@14{{macro content: |    }|}}
//   expected-remark@15{{macro content: |    defer {|}}
//   expected-remark@16{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@17{{macro content: |    }|}}
//   expected-remark@18{{macro content: |    return unsafe sizeGlobalCompound(_pPtr.baseAddress, n)|}}
//   expected-remark@19{{macro content: |}|}}
// }}

//--- test.swift
@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
func callSizeGlobalCompound(_ p: Span<CInt>, _ n: Int) {
  sizeGlobalCompound(p, n)
}
