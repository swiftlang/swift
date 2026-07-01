// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: env SWIFT_BACKTRACE="" %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -strict-memory-safety \
// RUN:   -Xcc -Werror %t/test.swift -import-bridging-header %t/test.h -dump-macro-expansions -eager-macro-checking -Xcc -fbounds-safety -disable-objc-interop 2> %t/expansion.out
// RUN: %diff %t/expansion.out %t/expansion.expected

//--- test.swift
public func callMemCpyOriginal(_ dst: UnsafeMutableRawPointer, _ src: UnsafeMutableRawPointer) {
  let _ = unsafe memcpy(dst, src, 13)
  let _ = unsafe foo(dst, src, 13)
}

public func callMemCpySwiftified(_ dst: UnsafeMutableRawBufferPointer, _ src: UnsafeRawBufferPointer) {
  let _ = unsafe foo(dst, src)
}

//--- test.h
#include <stddef.h>

// In -fbounds-safety mode this will inherit __sized_by attributes from
// clang's implicit declaration of memcpy. These __sized_by attributes
// refer to the third parameter, but it has no name: swiftify must skip
// this decl to prevent emitting a malformed size expression.
// Unlike memcmp, memcpy is not automatically skipped for belonging to
// SwiftShims.
extern void* memcpy(void*, const void*, size_t);

#define __sized_by(n) __attribute__((sized_by(n)))

// This is here to verify that the output actually contains macro expansions.
void * __sized_by(n) foo(void *__sized_by(n) dst, const void *__sized_by(n) src, size_t n);

//--- expansion.expected
@__swiftmacro_So3foo15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public func foo(_ dst: UnsafeMutableRawBufferPointer, _ src: UnsafeRawBufferPointer) -> UnsafeMutableRawBufferPointer {
    let n = src.count
    if dst.count != n {
      fatalError("bounds check failure in foo: expected \(n) but got \(dst.count)")
    }
    return unsafe UnsafeMutableRawBufferPointer(start: unsafe foo(dst.baseAddress, src.baseAddress, n), count: Int(n))
}
------------------------------
