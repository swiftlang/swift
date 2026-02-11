
// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -o %t/test.swiftmodule %t/test.swift -I %t -strict-memory-safety \
// RUN:   -verify -verify-additional-file %t%{fs-sep}test.h

// RUN: env SWIFT_BACKTRACE="" %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -o %t/test.swiftmodule %t/test.swift -I %t -strict-memory-safety \
// RUN:   -dump-macro-expansions 2> %t/expansions.out

// RUN: %if OS_FAMILY=darwin %{ \
// RUN:   %diff %t/%target-os-expansions.expected %t/expansions.out \
// RUN: %} %else %{\
// RUN:   %diff %t/other-expansions.expected %t/expansions.out \
// RUN: %}

//--- test.h
#pragma once

#define __counted_by(x) __attribute__((__counted_by__(x)))
#define __noescape __attribute__((noescape))

void bufferPointer(int *__counted_by(len), int len) __attribute__((availability(ios,introduced=2.0))) __attribute__((availability(macosx,introduced=10.5)));
void span(int *__counted_by(len) p __noescape, int len) __attribute__((availability(ios,introduced=2.0))) __attribute__((availability(macosx,introduced=10.5)));

//--- macosx-expansions.expected
@__swiftmacro_So13bufferPointer15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@available(macOS 10.5, *) @_alwaysEmitIntoClient @_disfavoredOverload
public func bufferPointer(_ _bufferPointer_param0: UnsafeMutableBufferPointer<Int32>) {
    let _bufferPointer_param1 = Int32(exactly: _bufferPointer_param0.count)!
    return unsafe bufferPointer(_bufferPointer_param0.baseAddress!, _bufferPointer_param1)
}
------------------------------
@__swiftmacro_So4span15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@available(macOS 10.5, *) @_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
public func span(_ p: inout MutableSpan<Int32>) {
    let len = Int32(exactly: p.count)!
    let _pPtr = unsafe p.withUnsafeMutableBufferPointer {
        unsafe $0
    }
    return unsafe span(_pPtr.baseAddress!, len)
}
------------------------------
//--- ios-expansions.expected
@__swiftmacro_So13bufferPointer15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@available(iOS 2.0, *) @_alwaysEmitIntoClient @_disfavoredOverload
public func bufferPointer(_ _bufferPointer_param0: UnsafeMutableBufferPointer<Int32>) {
    let _bufferPointer_param1 = Int32(exactly: _bufferPointer_param0.count)!
    return unsafe bufferPointer(_bufferPointer_param0.baseAddress!, _bufferPointer_param1)
}
------------------------------
@__swiftmacro_So4span15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@available(iOS 2.0, *) @_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
public func span(_ p: inout MutableSpan<Int32>) {
    let len = Int32(exactly: p.count)!
    let _pPtr = unsafe p.withUnsafeMutableBufferPointer {
        unsafe $0
    }
    return unsafe span(_pPtr.baseAddress!, len)
}
------------------------------
//--- watchos-expansions.expected
@__swiftmacro_So13bufferPointer15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@available(watchOS 2.0, *) @_alwaysEmitIntoClient @_disfavoredOverload
public func bufferPointer(_ _bufferPointer_param0: UnsafeMutableBufferPointer<Int32>) {
    let _bufferPointer_param1 = Int32(exactly: _bufferPointer_param0.count)!
    return unsafe bufferPointer(_bufferPointer_param0.baseAddress!, _bufferPointer_param1)
}
------------------------------
@__swiftmacro_So4span15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@available(watchOS 2.0, *) @_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
public func span(_ p: inout MutableSpan<Int32>) {
    let len = Int32(exactly: p.count)!
    let _pPtr = unsafe p.withUnsafeMutableBufferPointer {
        unsafe $0
    }
    return unsafe span(_pPtr.baseAddress!, len)
}
------------------------------
//--- xros-expansions.expected
@__swiftmacro_So13bufferPointer15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@available(visionOS 1.0, *) @_alwaysEmitIntoClient @_disfavoredOverload
public func bufferPointer(_ _bufferPointer_param0: UnsafeMutableBufferPointer<Int32>) {
    let _bufferPointer_param1 = Int32(exactly: _bufferPointer_param0.count)!
    return unsafe bufferPointer(_bufferPointer_param0.baseAddress!, _bufferPointer_param1)
}
------------------------------
@__swiftmacro_So4span15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@available(visionOS 1.0, *) @_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload
public func span(_ p: inout MutableSpan<Int32>) {
    let len = Int32(exactly: p.count)!
    let _pPtr = unsafe p.withUnsafeMutableBufferPointer {
        unsafe $0
    }
    return unsafe span(_pPtr.baseAddress!, len)
}
------------------------------
//--- tvos-expansions.expected
@__swiftmacro_So13bufferPointer15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public func bufferPointer(_ _bufferPointer_param0: UnsafeMutableBufferPointer<Int32>) {
    let _bufferPointer_param1 = Int32(exactly: _bufferPointer_param0.count)!
    return unsafe bufferPointer(_bufferPointer_param0.baseAddress!, _bufferPointer_param1)
}
------------------------------
@__swiftmacro_So4span15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload public func span(_ p: inout MutableSpan<Int32>) {
    let len = Int32(exactly: p.count)!
    let _pPtr = unsafe p.withUnsafeMutableBufferPointer {
        unsafe $0
    }
    return unsafe span(_pPtr.baseAddress!, len)
}
------------------------------
//--- other-expansions.expected
@__swiftmacro_So13bufferPointer15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @_disfavoredOverload public func bufferPointer(_ _bufferPointer_param0: UnsafeMutableBufferPointer<Int32>) {
    let _bufferPointer_param1 = Int32(exactly: _bufferPointer_param0.count)!
    return unsafe bufferPointer(_bufferPointer_param0.baseAddress!, _bufferPointer_param1)
}
------------------------------
@__swiftmacro_So4span15_SwiftifyImportfMp_.swift
------------------------------
/// This is an auto-generated wrapper for safer interop
@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload public func span(_ p: inout MutableSpan<Int32>) {
    let len = Int32(exactly: p.count)!
    let _pPtr = unsafe p.withUnsafeMutableBufferPointer {
        unsafe $0
    }
    return unsafe span(_pPtr.baseAddress!, len)
}
------------------------------
//--- test.swift
import Test

func callBufferPointer(_ p: UnsafeMutablePointer<CInt>, len: CInt) {
  unsafe bufferPointer(p, len)
}

func callBufferPointer(_ p: UnsafeMutableBufferPointer<CInt>) {
  unsafe bufferPointer(p)
}

func callSpan(_ p: UnsafeMutablePointer<CInt>, len: CInt) {
  unsafe span(p, len)
}

func callSpan(_ p: inout MutableSpan<CInt>) {
  span(&p)
}

//--- module.modulemap
module Test {
  header "test.h"
}

