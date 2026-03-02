// REQUIRES: swift_feature_Lifetimes

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -I %t -enable-experimental-feature Lifetimes -strict-memory-safety -Xcc -Wno-ignored-attributes -Xcc -Wno-nullability-completeness \
// RUN:   %t/test.swift -verify -verify-additional-file %t%{fs-sep}test.h -Rmacro-expansions -suppress-notes

// Check that ClangImporter correctly infers and expands @_SwiftifyImport macros for functions with __sized_by __noescape parameters.

//--- test.h
#pragma once

#include <stdint.h>

#ifndef __sized_by
#define __sized_by(x) __attribute__((__sized_by__(x)))
#endif
#define __noescape __attribute__((noescape))

// expected-expansion@+13:63{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func simple(_ p: RawSpan) {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: p.byteCount)!|}}
//   expected-remark@4{{macro content: |    let _pPtr = unsafe p.withUnsafeBytes {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe simple(len, _pPtr.baseAddress!)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
void simple(int len, const void * __sized_by(len) __noescape p);

// expected-expansion@+13:38{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func swiftAttr(_ p: RawSpan) {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: p.byteCount)!|}}
//   expected-remark@4{{macro content: |    let _pPtr = unsafe p.withUnsafeBytes {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe swiftAttr(len, _pPtr.baseAddress!)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
void swiftAttr(int len, const void *p) __attribute__((swift_attr(
    "@_SwiftifyImport(.sizedBy(pointer: .param(2), size: \"len\"), .nonescaping(pointer: .param(2)), spanAvailability: \"visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4\")")));

// expected-expansion@+22:108{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func shared(_ p1: RawSpan, _ p2: RawSpan) {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: p1.byteCount)!|}}
//   expected-remark@4{{macro content: |    if p2.byteCount != len {|}}
//   expected-remark@5{{macro content: |      fatalError("bounds check failure in shared: expected \\(len) but got \\(p2.byteCount)")|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    let _p1Ptr = unsafe p1.withUnsafeBytes {|}}
//   expected-remark@8{{macro content: |        unsafe $0|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    defer {|}}
//   expected-remark@11{{macro content: |        _fixLifetime(p1)|}}
//   expected-remark@12{{macro content: |    }|}}
//   expected-remark@13{{macro content: |    let _p2Ptr = unsafe p2.withUnsafeBytes {|}}
//   expected-remark@14{{macro content: |        unsafe $0|}}
//   expected-remark@15{{macro content: |    }|}}
//   expected-remark@16{{macro content: |    defer {|}}
//   expected-remark@17{{macro content: |        _fixLifetime(p2)|}}
//   expected-remark@18{{macro content: |    }|}}
//   expected-remark@19{{macro content: |    return unsafe shared(len, _p1Ptr.baseAddress!, _p2Ptr.baseAddress!)|}}
//   expected-remark@20{{macro content: |}|}}
// }}
void shared(int len, const void * __sized_by(len) __noescape p1, const void * __sized_by(len) __noescape p2);

// expected-expansion@+16:89{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func complexExpr(_ len: Int32, _ offset: Int32, _ p: RawSpan) {|}}
//   expected-remark@3{{macro content: |    let _pCount = p.byteCount|}}
//   expected-remark@4{{macro content: |    if _pCount != len - offset {|}}
//   expected-remark@5{{macro content: |      fatalError("bounds check failure in complexExpr: expected \\(len - offset) but got \\(_pCount)")|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    let _pPtr = unsafe p.withUnsafeBytes {|}}
//   expected-remark@8{{macro content: |        unsafe $0|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    defer {|}}
//   expected-remark@11{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@12{{macro content: |    }|}}
//   expected-remark@13{{macro content: |    return unsafe complexExpr(len, offset, _pPtr.baseAddress!)|}}
//   expected-remark@14{{macro content: |}|}}
// }}
void complexExpr(int len, int offset, const void * __sized_by(len - offset) __noescape p);

// expected-expansion@+13:90{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func nullUnspecified(_ p: RawSpan) {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: p.byteCount)!|}}
//   expected-remark@4{{macro content: |    let _pPtr = unsafe p.withUnsafeBytes {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe nullUnspecified(len, _pPtr.baseAddress!)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
void nullUnspecified(int len, const void * __sized_by(len) __noescape _Null_unspecified p);

// expected-expansion@+13:73{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func nonnull(_ p: RawSpan) {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: p.byteCount)!|}}
//   expected-remark@4{{macro content: |    let _pPtr = unsafe p.withUnsafeBytes {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe nonnull(len, _pPtr.baseAddress!)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
void nonnull(int len, const void * __sized_by(len) __noescape _Nonnull p);

// expected-expansion@+13:75{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func nullable(_ p: RawSpan?) {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: p?.byteCount ?? 0)!|}}
//   expected-remark@4{{macro content: |    let _pPtr = unsafe p?.withUnsafeBytes {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe nullable(len, _pPtr?.baseAddress)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
void nullable(int len, const void * __sized_by(len) __noescape _Nullable p);

// expected-expansion@+6:71{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func returnPointer(_ len: Int32) -> UnsafeRawBufferPointer {|}}
//   expected-remark@3{{macro content: |    return unsafe UnsafeRawBufferPointer(start: unsafe returnPointer(len), count: Int(len))|}}
//   expected-remark@4{{macro content: |}|}}
// }}
const void * __sized_by(len) __noescape _Nonnull returnPointer(int len);

typedef struct foo opaque_t;
// expected-expansion@+13:61{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func opaque(_ p: RawSpan) {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: p.byteCount)!|}}
//   expected-remark@4{{macro content: |    let _pPtr = unsafe p.withUnsafeBytes {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe opaque(len, OpaquePointer(_pPtr.baseAddress!))|}}
//   expected-remark@11{{macro content: |}|}}
// }}
void opaque(int len, opaque_t * __sized_by(len) __noescape p);

// expected-expansion@+13:41{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func bytesized(_ _bytesized_param1: RawSpan) {|}}
//   expected-remark@3{{macro content: |    let _bytesized_param0 = Int32(exactly: _bytesized_param1.byteCount)!|}}
//   expected-remark@4{{macro content: |    let __bytesized_param1Ptr = unsafe _bytesized_param1.withUnsafeBytes {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(_bytesized_param1)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe bytesized(_bytesized_param0, __bytesized_param1Ptr.baseAddress!.assumingMemoryBound(to: UInt8.self))|}}
//   expected-remark@11{{macro content: |}|}}
// }}
void bytesized(int size, const uint8_t *__sized_by(size) __noescape);

// expected-expansion@+13:59{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(_charsized_param0: copy _charsized_param0) @_disfavoredOverload public func charsized(_ _charsized_param0: inout MutableRawSpan) {|}}
//   expected-remark@3{{macro content: |    let _charsized_param1 = Int32(exactly: _charsized_param0.byteCount)!|}}
//   expected-remark@4{{macro content: |    let __charsized_param0Ptr = unsafe _charsized_param0.withUnsafeMutableBytes {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(_charsized_param0)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe charsized(__charsized_param0Ptr.baseAddress!.assumingMemoryBound(to: CChar.self), _charsized_param1)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
void charsized(char *__sized_by(size) __noescape, int size);

void doublebytesized(uint16_t *__sized_by(size) __noescape, int size);

//--- module.modulemap
module Test {
  header "test.h"
}

//--- test.swift
// GENERATED-BY: %target-swift-ide-test -print-module -module-to-print=Test -plugin-path %swift-plugin-dir -I %t -source-filename=x -enable-experimental-feature Lifetimes -Xcc -Wno-ignored-attributes -Xcc -Wno-nullability-completeness > %t/Test-interface.swift && %swift-function-caller-generator Test %t/Test-interface.swift
// GENERATED-HASH: 95f68132be402dc5cff4ab6ea91d2e55d83a3268cb7f4169471b977e4bc1ccf4
import Test



func call_simple(_ len: Int32, _ p: UnsafeRawPointer!) {
  return unsafe simple(len, p)
}

func call_swiftAttr(_ len: Int32, _ p: UnsafeRawPointer!) {
  return unsafe swiftAttr(len, p)
}

func call_shared(_ len: Int32, _ p1: UnsafeRawPointer!, _ p2: UnsafeRawPointer!) {
  return unsafe shared(len, p1, p2)
}

func call_complexExpr(_ len: Int32, _ offset: Int32, _ p: UnsafeRawPointer!) {
  return unsafe complexExpr(len, offset, p)
}

func call_nullUnspecified(_ len: Int32, _ p: UnsafeRawPointer!) {
  return unsafe nullUnspecified(len, p)
}

func call_nonnull(_ len: Int32, _ p: UnsafeRawPointer) {
  return unsafe nonnull(len, p)
}

func call_nullable(_ len: Int32, _ p: UnsafeRawPointer?) {
  return unsafe nullable(len, p)
}

func call_returnPointer(_ len: Int32) -> UnsafeRawPointer {
  return unsafe returnPointer(len)
}

func call_opaque(_ len: Int32, _ p: OpaquePointer!) {
  return unsafe opaque(len, p)
}

func call_bytesized(_ size: Int32, _ _bytesized_param1: UnsafePointer<UInt8>!) {
  return unsafe bytesized(size,  _bytesized_param1)
}

func call_charsized(_ _charsized_param0: UnsafeMutablePointer<CChar>!, _ size: Int32) {
  return unsafe charsized( _charsized_param0, size)
}

func call_doublebytesized(_ _doublebytesized_param0: UnsafeMutablePointer<UInt16>!, _ size: Int32) {
  return unsafe doublebytesized( _doublebytesized_param0, size)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_bytesized(_ _bytesized_param1: RawSpan) {
  return bytesized(_bytesized_param1)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(_charsized_param0: copy _charsized_param0)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_charsized(_ _charsized_param0: inout MutableRawSpan) {
  return charsized(&_charsized_param0)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_complexExpr(_ len: Int32, _ offset: Int32, _ p: RawSpan) {
  return complexExpr(len, offset, p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_nonnull(_ p: RawSpan) {
  return nonnull(p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_nullUnspecified(_ p: RawSpan) {
  return nullUnspecified(p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_nullable(_ p: RawSpan?) {
  return nullable(p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_opaque(_ p: RawSpan) {
  return opaque(p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_returnPointer(_ len: Int32) -> UnsafeRawBufferPointer {
  return unsafe returnPointer(len)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_shared(_ p1: RawSpan, _ p2: RawSpan) {
  return shared(p1, p2)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_simple(_ p: RawSpan) {
  return simple(p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_swiftAttr(_ p: RawSpan) {
  return swiftAttr(p)
}
