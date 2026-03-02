// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -I %t -strict-memory-safety -Xcc -Wno-nullability-completeness \
// RUN:   %t/test.swift -verify -verify-additional-file %t%{fs-sep}test.h -Rmacro-expansions -suppress-notes

// Check that ClangImporter correctly infers and expands @_SwiftifyImport macros for functions with __sized_by parameters.

//--- test.h
#pragma once

#include <stdint.h>

#ifndef __sized_by
#define __sized_by(x) __attribute__((__sized_by__(x)))
#endif

// expected-expansion@+7:46{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func simple(_ p: UnsafeMutableRawBufferPointer) {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe simple(len, p.baseAddress!)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
void simple(int len, void * __sized_by(len) p);

// expected-expansion@+7:32{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func swiftAttr(_ p: UnsafeMutableRawBufferPointer) {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe swiftAttr(len, p.baseAddress!)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
void swiftAttr(int len, void *p) __attribute__((swift_attr(
    "@_SwiftifyImport(.sizedBy(pointer: .param(2), size: \"len\"))")));

// expected-expansion@+10:74{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func shared(_ p1: UnsafeMutableRawBufferPointer, _ p2: UnsafeMutableRawBufferPointer) {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: p1.count)!|}}
//   expected-remark@4{{macro content: |    if p2.count != len {|}}
//   expected-remark@5{{macro content: |      fatalError("bounds check failure in shared: expected \\(len) but got \\(p2.count)")|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    return unsafe shared(len, p1.baseAddress!, p2.baseAddress!)|}}
//   expected-remark@8{{macro content: |}|}}
// }}
void shared(int len, void * __sized_by(len) p1, void * __sized_by(len) p2);

// expected-expansion@+10:72{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func complexExpr(_ len: Int32, _ offset: Int32, _ p: UnsafeMutableRawBufferPointer) {|}}
//   expected-remark@3{{macro content: |    let _pCount = p.count|}}
//   expected-remark@4{{macro content: |    if _pCount != len - offset {|}}
//   expected-remark@5{{macro content: |      fatalError("bounds check failure in complexExpr: expected \\(len - offset) but got \\(_pCount)")|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    return unsafe complexExpr(len, offset, p.baseAddress!)|}}
//   expected-remark@8{{macro content: |}|}}
// }}
void complexExpr(int len, int offset, void * __sized_by(len - offset) p);

// expected-expansion@+7:73{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func nullUnspecified(_ p: UnsafeMutableRawBufferPointer) {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe nullUnspecified(len, p.baseAddress!)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
void nullUnspecified(int len, void * __sized_by(len) _Null_unspecified p);

// expected-expansion@+7:56{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func nonnull(_ p: UnsafeMutableRawBufferPointer) {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe nonnull(len, p.baseAddress!)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
void nonnull(int len, void * __sized_by(len) _Nonnull p);

// expected-expansion@+7:58{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func nullable(_ p: UnsafeMutableRawBufferPointer?) {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: unsafe p?.count ?? 0)!|}}
//   expected-remark@4{{macro content: |    return unsafe nullable(len, p?.baseAddress)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
void nullable(int len, void * __sized_by(len) _Nullable p);

// expected-expansion@+6:45{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func returnPointer(_ len: Int32) -> UnsafeMutableRawBufferPointer {|}}
//   expected-remark@3{{macro content: |    return unsafe UnsafeMutableRawBufferPointer(start: unsafe returnPointer(len), count: Int(len))|}}
//   expected-remark@4{{macro content: |}|}}
// }}
void * __sized_by(len) returnPointer(int len);

typedef struct foo opaque_t;
// expected-expansion@+7:50{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func opaque(_ p: UnsafeRawBufferPointer) {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe opaque(len, OpaquePointer(p.baseAddress!))|}}
//   expected-remark@5{{macro content: |}|}}
// }}
void opaque(int len, opaque_t * __sized_by(len) p);

typedef opaque_t *opaqueptr_t;
// expected-expansion@+7:54{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func opaqueptr(_ p: UnsafeRawBufferPointer) {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe opaqueptr(len, OpaquePointer(p.baseAddress!))|}}
//   expected-remark@5{{macro content: |}|}}
// }}
void opaqueptr(int len, opaqueptr_t __sized_by(len) p);

// expected-expansion@+7:48{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func charsized(_ _charsized_param0: UnsafeMutableRawBufferPointer) {|}}
//   expected-remark@3{{macro content: |    let _charsized_param1 = Int32(exactly: _charsized_param0.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe charsized(_charsized_param0.baseAddress!.assumingMemoryBound(to: CChar.self), _charsized_param1)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
void charsized(char *__sized_by(size), int size);

// expected-expansion@+6:45{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func bytesized(_ size: Int32) -> UnsafeMutableRawBufferPointer {|}}
//   expected-remark@3{{macro content: |    return unsafe UnsafeMutableRawBufferPointer(start: unsafe bytesized(size), count: Int(size))|}}
//   expected-remark@4{{macro content: |}|}}
// }}
uint8_t *__sized_by(size) bytesized(int size);

void doublebytesized(uint16_t *__sized_by(size), int size);

typedef uint8_t * bytesizedptr_t;
// expected-expansion@+7:66{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func aliasedBytesized(_ p: UnsafeMutableRawBufferPointer) {|}}
//   expected-remark@3{{macro content: |    let size = Int32(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe aliasedBytesized(p.baseAddress!.assumingMemoryBound(to: UInt8.self), size)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
void aliasedBytesized(bytesizedptr_t __sized_by(size) p, int size);

//--- module.modulemap
module Test {
  header "test.h"
}

//--- test.swift
// GENERATED-BY: %target-swift-ide-test -print-module -module-to-print=Test -plugin-path %swift-plugin-dir -I %t -source-filename=x -Xcc -Wno-nullability-completeness > %t/Test-interface.swift && %swift-function-caller-generator Test %t/Test-interface.swift
// GENERATED-HASH: aa5390a44af0df94f37482b65880a491defd2b196a9ff00e67dfcc834e86c6cc
import Test



func call_simple(_ len: Int32, _ p: UnsafeMutableRawPointer!) {
  return unsafe simple(len, p)
}

func call_swiftAttr(_ len: Int32, _ p: UnsafeMutableRawPointer!) {
  return unsafe swiftAttr(len, p)
}

func call_shared(_ len: Int32, _ p1: UnsafeMutableRawPointer!, _ p2: UnsafeMutableRawPointer!) {
  return unsafe shared(len, p1, p2)
}

func call_complexExpr(_ len: Int32, _ offset: Int32, _ p: UnsafeMutableRawPointer!) {
  return unsafe complexExpr(len, offset, p)
}

func call_nullUnspecified(_ len: Int32, _ p: UnsafeMutableRawPointer!) {
  return unsafe nullUnspecified(len, p)
}

func call_nonnull(_ len: Int32, _ p: UnsafeMutableRawPointer) {
  return unsafe nonnull(len, p)
}

func call_nullable(_ len: Int32, _ p: UnsafeMutableRawPointer?) {
  return unsafe nullable(len, p)
}

func call_returnPointer(_ len: Int32) -> UnsafeMutableRawPointer! {
  return unsafe returnPointer(len)
}

func call_opaque(_ len: Int32, _ p: OpaquePointer!) {
  return unsafe opaque(len, p)
}

func call_opaqueptr(_ len: Int32, _ p: OpaquePointer!) {
  return unsafe opaqueptr(len, p)
}

func call_charsized(_ _charsized_param0: UnsafeMutablePointer<CChar>!, _ size: Int32) {
  return unsafe charsized( _charsized_param0, size)
}

func call_bytesized(_ size: Int32) -> UnsafeMutablePointer<UInt8>! {
  return unsafe bytesized(size)
}

func call_doublebytesized(_ _doublebytesized_param0: UnsafeMutablePointer<UInt16>!, _ size: Int32) {
  return unsafe doublebytesized( _doublebytesized_param0, size)
}

func call_aliasedBytesized(_ p: UnsafeMutablePointer<UInt8>!, _ size: Int32) {
  return unsafe aliasedBytesized(p, size)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_aliasedBytesized(_ p: UnsafeMutableRawBufferPointer) {
  return unsafe aliasedBytesized(p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_bytesized(_ size: Int32) -> UnsafeMutableRawBufferPointer {
  return unsafe bytesized(size)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_charsized(_ _charsized_param0: UnsafeMutableRawBufferPointer) {
  return unsafe charsized(_charsized_param0)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_complexExpr(_ len: Int32, _ offset: Int32, _ p: UnsafeMutableRawBufferPointer) {
  return unsafe complexExpr(len, offset, p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_nonnull(_ p: UnsafeMutableRawBufferPointer) {
  return unsafe nonnull(p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_nullUnspecified(_ p: UnsafeMutableRawBufferPointer) {
  return unsafe nullUnspecified(p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_nullable(_ p: UnsafeMutableRawBufferPointer?) {
  return unsafe nullable(p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_opaque(_ p: UnsafeRawBufferPointer) {
  return unsafe opaque(p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_opaqueptr(_ p: UnsafeRawBufferPointer) {
  return unsafe opaqueptr(p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_returnPointer(_ len: Int32) -> UnsafeMutableRawBufferPointer {
  return unsafe returnPointer(len)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_shared(_ p1: UnsafeMutableRawBufferPointer, _ p2: UnsafeMutableRawBufferPointer) {
  return unsafe shared(p1, p2)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_simple(_ p: UnsafeMutableRawBufferPointer) {
  return unsafe simple(p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_swiftAttr(_ p: UnsafeMutableRawBufferPointer) {
  return unsafe swiftAttr(p)
}
