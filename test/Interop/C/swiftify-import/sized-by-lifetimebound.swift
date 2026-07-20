// REQUIRES: swift_feature_SafeInteropWrappers
// REQUIRES: swift_feature_SafeInteropWrappersNullAsEmptySpan
// REQUIRES: swift_feature_Lifetimes

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -I %t -enable-experimental-feature SafeInteropWrappers -enable-experimental-feature SafeInteropWrappersNullAsEmptySpan -enable-experimental-feature Lifetimes -strict-memory-safety -Xcc -Wno-nullability-completeness \
// RUN:   %t/test.swift -verify -verify-additional-file %t%{fs-sep}test.h -verify-additional-prefix experimental- -Rmacro-expansions -suppress-notes -eager-macro-checking

// lifetimebound support is not stabilized yet. Don't generate _any_ overloads on functions with lifetimebound to prevent future sourcebreak.
// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -I %t -enable-experimental-feature Lifetimes -strict-memory-safety -Xcc -Wno-nullability-completeness \
// RUN:   %t/test.swift -verify -verify-additional-file %t%{fs-sep}test.h -verify-additional-prefix stable- -Rmacro-expansions -suppress-notes -eager-macro-checking

// Check that ClangImporter correctly infers and expands @_SwiftifyImport macros for functions with __sized_by __lifetimebound parameters and return values.

//--- test.h
#pragma once

#include <stdint.h>

#ifndef __sized_by
#define __sized_by(x) __attribute__((__sized_by__(x)))
#endif
#define __lifetimebound __attribute__((lifetimebound))

// expected-experimental-expansion@+18:70{{
//   expected-experimental-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-experimental-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_disfavoredOverload public func simple(_ len: CInt, _ p: RawSpan) -> RawSpan {|}}
//   expected-experimental-remark@3{{macro content: |    let len2 = CInt(exactly: p.byteCount)!|}}
//   expected-experimental-remark@4{{macro content: |    let _pPtr = p.withUnsafeBytes {|}}
//   expected-experimental-remark@5{{macro content: |        unsafe $0|}}
//   expected-experimental-remark@6{{macro content: |    }|}}
//   expected-experimental-remark@7{{macro content: |    defer {|}}
//   expected-experimental-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-experimental-remark@9{{macro content: |    }|}}
//   expected-experimental-remark@10{{macro content: |    let _resultValue: UnsafeRawPointer? = unsafe simple(len, len2, _pPtr.baseAddress)|}}
//   expected-experimental-remark@11{{macro content: |    if unsafe _resultValue == nil {|}}
//   expected-experimental-remark@12{{macro content: |      precondition(len == 0, "sized_by may only be null if size is 0 (unlike sized_by_or_null)")|}}
//   expected-experimental-remark@13{{macro content: |      return RawSpan()|}}
//   expected-experimental-remark@14{{macro content: |    }|}}
//   expected-experimental-remark@15{{macro content: |    return unsafe _swiftifyOverrideLifetime(RawSpan(_unsafeStart: _resultValue!, byteCount: Int(len)), copying: ())|}}
//   expected-experimental-remark@16{{macro content: |}|}}
// }}
const void * __sized_by(len) simple(int len, int len2, const void * p __sized_by(len2) __lifetimebound);

// expected-experimental-expansion@+18:60{{
//   expected-experimental-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-experimental-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_disfavoredOverload public func shared(_ p: RawSpan) -> RawSpan {|}}
//   expected-experimental-remark@3{{macro content: |    let len = CInt(exactly: p.byteCount)!|}}
//   expected-experimental-remark@4{{macro content: |    let _pPtr = p.withUnsafeBytes {|}}
//   expected-experimental-remark@5{{macro content: |        unsafe $0|}}
//   expected-experimental-remark@6{{macro content: |    }|}}
//   expected-experimental-remark@7{{macro content: |    defer {|}}
//   expected-experimental-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-experimental-remark@9{{macro content: |    }|}}
//   expected-experimental-remark@10{{macro content: |    let _resultValue: UnsafeRawPointer? = unsafe shared(len, _pPtr.baseAddress)|}}
//   expected-experimental-remark@11{{macro content: |    if unsafe _resultValue == nil {|}}
//   expected-experimental-remark@12{{macro content: |      precondition(len == 0, "sized_by may only be null if size is 0 (unlike sized_by_or_null)")|}}
//   expected-experimental-remark@13{{macro content: |      return RawSpan()|}}
//   expected-experimental-remark@14{{macro content: |    }|}}
//   expected-experimental-remark@15{{macro content: |    return unsafe _swiftifyOverrideLifetime(RawSpan(_unsafeStart: _resultValue!, byteCount: Int(len)), copying: ())|}}
//   expected-experimental-remark@16{{macro content: |}|}}
// }}
const void * __sized_by(len) shared(int len, const void * p __sized_by(len) __lifetimebound);

// expected-experimental-expansion@+18:96{{
//   expected-experimental-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-experimental-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_disfavoredOverload public func complexExpr(_ len: CInt, _ offset: CInt, _ p: RawSpan) -> RawSpan {|}}
//   expected-experimental-remark@3{{macro content: |    let len2 = CInt(exactly: p.byteCount)!|}}
//   expected-experimental-remark@4{{macro content: |    let _pPtr = p.withUnsafeBytes {|}}
//   expected-experimental-remark@5{{macro content: |        unsafe $0|}}
//   expected-experimental-remark@6{{macro content: |    }|}}
//   expected-experimental-remark@7{{macro content: |    defer {|}}
//   expected-experimental-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-experimental-remark@9{{macro content: |    }|}}
//   expected-experimental-remark@10{{macro content: |    let _resultValue: UnsafeRawPointer? = unsafe complexExpr(len, offset, len2, _pPtr.baseAddress)|}}
//   expected-experimental-remark@11{{macro content: |    if unsafe _resultValue == nil {|}}
//   expected-experimental-remark@12{{macro content: |      precondition((len - offset) == 0, "sized_by may only be null if size is 0 (unlike sized_by_or_null)")|}}
//   expected-experimental-remark@13{{macro content: |      return RawSpan()|}}
//   expected-experimental-remark@14{{macro content: |    }|}}
//   expected-experimental-remark@15{{macro content: |    return unsafe _swiftifyOverrideLifetime(RawSpan(_unsafeStart: _resultValue!, byteCount: Int((len - offset))), copying: ())|}}
//   expected-experimental-remark@16{{macro content: |}|}}
// }}
const void * __sized_by(len - offset) complexExpr(int len, int offset, int len2, const void * p __sized_by(len2) __lifetimebound);

// expected-experimental-expansion@+18:115{{
//   expected-experimental-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-experimental-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_disfavoredOverload public func nullUnspecified(_ len: CInt, _ p: RawSpan) -> RawSpan {|}}
//   expected-experimental-remark@3{{macro content: |    let len2 = CInt(exactly: p.byteCount)!|}}
//   expected-experimental-remark@4{{macro content: |    let _pPtr = p.withUnsafeBytes {|}}
//   expected-experimental-remark@5{{macro content: |        unsafe $0|}}
//   expected-experimental-remark@6{{macro content: |    }|}}
//   expected-experimental-remark@7{{macro content: |    defer {|}}
//   expected-experimental-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-experimental-remark@9{{macro content: |    }|}}
//   expected-experimental-remark@10{{macro content: |    let _resultValue: UnsafeRawPointer? = unsafe nullUnspecified(len, len2, _pPtr.baseAddress)|}}
//   expected-experimental-remark@11{{macro content: |    if unsafe _resultValue == nil {|}}
//   expected-experimental-remark@12{{macro content: |      precondition(len == 0, "sized_by may only be null if size is 0 (unlike sized_by_or_null)")|}}
//   expected-experimental-remark@13{{macro content: |      return RawSpan()|}}
//   expected-experimental-remark@14{{macro content: |    }|}}
//   expected-experimental-remark@15{{macro content: |    return unsafe _swiftifyOverrideLifetime(RawSpan(_unsafeStart: _resultValue!, byteCount: Int(len)), copying: ())|}}
//   expected-experimental-remark@16{{macro content: |}|}}
// }}
const void * __sized_by(len) _Null_unspecified nullUnspecified(int len, int len2, const void * _Null_unspecified p __sized_by(len2) __lifetimebound);

// expected-experimental-expansion@+13:89{{
//   expected-experimental-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-experimental-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_disfavoredOverload public func nonnull(_ len: CInt, _ p: RawSpan) -> RawSpan {|}}
//   expected-experimental-remark@3{{macro content: |    let len2 = CInt(exactly: p.byteCount)!|}}
//   expected-experimental-remark@4{{macro content: |    let _pPtr = p.withUnsafeBytes {|}}
//   expected-experimental-remark@5{{macro content: |        unsafe $0|}}
//   expected-experimental-remark@6{{macro content: |    }|}}
//   expected-experimental-remark@7{{macro content: |    defer {|}}
//   expected-experimental-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-experimental-remark@9{{macro content: |    }|}}
//   expected-experimental-remark@10{{macro content: |    return unsafe _swiftifyOverrideLifetime(RawSpan(_unsafeStart: unsafe nonnull(len, len2, _pPtr.baseAddress!), byteCount: Int(len)), copying: ())|}}
//   expected-experimental-remark@11{{macro content: |}|}}
// }}
const void * __sized_by(len) _Nonnull nonnull(int len, int len2, const void * _Nonnull p __sized_by(len2) __lifetimebound);

// expected-experimental-expansion@+18:92{{
//   expected-experimental-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-experimental-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_disfavoredOverload public func nullable(_ len: CInt, _ p: RawSpan) -> RawSpan {|}}
//   expected-experimental-remark@3{{macro content: |    let len2 = CInt(exactly: p.byteCount)!|}}
//   expected-experimental-remark@4{{macro content: |    let _pPtr = p.withUnsafeBytes {|}}
//   expected-experimental-remark@5{{macro content: |        unsafe $0|}}
//   expected-experimental-remark@6{{macro content: |    }|}}
//   expected-experimental-remark@7{{macro content: |    defer {|}}
//   expected-experimental-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-experimental-remark@9{{macro content: |    }|}}
//   expected-experimental-remark@10{{macro content: |    let _resultValue = unsafe nullable(len, len2, _pPtr.baseAddress)|}}
//   expected-experimental-remark@11{{macro content: |    if unsafe _resultValue == nil {|}}
//   expected-experimental-remark@12{{macro content: |      precondition(len == 0, "sized_by may only be null if size is 0 (unlike sized_by_or_null)")|}}
//   expected-experimental-remark@13{{macro content: |      return RawSpan()|}}
//   expected-experimental-remark@14{{macro content: |    }|}}
//   expected-experimental-remark@15{{macro content: |    return unsafe _swiftifyOverrideLifetime(RawSpan(_unsafeStart: _resultValue!, byteCount: Int(len)), copying: ())|}}
//   expected-experimental-remark@16{{macro content: |}|}}
// }}
const void * __sized_by(len) _Nullable nullable(int len, int len2, const void * _Nullable p __sized_by(len2) __lifetimebound);

typedef struct foo opaque_t;
// expected-experimental-expansion@+18:66{{
//   expected-experimental-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-experimental-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_disfavoredOverload public func opaque(_ len: CInt, _ p: RawSpan) -> RawSpan {|}}
//   expected-experimental-remark@3{{macro content: |    let len2 = CInt(exactly: p.byteCount)!|}}
//   expected-experimental-remark@4{{macro content: |    let _pPtr = p.withUnsafeBytes {|}}
//   expected-experimental-remark@5{{macro content: |        unsafe $0|}}
//   expected-experimental-remark@6{{macro content: |    }|}}
//   expected-experimental-remark@7{{macro content: |    defer {|}}
//   expected-experimental-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-experimental-remark@9{{macro content: |    }|}}
//   expected-experimental-remark@10{{macro content: |    let _resultValue: OpaquePointer? = unsafe opaque(len, len2, OpaquePointer(_pPtr.baseAddress))|}}
//   expected-experimental-remark@11{{macro content: |    if unsafe _resultValue == nil {|}}
//   expected-experimental-remark@12{{macro content: |      precondition(len == 0, "sized_by may only be null if size is 0 (unlike sized_by_or_null)")|}}
//   expected-experimental-remark@13{{macro content: |      return RawSpan()|}}
//   expected-experimental-remark@14{{macro content: |    }|}}
//   expected-experimental-remark@15{{macro content: |    return unsafe _swiftifyOverrideLifetime(RawSpan(_unsafeStart: unsafe UnsafeRawPointer(_resultValue!), byteCount: Int(len)), copying: ())|}}
//   expected-experimental-remark@16{{macro content: |}|}}
// }}
opaque_t * __sized_by(len) opaque(int len, int len2, opaque_t * p __sized_by(len2) __lifetimebound);

// expected-experimental-expansion@+11:70{{
//   expected-experimental-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-experimental-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(borrow p) @_disfavoredOverload public func nonsizedLifetime(_ len: CInt, _ p: UnsafeRawPointer!) -> RawSpan {|}}
//   expected-experimental-remark@3{{macro content: |    let _resultValue: UnsafeRawPointer? = unsafe nonsizedLifetime(len, p)|}}
//   expected-experimental-remark@4{{macro content: |    if unsafe _resultValue == nil {|}}
//   expected-experimental-remark@5{{macro content: |      precondition(len == 0, "sized_by may only be null if size is 0 (unlike sized_by_or_null)")|}}
//   expected-experimental-remark@6{{macro content: |      return RawSpan()|}}
//   expected-experimental-remark@7{{macro content: |    }|}}
//   expected-experimental-remark@8{{macro content: |    return unsafe _swiftifyOverrideLifetime(RawSpan(_unsafeStart: _resultValue!, byteCount: Int(len)), copying: ())|}}
//   expected-experimental-remark@9{{macro content: |}|}}
// }}
const void * __sized_by(len) nonsizedLifetime(int len, const void * p __lifetimebound);

// expected-experimental-expansion@+18:65{{
//   expected-experimental-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-experimental-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_disfavoredOverload public func bytesized(_ p: RawSpan) -> MutableRawSpan {|}}
//   expected-experimental-remark@3{{macro content: |    let size = CInt(exactly: p.byteCount)!|}}
//   expected-experimental-remark@4{{macro content: |    let _pPtr = p.withUnsafeBytes {|}}
//   expected-experimental-remark@5{{macro content: |        unsafe $0|}}
//   expected-experimental-remark@6{{macro content: |    }|}}
//   expected-experimental-remark@7{{macro content: |    defer {|}}
//   expected-experimental-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-experimental-remark@9{{macro content: |    }|}}
//   expected-experimental-remark@10{{macro content: |    let _resultValue: UnsafeMutablePointer<UInt8>? = unsafe bytesized(size, _pPtr.baseAddress?.assumingMemoryBound(to: UInt8.self))|}}
//   expected-experimental-remark@11{{macro content: |    if unsafe _resultValue == nil {|}}
//   expected-experimental-remark@12{{macro content: |      precondition(size == 0, "sized_by may only be null if size is 0 (unlike sized_by_or_null)")|}}
//   expected-experimental-remark@13{{macro content: |      return MutableRawSpan()|}}
//   expected-experimental-remark@14{{macro content: |    }|}}
//   expected-experimental-remark@15{{macro content: |    return unsafe _swiftifyOverrideLifetime(MutableRawSpan(_unsafeStart: _resultValue!, byteCount: Int(size)), copying: ())|}}
//   expected-experimental-remark@16{{macro content: |}|}}
// }}
uint8_t *__sized_by(size)  bytesized(int size, const uint8_t * p __sized_by(size) __lifetimebound);

// expected-experimental-expansion@+18:85{{
//   expected-experimental-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-experimental-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_lifetime(p: copy p) @_disfavoredOverload public func charsized(_ p: inout MutableRawSpan) -> MutableRawSpan {|}}
//   expected-experimental-remark@3{{macro content: |    let size = CInt(exactly: p.byteCount)!|}}
//   expected-experimental-remark@4{{macro content: |    let _pPtr = p.withUnsafeMutableBytes {|}}
//   expected-experimental-remark@5{{macro content: |        unsafe $0|}}
//   expected-experimental-remark@6{{macro content: |    }|}}
//   expected-experimental-remark@7{{macro content: |    defer {|}}
//   expected-experimental-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-experimental-remark@9{{macro content: |    }|}}
//   expected-experimental-remark@10{{macro content: |    let _resultValue: UnsafeMutablePointer<CChar>? = unsafe charsized(_pPtr.baseAddress?.assumingMemoryBound(to: CChar.self), size)|}}
//   expected-experimental-remark@11{{macro content: |    if unsafe _resultValue == nil {|}}
//   expected-experimental-remark@12{{macro content: |      precondition(size == 0, "sized_by may only be null if size is 0 (unlike sized_by_or_null)")|}}
//   expected-experimental-remark@13{{macro content: |      return MutableRawSpan()|}}
//   expected-experimental-remark@14{{macro content: |    }|}}
//   expected-experimental-remark@15{{macro content: |    return unsafe _swiftifyOverrideLifetime(MutableRawSpan(_unsafeStart: _resultValue!, byteCount: Int(size)), copying: ())|}}
//   expected-experimental-remark@16{{macro content: |}|}}
// }}
char *__sized_by(size) charsized(char * p __sized_by(size) __lifetimebound, int size);

const uint16_t *__sized_by(size)  doublebytesized(uint16_t * p __sized_by(size) __lifetimebound, int size);

//--- module.modulemap
module Test {
  header "test.h"
}

//--- test.swift
// GENERATED-BY: %target-swift-ide-test -print-module -module-to-print=Test -plugin-path %swift-plugin-dir -I %t -source-filename=x -enable-experimental-feature SafeInteropWrappers -enable-experimental-feature SafeInteropWrappersNullAsEmptySpan -Xcc -Wno-nullability-completeness > %t/Test-interface.swift && %swift-function-caller-generator Test %t/Test-interface.swift
// GENERATED-HASH: 8ed2812c1671e1913d51407099db2a357e9510a43ed7fb87b804a24ba5d6a633
import Test



func call_simple(_ len: CInt, _ len2: CInt, _ p: UnsafeRawPointer!) -> UnsafeRawPointer! {
  return unsafe simple(len, len2, p)
}

func call_shared(_ len: CInt, _ p: UnsafeRawPointer!) -> UnsafeRawPointer! {
  return unsafe shared(len, p)
}

func call_complexExpr(_ len: CInt, _ offset: CInt, _ len2: CInt, _ p: UnsafeRawPointer!) -> UnsafeRawPointer! {
  return unsafe complexExpr(len, offset, len2, p)
}

func call_nullUnspecified(_ len: CInt, _ len2: CInt, _ p: UnsafeRawPointer!) -> UnsafeRawPointer! {
  return unsafe nullUnspecified(len, len2, p)
}

func call_nonnull(_ len: CInt, _ len2: CInt, _ p: UnsafeRawPointer) -> UnsafeRawPointer {
  return unsafe nonnull(len, len2, p)
}

func call_nullable(_ len: CInt, _ len2: CInt, _ p: UnsafeRawPointer?) -> UnsafeRawPointer? {
  return unsafe nullable(len, len2, p)
}

func call_opaque(_ len: CInt, _ len2: CInt, _ p: OpaquePointer!) -> OpaquePointer! {
  return unsafe opaque(len, len2, p)
}

func call_nonsizedLifetime(_ len: CInt, _ p: UnsafeRawPointer!) -> UnsafeRawPointer! {
  return unsafe nonsizedLifetime(len, p)
}

func call_bytesized(_ size: CInt, _ p: UnsafePointer<UInt8>!) -> UnsafeMutablePointer<UInt8>! {
  return unsafe bytesized(size, p)
}

func call_charsized(_ p: UnsafeMutablePointer<CChar>!, _ size: CInt) -> UnsafeMutablePointer<CChar>! {
  return unsafe charsized(p, size)
}

func call_doublebytesized(_ p: UnsafeMutablePointer<UInt16>!, _ size: CInt) -> UnsafePointer<UInt16>! {
  return unsafe doublebytesized(p, size)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(copy p)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_bytesized(_ p: RawSpan) -> MutableRawSpan {
  // expected-stable-error@+3{{missing argument for parameter #2 in call}}
  // expected-stable-error@+2{{cannot convert value of type 'RawSpan' to expected argument type 'CInt' (aka 'Int32')}}
  // expected-stable-error@+1{{cannot convert return expression of type 'UnsafeMutablePointer<UInt8>?' to return type 'MutableRawSpan'}}
  return bytesized(p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(copy p)
@_lifetime(p: copy p)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_charsized(_ p: inout MutableRawSpan) -> MutableRawSpan {
  // expected-stable-error@+2{{missing argument for parameter #2 in call}}
  // expected-stable-error@+1{{cannot convert return expression of type 'UnsafeMutablePointer<CChar>?' (aka 'Optional<UnsafeMutablePointer<Int8>>') to return type 'MutableRawSpan'}}
  return charsized(&p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(copy p)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_complexExpr(_ len: CInt, _ offset: CInt, _ p: RawSpan) -> RawSpan {
  // expected-stable-error@+3{{missing argument for parameter #4 in call}}
  // expected-stable-error@+2{{cannot convert value of type 'RawSpan' to expected argument type 'CInt' (aka 'Int32')}}
  // expected-stable-error@+1{{cannot convert return expression of type 'UnsafeRawPointer?' to return type 'RawSpan'}}
  return complexExpr(len, offset, p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(copy p)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_nonnull(_ len: CInt, _ p: RawSpan) -> RawSpan {
  // expected-stable-error@+3{{missing argument for parameter #3 in call}}
  // expected-stable-error@+2{{cannot convert value of type 'RawSpan' to expected argument type 'CInt' (aka 'Int32')}}
  // expected-stable-error@+1{{cannot convert return expression of type 'UnsafeRawPointer' to return type 'RawSpan'}}
  return nonnull(len, p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(borrow p)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_nonsizedLifetime(_ len: CInt, _ p: UnsafeRawPointer!) -> RawSpan {
  // expected-stable-error@+1{{cannot convert return expression of type 'UnsafeRawPointer?' to return type 'RawSpan'}}
  return unsafe nonsizedLifetime(len, p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(copy p)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_nullUnspecified(_ len: CInt, _ p: RawSpan) -> RawSpan {
  // expected-stable-error@+3{{missing argument for parameter #3 in call}}
  // expected-stable-error@+2{{cannot convert value of type 'RawSpan' to expected argument type 'CInt' (aka 'Int32')}}
  // expected-stable-error@+1{{cannot convert return expression of type 'UnsafeRawPointer?' to return type 'RawSpan'}}
  return nullUnspecified(len, p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(copy p)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_nullable(_ len: CInt, _ p: RawSpan) -> RawSpan {
  // expected-stable-error@+3{{missing argument for parameter #3 in call}}
  // expected-stable-error@+2{{cannot convert value of type 'RawSpan' to expected argument type 'CInt' (aka 'Int32')}}
  // expected-stable-error@+1{{cannot convert return expression of type 'UnsafeRawPointer?' to return type 'RawSpan'}}
  return nullable(len, p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(copy p)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_opaque(_ len: CInt, _ p: RawSpan) -> RawSpan {
  // expected-stable-error@+3{{missing argument for parameter #3 in call}}
  // expected-stable-error@+2{{cannot convert value of type 'RawSpan' to expected argument type 'CInt' (aka 'Int32')}}
  // expected-stable-error@+1{{cannot convert return expression of type 'OpaquePointer?' to return type 'RawSpan'}}
  return opaque(len, p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(copy p)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_shared(_ p: RawSpan) -> RawSpan {
  // expected-stable-error@+3{{missing argument for parameter #2 in call}}
  // expected-stable-error@+2{{cannot convert value of type 'RawSpan' to expected argument type 'CInt' (aka 'Int32')}}
  // expected-stable-error@+1{{cannot convert return expression of type 'UnsafeRawPointer?' to return type 'RawSpan'}}
  return shared(p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(copy p)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_simple(_ len: CInt, _ p: RawSpan) -> RawSpan {
  // expected-stable-error@+3{{missing argument for parameter #3 in call}}
  // expected-stable-error@+2{{cannot convert value of type 'RawSpan' to expected argument type 'CInt' (aka 'Int32')}}
  // expected-stable-error@+1{{cannot convert return expression of type 'UnsafeRawPointer?' to return type 'RawSpan'}}
  return simple(len, p)
}
