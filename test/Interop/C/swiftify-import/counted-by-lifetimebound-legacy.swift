// REQUIRES: swift_feature_SafeInteropWrappers
// REQUIRES: swift_feature_Lifetimes

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// Mirrors counted-by-lifetimebound.swift but exercises the legacy opt-in
// wrapper shape: without SafeInteropWrappersNullAsEmptySpan, normal Optional
// pointer parameters / return values propagate as Optional in the wrapper.
// IUO (`_Null_unspecified`) parameters/returns still follow the modern
// wrapper convention regardless of the flag.
// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -I %t -enable-experimental-feature SafeInteropWrappers -enable-experimental-feature Lifetimes -strict-memory-safety -Xcc -Wno-nullability-completeness \
// RUN:   %t/test.swift -verify -verify-additional-file %t%{fs-sep}test.h -Rmacro-expansions -suppress-notes -verify-additional-prefix experimental- -eager-macro-checking

// lifetimebound support is not stabilized yet. Don't generate _any_ overloads on functions with lifetimebound to prevent future sourcebreak.
// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -I %t -enable-experimental-feature Lifetimes -strict-memory-safety -Xcc -Wno-nullability-completeness \
// RUN:   %t/test.swift -verify -verify-additional-file %t%{fs-sep}test.h -Rmacro-expansions -suppress-notes -verify-additional-prefix stable- -eager-macro-checking

// Check that ClangImporter correctly infers and expands @_SwiftifyImport macros for functions with __counted_by __lifetimebound parameters and return values.

//--- test.h
#pragma once

#define __counted_by(x) __attribute__((__counted_by__(x)))
#define __counted_by_or_null(x) __attribute__((__counted_by_or_null__(x)))
#define __lifetimebound __attribute__((lifetimebound))

// expected-experimental-expansion@+18:58{{
//   expected-experimental-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-experimental-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_lifetime(p: copy p) @_disfavoredOverload public func simple(_ len: CInt, _ p: inout MutableSpan<CInt>) -> MutableSpan<CInt> {|}}
//   expected-experimental-remark@3{{macro content: |    let len2 = CInt(exactly: p.count)!|}}
//   expected-experimental-remark@4{{macro content: |    let _pPtr = p.withUnsafeMutableBufferPointer {|}}
//   expected-experimental-remark@5{{macro content: |        unsafe $0|}}
//   expected-experimental-remark@6{{macro content: |    }|}}
//   expected-experimental-remark@7{{macro content: |    defer {|}}
//   expected-experimental-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-experimental-remark@9{{macro content: |    }|}}
//   expected-experimental-remark@10{{macro content: |    let _resultValue: UnsafeMutablePointer<CInt>? = unsafe simple(len, len2, _pPtr.baseAddress)|}}
//   expected-experimental-remark@11{{macro content: |    if unsafe _resultValue == nil {|}}
//   expected-experimental-remark@12{{macro content: |      precondition(len == 0, "counted_by may only be null if count is 0 (unlike counted_by_or_null)")|}}
//   expected-experimental-remark@13{{macro content: |      return MutableSpan<CInt>()|}}
//   expected-experimental-remark@14{{macro content: |    }|}}
//   expected-experimental-remark@15{{macro content: |    return unsafe _swiftifyOverrideLifetime(MutableSpan<CInt>(_unsafeStart: _resultValue!, count: Int(len)), copying: ())|}}
//   expected-experimental-remark@16{{macro content: |}|}}
// }}
int * __counted_by(len) simple(int len, int len2, int * p __counted_by(len2) __lifetimebound);

// expected-experimental-expansion@+18:48{{
//   expected-experimental-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-experimental-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_lifetime(p: copy p) @_disfavoredOverload public func shared(_ p: inout MutableSpan<CInt>) -> MutableSpan<CInt> {|}}
//   expected-experimental-remark@3{{macro content: |    let len = CInt(exactly: p.count)!|}}
//   expected-experimental-remark@4{{macro content: |    let _pPtr = p.withUnsafeMutableBufferPointer {|}}
//   expected-experimental-remark@5{{macro content: |        unsafe $0|}}
//   expected-experimental-remark@6{{macro content: |    }|}}
//   expected-experimental-remark@7{{macro content: |    defer {|}}
//   expected-experimental-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-experimental-remark@9{{macro content: |    }|}}
//   expected-experimental-remark@10{{macro content: |    let _resultValue: UnsafeMutablePointer<CInt>? = unsafe shared(len, _pPtr.baseAddress)|}}
//   expected-experimental-remark@11{{macro content: |    if unsafe _resultValue == nil {|}}
//   expected-experimental-remark@12{{macro content: |      precondition(len == 0, "counted_by may only be null if count is 0 (unlike counted_by_or_null)")|}}
//   expected-experimental-remark@13{{macro content: |      return MutableSpan<CInt>()|}}
//   expected-experimental-remark@14{{macro content: |    }|}}
//   expected-experimental-remark@15{{macro content: |    return unsafe _swiftifyOverrideLifetime(MutableSpan<CInt>(_unsafeStart: _resultValue!, count: Int(len)), copying: ())|}}
//   expected-experimental-remark@16{{macro content: |}|}}
// }}
int * __counted_by(len) shared(int len, int * p __counted_by(len) __lifetimebound);

// expected-experimental-expansion@+18:84{{
//   expected-experimental-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-experimental-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_lifetime(p: copy p) @_disfavoredOverload public func complexExpr(_ len: CInt, _ offset: CInt, _ p: inout MutableSpan<CInt>) -> MutableSpan<CInt> {|}}
//   expected-experimental-remark@3{{macro content: |    let len2 = CInt(exactly: p.count)!|}}
//   expected-experimental-remark@4{{macro content: |    let _pPtr = p.withUnsafeMutableBufferPointer {|}}
//   expected-experimental-remark@5{{macro content: |        unsafe $0|}}
//   expected-experimental-remark@6{{macro content: |    }|}}
//   expected-experimental-remark@7{{macro content: |    defer {|}}
//   expected-experimental-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-experimental-remark@9{{macro content: |    }|}}
//   expected-experimental-remark@10{{macro content: |    let _resultValue: UnsafeMutablePointer<CInt>? = unsafe complexExpr(len, offset, len2, _pPtr.baseAddress)|}}
//   expected-experimental-remark@11{{macro content: |    if unsafe _resultValue == nil {|}}
//   expected-experimental-remark@12{{macro content: |      precondition((len - offset) == 0, "counted_by may only be null if count is 0 (unlike counted_by_or_null)")|}}
//   expected-experimental-remark@13{{macro content: |      return MutableSpan<CInt>()|}}
//   expected-experimental-remark@14{{macro content: |    }|}}
//   expected-experimental-remark@15{{macro content: |    return unsafe _swiftifyOverrideLifetime(MutableSpan<CInt>(_unsafeStart: _resultValue!, count: Int((len - offset))), copying: ())|}}
//   expected-experimental-remark@16{{macro content: |}|}}
// }}
int * __counted_by(len - offset) complexExpr(int len, int offset, int len2, int * p __counted_by(len2) __lifetimebound);

// expected-experimental-expansion@+18:103{{
//   expected-experimental-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-experimental-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_lifetime(p: copy p) @_disfavoredOverload public func nullUnspecified(_ len: CInt, _ p: inout MutableSpan<CInt>) -> MutableSpan<CInt> {|}}
//   expected-experimental-remark@3{{macro content: |    let len2 = CInt(exactly: p.count)!|}}
//   expected-experimental-remark@4{{macro content: |    let _pPtr = p.withUnsafeMutableBufferPointer {|}}
//   expected-experimental-remark@5{{macro content: |        unsafe $0|}}
//   expected-experimental-remark@6{{macro content: |    }|}}
//   expected-experimental-remark@7{{macro content: |    defer {|}}
//   expected-experimental-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-experimental-remark@9{{macro content: |    }|}}
//   expected-experimental-remark@10{{macro content: |    let _resultValue: UnsafeMutablePointer<CInt>? = unsafe nullUnspecified(len, len2, _pPtr.baseAddress)|}}
//   expected-experimental-remark@11{{macro content: |    if unsafe _resultValue == nil {|}}
//   expected-experimental-remark@12{{macro content: |      precondition(len == 0, "counted_by may only be null if count is 0 (unlike counted_by_or_null)")|}}
//   expected-experimental-remark@13{{macro content: |      return MutableSpan<CInt>()|}}
//   expected-experimental-remark@14{{macro content: |    }|}}
//   expected-experimental-remark@15{{macro content: |    return unsafe _swiftifyOverrideLifetime(MutableSpan<CInt>(_unsafeStart: _resultValue!, count: Int(len)), copying: ())|}}
//   expected-experimental-remark@16{{macro content: |}|}}
// }}
int * __counted_by(len) _Null_unspecified nullUnspecified(int len, int len2, int * _Null_unspecified p __counted_by(len2) __lifetimebound);

// expected-experimental-expansion@+13:77{{
//   expected-experimental-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-experimental-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_lifetime(p: copy p) @_disfavoredOverload public func nonnull(_ len: CInt, _ p: inout MutableSpan<CInt>) -> MutableSpan<CInt> {|}}
//   expected-experimental-remark@3{{macro content: |    let len2 = CInt(exactly: p.count)!|}}
//   expected-experimental-remark@4{{macro content: |    let _pPtr = p.withUnsafeMutableBufferPointer {|}}
//   expected-experimental-remark@5{{macro content: |        unsafe $0|}}
//   expected-experimental-remark@6{{macro content: |    }|}}
//   expected-experimental-remark@7{{macro content: |    defer {|}}
//   expected-experimental-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-experimental-remark@9{{macro content: |    }|}}
//   expected-experimental-remark@10{{macro content: |    return unsafe _swiftifyOverrideLifetime(MutableSpan<CInt>(_unsafeStart: unsafe nonnull(len, len2, _pPtr.baseAddress!), count: Int(len)), copying: ())|}}
//   expected-experimental-remark@11{{macro content: |}|}}
// }}
int * __counted_by(len) _Nonnull nonnull(int len, int len2, int * _Nonnull p __counted_by(len2) __lifetimebound);

// expected-experimental-expansion@+17:80{{
//   expected-experimental-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-experimental-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_lifetime(p: copy p) @_disfavoredOverload public func nullable(_ len: CInt, _ p: inout MutableSpan<CInt>?) -> MutableSpan<CInt>? {|}}
//   expected-experimental-remark@3{{macro content: |    let len2 = CInt(exactly: p?.count ?? 0)!|}}
//   expected-experimental-remark@4{{macro content: |    let _pPtr = p?.withUnsafeMutableBufferPointer {|}}
//   expected-experimental-remark@5{{macro content: |        unsafe $0|}}
//   expected-experimental-remark@6{{macro content: |    }|}}
//   expected-experimental-remark@7{{macro content: |    defer {|}}
//   expected-experimental-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-experimental-remark@9{{macro content: |    }|}}
//   expected-experimental-remark@10{{macro content: |    let _resultValue = unsafe nullable(len, len2, _pPtr?.baseAddress)|}}
//   expected-experimental-remark@11{{macro content: |    if unsafe _resultValue == nil {|}}
//   expected-experimental-remark@12{{macro content: |      return nil|}}
//   expected-experimental-remark@13{{macro content: |    }|}}
//   expected-experimental-remark@14{{macro content: |    return unsafe _swiftifyOverrideLifetime(MutableSpan<CInt>(_unsafeStart: _resultValue!, count: Int(len)), copying: ())|}}
//   expected-experimental-remark@15{{macro content: |}|}}
// }}
int * __counted_by(len) _Nullable nullable(int len, int len2, int * _Nullable p __counted_by(len2) __lifetimebound);

typedef struct foo opaque_t;
opaque_t * __counted_by(len) opaque(int len, int len2, opaque_t * p __counted_by(len2) __lifetimebound);

// expected-experimental-expansion@+11:60{{
//   expected-experimental-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-experimental-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(borrow p) @_disfavoredOverload public func noncountedLifetime(_ len: CInt, _ p: UnsafeMutablePointer<CInt>!) -> MutableSpan<CInt> {|}}
//   expected-experimental-remark@3{{macro content: |    let _resultValue: UnsafeMutablePointer<CInt>? = unsafe noncountedLifetime(len, p)|}}
//   expected-experimental-remark@4{{macro content: |    if unsafe _resultValue == nil {|}}
//   expected-experimental-remark@5{{macro content: |      precondition(len == 0, "counted_by may only be null if count is 0 (unlike counted_by_or_null)")|}}
//   expected-experimental-remark@6{{macro content: |      return MutableSpan<CInt>()|}}
//   expected-experimental-remark@7{{macro content: |    }|}}
//   expected-experimental-remark@8{{macro content: |    return unsafe _swiftifyOverrideLifetime(MutableSpan<CInt>(_unsafeStart: _resultValue!, count: Int(len)), copying: ())|}}
//   expected-experimental-remark@9{{macro content: |}|}}
// }}
int * __counted_by(len) noncountedLifetime(int len, int * p __lifetimebound);

// expected-experimental-expansion@+19:60{{
//   expected-experimental-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-experimental-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_lifetime(p: copy p) @_disfavoredOverload public func constant(_ p: inout MutableSpan<CInt>?) -> MutableSpan<CInt>? {|}}
//   expected-experimental-remark@3{{macro content: |    if let _pCount = p?.count, _pCount != CInt(13) {|}}
//   expected-experimental-remark@4{{macro content: |      fatalError("bounds check failure in constant: expected \\(CInt(13)) but got \\(_pCount)")|}}
//   expected-experimental-remark@5{{macro content: |    }|}}
//   expected-experimental-remark@6{{macro content: |    let _pPtr = p?.withUnsafeMutableBufferPointer {|}}
//   expected-experimental-remark@7{{macro content: |        unsafe $0|}}
//   expected-experimental-remark@8{{macro content: |    }|}}
//   expected-experimental-remark@9{{macro content: |    defer {|}}
//   expected-experimental-remark@10{{macro content: |        _fixLifetime(p)|}}
//   expected-experimental-remark@11{{macro content: |    }|}}
//   expected-experimental-remark@12{{macro content: |    let _resultValue = unsafe constant(_pPtr?.baseAddress)|}}
//   expected-experimental-remark@13{{macro content: |    if unsafe _resultValue == nil {|}}
//   expected-experimental-remark@14{{macro content: |      return nil|}}
//   expected-experimental-remark@15{{macro content: |    }|}}
//   expected-experimental-remark@16{{macro content: |    return unsafe _swiftifyOverrideLifetime(MutableSpan<CInt>(_unsafeStart: _resultValue!, count: Int(CInt(13))), copying: ())|}}
//   expected-experimental-remark@17{{macro content: |}|}}
// }}
int * __counted_by(13) _Nullable constant(int * _Nullable p __counted_by_or_null(13) __lifetimebound);

struct EscapableStruct {};
// make sure __lifetimebound is ignored when return value is escapable
// expected-experimental-expansion@+7:87{{
//   expected-experimental-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-experimental-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func lifetimeboundEscapableReturn(_ p: UnsafeMutableBufferPointer<CInt>) -> EscapableStruct {|}}
//   expected-experimental-remark@3{{macro content: |    let len = CInt(exactly: p.count)!|}}
//   expected-experimental-remark@4{{macro content: |    return unsafe lifetimeboundEscapableReturn(len, p.baseAddress)|}}
//   expected-experimental-remark@5{{macro content: |}|}}
// }}
struct EscapableStruct lifetimeboundEscapableReturn(int len, int * __counted_by(len) p __lifetimebound);

struct __attribute__((swift_attr("~Escapable"))) NonescapableStruct {};
// expected-experimental-expansion@+13:93{{
//   expected-experimental-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-experimental-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_lifetime(p: copy p) @_disfavoredOverload public func lifetimeboundNonescapableReturn(_ p: inout MutableSpan<CInt>) -> NonescapableStruct {|}}
//   expected-experimental-remark@3{{macro content: |    let len = CInt(exactly: p.count)!|}}
//   expected-experimental-remark@4{{macro content: |    let _pPtr = p.withUnsafeMutableBufferPointer {|}}
//   expected-experimental-remark@5{{macro content: |        unsafe $0|}}
//   expected-experimental-remark@6{{macro content: |    }|}}
//   expected-experimental-remark@7{{macro content: |    defer {|}}
//   expected-experimental-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-experimental-remark@9{{macro content: |    }|}}
//   expected-experimental-remark@10{{macro content: |    return unsafe _swiftifyOverrideLifetime(unsafe lifetimeboundNonescapableReturn(len, _pPtr.baseAddress), copying: ())|}}
//   expected-experimental-remark@11{{macro content: |}|}}
// }}
struct NonescapableStruct lifetimeboundNonescapableReturn(int len, int * __counted_by(len) p __lifetimebound);

// expected-experimental-expansion@+13:150{{
//   expected-experimental-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-experimental-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p, copy s) @_lifetime(p: copy p) @_disfavoredOverload public func lifetimeboundNonescapableReturnDoubleBounds(_ p: inout MutableSpan<CInt>, _ s: NonescapableStruct) -> NonescapableStruct {|}}
//   expected-experimental-remark@3{{macro content: |    let len = CInt(exactly: p.count)!|}}
//   expected-experimental-remark@4{{macro content: |    let _pPtr = p.withUnsafeMutableBufferPointer {|}}
//   expected-experimental-remark@5{{macro content: |        unsafe $0|}}
//   expected-experimental-remark@6{{macro content: |    }|}}
//   expected-experimental-remark@7{{macro content: |    defer {|}}
//   expected-experimental-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-experimental-remark@9{{macro content: |    }|}}
//   expected-experimental-remark@10{{macro content: |    return unsafe _swiftifyOverrideLifetime(unsafe lifetimeboundNonescapableReturnDoubleBounds(len, _pPtr.baseAddress, s), copying: ())|}}
//   expected-experimental-remark@11{{macro content: |}|}}
// }}
struct NonescapableStruct lifetimeboundNonescapableReturnDoubleBounds(int len, int * __counted_by(len) p __lifetimebound, struct NonescapableStruct s __lifetimebound);

// expected-experimental-expansion@+19:135{{
//   expected-experimental-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-experimental-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_lifetime(p: copy p) @_disfavoredOverload public func oneLifetimeboundOneEscapable(_ len: CInt, _ p: inout MutableSpan<CInt>, _ p2: UnsafeMutableBufferPointer<CInt>) -> MutableSpan<CInt> {|}}
//   expected-experimental-remark@3{{macro content: |    let len2 = CInt(exactly: p.count)!|}}
//   expected-experimental-remark@4{{macro content: |    let len3 = CInt(exactly: p2.count)!|}}
//   expected-experimental-remark@5{{macro content: |    let _pPtr = p.withUnsafeMutableBufferPointer {|}}
//   expected-experimental-remark@6{{macro content: |        unsafe $0|}}
//   expected-experimental-remark@7{{macro content: |    }|}}
//   expected-experimental-remark@8{{macro content: |    defer {|}}
//   expected-experimental-remark@9{{macro content: |        _fixLifetime(p)|}}
//   expected-experimental-remark@10{{macro content: |    }|}}
//   expected-experimental-remark@11{{macro content: |    let _resultValue: UnsafeMutablePointer<CInt>? = unsafe oneLifetimeboundOneEscapable(len, len2, _pPtr.baseAddress, len3, p2.baseAddress)|}}
//   expected-experimental-remark@12{{macro content: |    if unsafe _resultValue == nil {|}}
//   expected-experimental-remark@13{{macro content: |      precondition(len == 0, "counted_by may only be null if count is 0 (unlike counted_by_or_null)")|}}
//   expected-experimental-remark@14{{macro content: |      return MutableSpan<CInt>()|}}
//   expected-experimental-remark@15{{macro content: |    }|}}
//   expected-experimental-remark@16{{macro content: |    return unsafe _swiftifyOverrideLifetime(MutableSpan<CInt>(_unsafeStart: _resultValue!, count: Int(len)), copying: ())|}}
//   expected-experimental-remark@17{{macro content: |}|}}
// }}
int * __counted_by(len) oneLifetimeboundOneEscapable(int len, int len2, int * p __counted_by(len2) __lifetimebound, int len3, int * p2 __counted_by(len3));

//--- module.modulemap
module Test {
  header "test.h"
}

//--- test.swift
// GENERATED-BY: %target-swift-ide-test -print-module -module-to-print=Test -enable-experimental-feature SafeInteropWrappers -enable-experimental-feature Lifetimes -plugin-path %swift-plugin-dir -I %t -source-filename=x -enable-experimental-feature SafeInteropWrappers -Xcc -Wno-nullability-completeness > %t/Test-interface.swift && %swift-function-caller-generator Test %t/Test-interface.swift
// GENERATED-HASH: e578470f1190d56e60e74393710cb078725172a3897488e395921c158c79e5e5
import Test

func call_simple(_ len: CInt, _ len2: CInt, _ p: UnsafeMutablePointer<CInt>!) -> UnsafeMutablePointer<CInt>! {
  return unsafe simple(len, len2, p)
}

func call_shared(_ len: CInt, _ p: UnsafeMutablePointer<CInt>!) -> UnsafeMutablePointer<CInt>! {
  return unsafe shared(len, p)
}

func call_complexExpr(_ len: CInt, _ offset: CInt, _ len2: CInt, _ p: UnsafeMutablePointer<CInt>!) -> UnsafeMutablePointer<CInt>! {
  return unsafe complexExpr(len, offset, len2, p)
}

func call_nullUnspecified(_ len: CInt, _ len2: CInt, _ p: UnsafeMutablePointer<CInt>!) -> UnsafeMutablePointer<CInt>! {
  return unsafe nullUnspecified(len, len2, p)
}

func call_nonnull(_ len: CInt, _ len2: CInt, _ p: UnsafeMutablePointer<CInt>) -> UnsafeMutablePointer<CInt> {
  return unsafe nonnull(len, len2, p)
}

func call_nullable(_ len: CInt, _ len2: CInt, _ p: UnsafeMutablePointer<CInt>?) -> UnsafeMutablePointer<CInt>? {
  return unsafe nullable(len, len2, p)
}

func call_opaque(_ len: CInt, _ len2: CInt, _ p: OpaquePointer!) -> OpaquePointer! {
  return unsafe opaque(len, len2, p)
}

func call_noncountedLifetime(_ len: CInt, _ p: UnsafeMutablePointer<CInt>!) -> UnsafeMutablePointer<CInt>! {
  return unsafe noncountedLifetime(len, p)
}

func call_constant(_ p: UnsafeMutablePointer<CInt>?) -> UnsafeMutablePointer<CInt>? {
  return unsafe constant(p)
}

func call_lifetimeboundEscapableReturn(_ len: CInt, _ p: UnsafeMutablePointer<CInt>!) -> EscapableStruct {
  return unsafe lifetimeboundEscapableReturn(len, p)
}

// expected-stable-error@+2{{a function with a ~Escapable result requires '@_lifetime(...)'}}
// expected-experimental-error@+1{{a function with a ~Escapable result requires '@_lifetime(...)'}}
func call_lifetimeboundNonescapableReturn(_ len: CInt, _ p: UnsafeMutablePointer<CInt>!) -> NonescapableStruct {
  return unsafe lifetimeboundNonescapableReturn(len, p)
}

func call_lifetimeboundNonescapableReturnDoubleBounds(_ len: CInt, _ p: UnsafeMutablePointer<CInt>!, _ s: NonescapableStruct) -> NonescapableStruct {
  return unsafe lifetimeboundNonescapableReturnDoubleBounds(len, p, s)
}

func call_oneLifetimeboundOneEscapable(_ len: CInt, _ len2: CInt, _ p: UnsafeMutablePointer<CInt>!, _ len3: CInt, _ p2: UnsafeMutablePointer<CInt>!) -> UnsafeMutablePointer<CInt>! {
  return unsafe oneLifetimeboundOneEscapable(len, len2, p, len3, p2)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(copy p)
@_lifetime(p: copy p)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_complexExpr(_ len: CInt, _ offset: CInt, _ p: inout MutableSpan<CInt>) -> MutableSpan<CInt> {
  // expected-stable-error@+3{{missing argument for parameter #4 in call}}
  // expected-stable-error@+2{{cannot convert value of type 'MutableSpan<CInt>' (aka 'MutableSpan<Int32>') to expected argument type 'CInt' (aka 'Int32')}}
  // expected-stable-error@+1{{cannot convert return expression of type 'UnsafeMutablePointer<CInt>?' (aka 'Optional<UnsafeMutablePointer<Int32>>') to return type 'MutableSpan<CInt>' (aka 'MutableSpan<Int32>')}}
  return complexExpr(len, offset, &p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(copy p)
@_lifetime(p: copy p)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_constant(_ p: inout MutableSpan<CInt>?) -> MutableSpan<CInt>? {
  // expected-stable-error@+2{{cannot convert value of type 'UnsafeMutablePointer<MutableSpan<CInt>?>' (aka 'UnsafeMutablePointer<Optional<MutableSpan<Int32>>>') to expected argument type 'UnsafeMutablePointer<CInt>' (aka 'UnsafeMutablePointer<Int32>')}}
  // expected-stable-error@+1{{cannot convert return expression of type 'UnsafeMutablePointer<CInt>?' (aka 'Optional<UnsafeMutablePointer<Int32>>') to return type 'MutableSpan<CInt>?' (aka 'Optional<MutableSpan<Int32>>')}}
  return constant(&p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_lifetimeboundEscapableReturn(_ p: UnsafeMutableBufferPointer<CInt>) -> EscapableStruct {
  // expected-stable-error@+2{{missing argument for parameter #2 in call}}
  // expected-stable-error@+1{{cannot convert value of type 'UnsafeMutableBufferPointer<CInt>' (aka 'UnsafeMutableBufferPointer<Int32>') to expected argument type 'CInt' (aka 'Int32')}}
  return unsafe lifetimeboundEscapableReturn(p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(copy p)
@_lifetime(p: copy p)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_lifetimeboundNonescapableReturn(_ p: inout MutableSpan<CInt>) -> NonescapableStruct {
  // expected-stable-error@+3{{missing argument for parameter #2 in call}}
  // expected-stable-error@+2{{cannot convert value of type 'UnsafeMutablePointer<MutableSpan<CInt>>' (aka 'UnsafeMutablePointer<MutableSpan<Int32>>') to expected argument type 'UnsafeMutablePointer<CInt>' (aka 'UnsafeMutablePointer<Int32>')}}
  // expected-stable-error@+1{{cannot convert value of type 'MutableSpan<CInt>' (aka 'MutableSpan<Int32>') to expected argument type 'CInt' (aka 'Int32')}}
  return lifetimeboundNonescapableReturn(&p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(copy p, copy s)
@_lifetime(p: copy p)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_lifetimeboundNonescapableReturnDoubleBounds(_ p: inout MutableSpan<CInt>, _ s: NonescapableStruct) -> NonescapableStruct {
  // expected-stable-error@+3{{missing argument for parameter #3 in call}}
  // expected-stable-error@+2{{cannot convert value of type 'NonescapableStruct' to expected argument type 'UnsafeMutablePointer<CInt>' (aka 'UnsafeMutablePointer<Int32>')}}
  // expected-stable-error@+1{{cannot convert value of type 'MutableSpan<CInt>' (aka 'MutableSpan<Int32>') to expected argument type 'CInt' (aka 'Int32')}}
  return lifetimeboundNonescapableReturnDoubleBounds(&p, s)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(borrow p)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_noncountedLifetime(_ len: CInt, _ p: UnsafeMutablePointer<CInt>!) -> MutableSpan<CInt> {
  // expected-stable-error@+1{{cannot convert return expression of type 'UnsafeMutablePointer<CInt>?' (aka 'Optional<UnsafeMutablePointer<Int32>>') to return type 'MutableSpan<CInt>' (aka 'MutableSpan<Int32>')}}
  return unsafe noncountedLifetime(len, p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(copy p)
@_lifetime(p: copy p)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_nonnull(_ len: CInt, _ p: inout MutableSpan<CInt>) -> MutableSpan<CInt> {
  // expected-stable-error@+3{{missing argument for parameter #3 in call}}
  // expected-stable-error@+2{{cannot convert value of type 'MutableSpan<CInt>' (aka 'MutableSpan<Int32>') to expected argument type 'CInt' (aka 'Int32')}}
  // expected-stable-error@+1{{cannot convert return expression of type 'UnsafeMutablePointer<CInt>' (aka 'UnsafeMutablePointer<Int32>') to return type 'MutableSpan<CInt>' (aka 'MutableSpan<Int32>')}}
  return nonnull(len, &p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(copy p)
@_lifetime(p: copy p)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_nullUnspecified(_ len: CInt, _ p: inout MutableSpan<CInt>) -> MutableSpan<CInt> {
  // expected-stable-error@+3{{missing argument for parameter #3 in call}}
  // expected-stable-error@+2{{cannot convert value of type 'MutableSpan<CInt>' (aka 'MutableSpan<Int32>') to expected argument type 'CInt' (aka 'Int32')}}
  // expected-stable-error@+1{{cannot convert return expression of type 'UnsafeMutablePointer<CInt>?' (aka 'Optional<UnsafeMutablePointer<Int32>>') to return type 'MutableSpan<CInt>' (aka 'MutableSpan<Int32>')}}
  return nullUnspecified(len, &p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(copy p)
@_lifetime(p: copy p)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_nullable(_ len: CInt, _ p: inout MutableSpan<CInt>?) -> MutableSpan<CInt>? {
  // expected-stable-error@+3{{missing argument for parameter #3 in call}}
  // expected-stable-error@+2{{cannot convert value of type 'MutableSpan<CInt>?' (aka 'Optional<MutableSpan<Int32>>') to expected argument type 'CInt' (aka 'Int32')}}
  // expected-stable-error@+1{{cannot convert return expression of type 'UnsafeMutablePointer<CInt>?' (aka 'Optional<UnsafeMutablePointer<Int32>>') to return type 'MutableSpan<CInt>?' (aka 'Optional<MutableSpan<Int32>>')}}
  return nullable(len, &p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(copy p)
@_lifetime(p: copy p)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_oneLifetimeboundOneEscapable(_ len: CInt, _ p: inout MutableSpan<CInt>, _ p2: UnsafeMutableBufferPointer<CInt>) -> MutableSpan<CInt> {
  // expected-stable-error@+4{{cannot convert value of type 'UnsafeMutableBufferPointer<CInt>' (aka 'UnsafeMutableBufferPointer<Int32>') to expected argument type 'UnsafeMutablePointer<CInt>' (aka 'UnsafeMutablePointer<Int32>')}}
  // expected-stable-error@+3{{cannot convert value of type 'MutableSpan<CInt>' (aka 'MutableSpan<Int32>') to expected argument type 'CInt' (aka 'Int32')}}
  // expected-stable-error@+2{{missing arguments for parameters #4, #5 in call}}
  // expected-stable-error@+1{{cannot convert return expression of type 'UnsafeMutablePointer<CInt>?' (aka 'Optional<UnsafeMutablePointer<Int32>>') to return type 'MutableSpan<CInt>' (aka 'MutableSpan<Int32>')}}
  return unsafe oneLifetimeboundOneEscapable(len, &p, p2)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(copy p)
@_lifetime(p: copy p)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_shared(_ p: inout MutableSpan<CInt>) -> MutableSpan<CInt> {
  // expected-stable-error@+4{{missing argument for parameter #2 in call}}
  // expected-stable-error@+3{{cannot convert value of type 'UnsafeMutablePointer<MutableSpan<CInt>>' (aka 'UnsafeMutablePointer<MutableSpan<Int32>>') to expected argument type 'UnsafeMutablePointer<CInt>' (aka 'UnsafeMutablePointer<Int32>')}}
  // expected-stable-error@+2{{cannot convert value of type 'MutableSpan<CInt>' (aka 'MutableSpan<Int32>') to expected argument type 'CInt' (aka 'Int32')}}
  // expected-stable-error@+1{{cannot convert return expression of type 'UnsafeMutablePointer<CInt>?' (aka 'Optional<UnsafeMutablePointer<Int32>>') to return type 'MutableSpan<CInt>' (aka 'MutableSpan<Int32>')}}
  return shared(&p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(copy p)
@_lifetime(p: copy p)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_simple(_ len: CInt, _ p: inout MutableSpan<CInt>) -> MutableSpan<CInt> {
  // expected-stable-error@+3{{missing argument for parameter #3 in call}}
  // expected-stable-error@+2{{cannot convert value of type 'MutableSpan<CInt>' (aka 'MutableSpan<Int32>') to expected argument type 'CInt' (aka 'Int32')}}
  // expected-stable-error@+1{{cannot convert return expression of type 'UnsafeMutablePointer<CInt>?' (aka 'Optional<UnsafeMutablePointer<Int32>>') to return type 'MutableSpan<CInt>' (aka 'MutableSpan<Int32>')}}
  return simple(len, &p)
}
