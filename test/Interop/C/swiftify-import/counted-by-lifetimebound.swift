// REQUIRES: swift_feature_SafeInteropWrappers
// REQUIRES: swift_feature_Lifetimes

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -I %t -enable-experimental-feature SafeInteropWrappers -enable-experimental-feature Lifetimes -strict-memory-safety -Xcc -Wno-nullability-completeness \
// RUN:   %t/test.swift -verify -verify-additional-file %t%{fs-sep}test.h -Rmacro-expansions -suppress-notes -verify-additional-prefix experimental-

// lifetimebound support is not stabilized yet. Don't generate _any_ overloads on functions with lifetimebound to prevent future sourcebreak.
// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -I %t -enable-experimental-feature Lifetimes -strict-memory-safety -Xcc -Wno-nullability-completeness \
// RUN:   %t/test.swift -verify -verify-additional-file %t%{fs-sep}test.h -Rmacro-expansions -suppress-notes -verify-additional-prefix stable-

// Check that ClangImporter correctly infers and expands @_SwiftifyImport macros for functions with __counted_by __lifetimebound parameters and return values.

//--- test.h
#pragma once

#define __counted_by(x) __attribute__((__counted_by__(x)))
#define __counted_by_or_null(x) __attribute__((__counted_by_or_null__(x)))
#define __lifetimebound __attribute__((lifetimebound))

// expected-experimental-expansion@+13:58{{
//   expected-experimental-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-experimental-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_lifetime(p: copy p) @_disfavoredOverload public func simple(_ len: Int32, _ p: inout MutableSpan<Int32>) -> MutableSpan<Int32> {|}}
//   expected-experimental-remark@3{{macro content: |    let len2 = Int32(exactly: p.count)!|}}
//   expected-experimental-remark@4{{macro content: |    let _pPtr = p.withUnsafeMutableBufferPointer {|}}
//   expected-experimental-remark@5{{macro content: |        unsafe $0|}}
//   expected-experimental-remark@6{{macro content: |    }|}}
//   expected-experimental-remark@7{{macro content: |    defer {|}}
//   expected-experimental-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-experimental-remark@9{{macro content: |    }|}}
//   expected-experimental-remark@10{{macro content: |    return unsafe _swiftifyOverrideLifetime(MutableSpan<Int32>(_unsafeStart: unsafe simple(len, len2, _pPtr.baseAddress!), count: Int(len)), copying: ())|}}
//   expected-experimental-remark@11{{macro content: |}|}}
// }}
int * __counted_by(len) simple(int len, int len2, int * p __counted_by(len2) __lifetimebound);

// expected-experimental-expansion@+13:48{{
//   expected-experimental-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-experimental-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_lifetime(p: copy p) @_disfavoredOverload public func shared(_ p: inout MutableSpan<Int32>) -> MutableSpan<Int32> {|}}
//   expected-experimental-remark@3{{macro content: |    let len = Int32(exactly: p.count)!|}}
//   expected-experimental-remark@4{{macro content: |    let _pPtr = p.withUnsafeMutableBufferPointer {|}}
//   expected-experimental-remark@5{{macro content: |        unsafe $0|}}
//   expected-experimental-remark@6{{macro content: |    }|}}
//   expected-experimental-remark@7{{macro content: |    defer {|}}
//   expected-experimental-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-experimental-remark@9{{macro content: |    }|}}
//   expected-experimental-remark@10{{macro content: |    return unsafe _swiftifyOverrideLifetime(MutableSpan<Int32>(_unsafeStart: unsafe shared(len, _pPtr.baseAddress!), count: Int(len)), copying: ())|}}
//   expected-experimental-remark@11{{macro content: |}|}}
// }}
int * __counted_by(len) shared(int len, int * p __counted_by(len) __lifetimebound);

// expected-experimental-expansion@+13:84{{
//   expected-experimental-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-experimental-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_lifetime(p: copy p) @_disfavoredOverload public func complexExpr(_ len: Int32, _ offset: Int32, _ p: inout MutableSpan<Int32>) -> MutableSpan<Int32> {|}}
//   expected-experimental-remark@3{{macro content: |    let len2 = Int32(exactly: p.count)!|}}
//   expected-experimental-remark@4{{macro content: |    let _pPtr = p.withUnsafeMutableBufferPointer {|}}
//   expected-experimental-remark@5{{macro content: |        unsafe $0|}}
//   expected-experimental-remark@6{{macro content: |    }|}}
//   expected-experimental-remark@7{{macro content: |    defer {|}}
//   expected-experimental-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-experimental-remark@9{{macro content: |    }|}}
//   expected-experimental-remark@10{{macro content: |    return unsafe _swiftifyOverrideLifetime(MutableSpan<Int32>(_unsafeStart: unsafe complexExpr(len, offset, len2, _pPtr.baseAddress!), count: Int(len - offset)), copying: ())|}}
//   expected-experimental-remark@11{{macro content: |}|}}
// }}
int * __counted_by(len - offset) complexExpr(int len, int offset, int len2, int * p __counted_by(len2) __lifetimebound);

// expected-experimental-expansion@+13:103{{
//   expected-experimental-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-experimental-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_lifetime(p: copy p) @_disfavoredOverload public func nullUnspecified(_ len: Int32, _ p: inout MutableSpan<Int32>) -> MutableSpan<Int32> {|}}
//   expected-experimental-remark@3{{macro content: |    let len2 = Int32(exactly: p.count)!|}}
//   expected-experimental-remark@4{{macro content: |    let _pPtr = p.withUnsafeMutableBufferPointer {|}}
//   expected-experimental-remark@5{{macro content: |        unsafe $0|}}
//   expected-experimental-remark@6{{macro content: |    }|}}
//   expected-experimental-remark@7{{macro content: |    defer {|}}
//   expected-experimental-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-experimental-remark@9{{macro content: |    }|}}
//   expected-experimental-remark@10{{macro content: |    return unsafe _swiftifyOverrideLifetime(MutableSpan<Int32>(_unsafeStart: unsafe nullUnspecified(len, len2, _pPtr.baseAddress!), count: Int(len)), copying: ())|}}
//   expected-experimental-remark@11{{macro content: |}|}}
// }}
int * __counted_by(len) _Null_unspecified nullUnspecified(int len, int len2, int * _Null_unspecified p __counted_by(len2) __lifetimebound);

// expected-experimental-expansion@+13:77{{
//   expected-experimental-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-experimental-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_lifetime(p: copy p) @_disfavoredOverload public func nonnull(_ len: Int32, _ p: inout MutableSpan<Int32>) -> MutableSpan<Int32> {|}}
//   expected-experimental-remark@3{{macro content: |    let len2 = Int32(exactly: p.count)!|}}
//   expected-experimental-remark@4{{macro content: |    let _pPtr = p.withUnsafeMutableBufferPointer {|}}
//   expected-experimental-remark@5{{macro content: |        unsafe $0|}}
//   expected-experimental-remark@6{{macro content: |    }|}}
//   expected-experimental-remark@7{{macro content: |    defer {|}}
//   expected-experimental-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-experimental-remark@9{{macro content: |    }|}}
//   expected-experimental-remark@10{{macro content: |    return unsafe _swiftifyOverrideLifetime(MutableSpan<Int32>(_unsafeStart: unsafe nonnull(len, len2, _pPtr.baseAddress!), count: Int(len)), copying: ())|}}
//   expected-experimental-remark@11{{macro content: |}|}}
// }}
int * __counted_by(len) _Nonnull nonnull(int len, int len2, int * _Nonnull p __counted_by(len2) __lifetimebound);

// expected-experimental-expansion@+20:80{{
//   expected-experimental-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-experimental-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_lifetime(p: copy p) @_disfavoredOverload public func nullable(_ len: Int32, _ p: inout MutableSpan<Int32>?) -> MutableSpan<Int32>? {|}}
//   expected-experimental-remark@3{{macro content: |    let len2 = Int32(exactly: p?.count ?? 0)!|}}
//   expected-experimental-remark@4{{macro content: |    let _pPtr = p?.withUnsafeMutableBufferPointer {|}}
//   expected-experimental-remark@5{{macro content: |        unsafe $0|}}
//   expected-experimental-remark@6{{macro content: |    }|}}
//   expected-experimental-remark@7{{macro content: |    defer {|}}
//   expected-experimental-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-experimental-remark@9{{macro content: |    }|}}
//   expected-experimental-remark@10{{macro content: |    return unsafe _swiftifyOverrideLifetime({ () in|}}
//   expected-experimental-remark@11{{macro content: |      let _resultValue = unsafe nullable(len, len2, _pPtr?.baseAddress)|}}
//   expected-experimental-remark@12{{macro content: |      if unsafe _resultValue == nil {|}}
//   expected-experimental-remark@13{{macro content: |        return nil|}}
//   expected-experimental-remark@14{{macro content: |      } else {|}}
//   expected-experimental-remark@15{{macro content: |        return unsafe _swiftifyOverrideLifetime(MutableSpan<Int32>(_unsafeStart: _resultValue!, count: Int(len)), copying: ())|}}
//   expected-experimental-remark@16{{macro content: |      }|}}
//   expected-experimental-remark@17{{macro content: |        }(), copying: ())|}}
//   expected-experimental-remark@18{{macro content: |}|}}
// }}
int * __counted_by(len) _Nullable nullable(int len, int len2, int * _Nullable p __counted_by(len2) __lifetimebound);

typedef struct foo opaque_t;
opaque_t * __counted_by(len) opaque(int len, int len2, opaque_t * p __counted_by(len2) __lifetimebound);

// expected-experimental-expansion@+6:60{{
//   expected-experimental-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-experimental-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(borrow p) @_disfavoredOverload public func noncountedLifetime(_ len: Int32, _ p: UnsafeMutablePointer<Int32>!) -> MutableSpan<Int32> {|}}
//   expected-experimental-remark@3{{macro content: |    return unsafe _swiftifyOverrideLifetime(MutableSpan<Int32>(_unsafeStart: unsafe noncountedLifetime(len, p), count: Int(len)), copying: ())|}}
//   expected-experimental-remark@4{{macro content: |}|}}
// }}
int * __counted_by(len) noncountedLifetime(int len, int * p __lifetimebound);

// expected-experimental-expansion@+23:60{{
//   expected-experimental-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-experimental-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_lifetime(p: copy p) @_disfavoredOverload public func constant(_ p: inout MutableSpan<Int32>?) -> MutableSpan<Int32>? {|}}
//   expected-experimental-remark@3{{macro content: |    let _pCount = p?.count ?? 0|}}
//   expected-experimental-remark@4{{macro content: |    if _pCount != 13 {|}}
//   expected-experimental-remark@5{{macro content: |      fatalError("bounds check failure in constant: expected \\(13) but got \\(_pCount)")|}}
//   expected-experimental-remark@6{{macro content: |    }|}}
//   expected-experimental-remark@7{{macro content: |    let _pPtr = p?.withUnsafeMutableBufferPointer {|}}
//   expected-experimental-remark@8{{macro content: |        unsafe $0|}}
//   expected-experimental-remark@9{{macro content: |    }|}}
//   expected-experimental-remark@10{{macro content: |    defer {|}}
//   expected-experimental-remark@11{{macro content: |        _fixLifetime(p)|}}
//   expected-experimental-remark@12{{macro content: |    }|}}
//   expected-experimental-remark@13{{macro content: |    return unsafe _swiftifyOverrideLifetime({ () in|}}
//   expected-experimental-remark@14{{macro content: |      let _resultValue = unsafe constant(_pPtr?.baseAddress)|}}
//   expected-experimental-remark@15{{macro content: |      if unsafe _resultValue == nil {|}}
//   expected-experimental-remark@16{{macro content: |        return nil|}}
//   expected-experimental-remark@17{{macro content: |      } else {|}}
//   expected-experimental-remark@18{{macro content: |        return unsafe _swiftifyOverrideLifetime(MutableSpan<Int32>(_unsafeStart: _resultValue!, count: Int(13)), copying: ())|}}
//   expected-experimental-remark@19{{macro content: |      }|}}
//   expected-experimental-remark@20{{macro content: |        }(), copying: ())|}}
//   expected-experimental-remark@21{{macro content: |}|}}
// }}
int * __counted_by(13) _Nullable constant(int * _Nullable p __counted_by_or_null(13) __lifetimebound);

struct EscapableStruct {};
// make sure __lifetimebound is ignored when return value is escapable
// expected-experimental-expansion@+7:87{{
//   expected-experimental-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-experimental-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func lifetimeboundEscapableReturn(_ p: UnsafeMutableBufferPointer<Int32>) -> EscapableStruct {|}}
//   expected-experimental-remark@3{{macro content: |    let len = Int32(exactly: p.count)!|}}
//   expected-experimental-remark@4{{macro content: |    return unsafe lifetimeboundEscapableReturn(len, p.baseAddress!)|}}
//   expected-experimental-remark@5{{macro content: |}|}}
// }}
struct EscapableStruct lifetimeboundEscapableReturn(int len, int * __counted_by(len) p __lifetimebound);

struct __attribute__((swift_attr("~Escapable"))) NonescapableStruct {};
// expected-experimental-expansion@+13:93{{
//   expected-experimental-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-experimental-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_lifetime(p: copy p) @_disfavoredOverload public func lifetimeboundNonescapableReturn(_ p: inout MutableSpan<Int32>) -> NonescapableStruct {|}}
//   expected-experimental-remark@3{{macro content: |    let len = Int32(exactly: p.count)!|}}
//   expected-experimental-remark@4{{macro content: |    let _pPtr = p.withUnsafeMutableBufferPointer {|}}
//   expected-experimental-remark@5{{macro content: |        unsafe $0|}}
//   expected-experimental-remark@6{{macro content: |    }|}}
//   expected-experimental-remark@7{{macro content: |    defer {|}}
//   expected-experimental-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-experimental-remark@9{{macro content: |    }|}}
//   expected-experimental-remark@10{{macro content: |    return unsafe _swiftifyOverrideLifetime(unsafe lifetimeboundNonescapableReturn(len, _pPtr.baseAddress!), copying: ())|}}
//   expected-experimental-remark@11{{macro content: |}|}}
// }}
struct NonescapableStruct lifetimeboundNonescapableReturn(int len, int * __counted_by(len) p __lifetimebound);

// expected-experimental-expansion@+13:150{{
//   expected-experimental-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-experimental-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p, copy s) @_lifetime(p: copy p) @_disfavoredOverload public func lifetimeboundNonescapableReturnDoubleBounds(_ p: inout MutableSpan<Int32>, _ s: NonescapableStruct) -> NonescapableStruct {|}}
//   expected-experimental-remark@3{{macro content: |    let len = Int32(exactly: p.count)!|}}
//   expected-experimental-remark@4{{macro content: |    let _pPtr = p.withUnsafeMutableBufferPointer {|}}
//   expected-experimental-remark@5{{macro content: |        unsafe $0|}}
//   expected-experimental-remark@6{{macro content: |    }|}}
//   expected-experimental-remark@7{{macro content: |    defer {|}}
//   expected-experimental-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-experimental-remark@9{{macro content: |    }|}}
//   expected-experimental-remark@10{{macro content: |    return unsafe _swiftifyOverrideLifetime(unsafe lifetimeboundNonescapableReturnDoubleBounds(len, _pPtr.baseAddress!, s), copying: ())|}}
//   expected-experimental-remark@11{{macro content: |}|}}
// }}
struct NonescapableStruct lifetimeboundNonescapableReturnDoubleBounds(int len, int * __counted_by(len) p __lifetimebound, struct NonescapableStruct s __lifetimebound);

// expected-experimental-expansion@+14:135{{
//   expected-experimental-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-experimental-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_lifetime(p: copy p) @_disfavoredOverload public func oneLifetimeboundOneEscapable(_ len: Int32, _ p: inout MutableSpan<Int32>, _ p2: UnsafeMutableBufferPointer<Int32>) -> MutableSpan<Int32> {|}}
//   expected-experimental-remark@3{{macro content: |    let len2 = Int32(exactly: p.count)!|}}
//   expected-experimental-remark@4{{macro content: |    let len3 = Int32(exactly: p2.count)!|}}
//   expected-experimental-remark@5{{macro content: |    let _pPtr = p.withUnsafeMutableBufferPointer {|}}
//   expected-experimental-remark@6{{macro content: |        unsafe $0|}}
//   expected-experimental-remark@7{{macro content: |    }|}}
//   expected-experimental-remark@8{{macro content: |    defer {|}}
//   expected-experimental-remark@9{{macro content: |        _fixLifetime(p)|}}
//   expected-experimental-remark@10{{macro content: |    }|}}
//   expected-experimental-remark@11{{macro content: |    return unsafe _swiftifyOverrideLifetime(MutableSpan<Int32>(_unsafeStart: unsafe oneLifetimeboundOneEscapable(len, len2, _pPtr.baseAddress!, len3, p2.baseAddress!), count: Int(len)), copying: ())|}}
//   expected-experimental-remark@12{{macro content: |}|}}
// }}
int * __counted_by(len) oneLifetimeboundOneEscapable(int len, int len2, int * p __counted_by(len2) __lifetimebound, int len3, int * p2 __counted_by(len3));

//--- module.modulemap
module Test {
  header "test.h"
}

//--- test.swift
// GENERATED-BY: %target-swift-ide-test -print-module -module-to-print=Test -plugin-path %swift-plugin-dir -I %t -source-filename=x -enable-experimental-feature SafeInteropWrappers -Xcc -Wno-nullability-completeness > %t/Test-interface.swift && %swift-function-caller-generator Test %t/Test-interface.swift
// GENERATED-HASH: 4add983f4582598af9fb1ebe8326aa2d92603cb7366ecb416251b44680eb1fbb
import Test

func call_simple(_ len: Int32, _ len2: Int32, _ p: UnsafeMutablePointer<Int32>!) -> UnsafeMutablePointer<Int32>! {
  return unsafe simple(len, len2, p)
}

func call_shared(_ len: Int32, _ p: UnsafeMutablePointer<Int32>!) -> UnsafeMutablePointer<Int32>! {
  return unsafe shared(len, p)
}

func call_complexExpr(_ len: Int32, _ offset: Int32, _ len2: Int32, _ p: UnsafeMutablePointer<Int32>!) -> UnsafeMutablePointer<Int32>! {
  return unsafe complexExpr(len, offset, len2, p)
}

func call_nullUnspecified(_ len: Int32, _ len2: Int32, _ p: UnsafeMutablePointer<Int32>!) -> UnsafeMutablePointer<Int32>! {
  return unsafe nullUnspecified(len, len2, p)
}

func call_nonnull(_ len: Int32, _ len2: Int32, _ p: UnsafeMutablePointer<Int32>) -> UnsafeMutablePointer<Int32> {
  return unsafe nonnull(len, len2, p)
}

func call_nullable(_ len: Int32, _ len2: Int32, _ p: UnsafeMutablePointer<Int32>?) -> UnsafeMutablePointer<Int32>? {
  return unsafe nullable(len, len2, p)
}

func call_opaque(_ len: Int32, _ len2: Int32, _ p: OpaquePointer!) -> OpaquePointer! {
  return unsafe opaque(len, len2, p)
}

func call_noncountedLifetime(_ len: Int32, _ p: UnsafeMutablePointer<Int32>!) -> UnsafeMutablePointer<Int32>! {
  return unsafe noncountedLifetime(len, p)
}

func call_constant(_ p: UnsafeMutablePointer<Int32>?) -> UnsafeMutablePointer<Int32>? {
  return unsafe constant(p)
}

func call_lifetimeboundEscapableReturn(_ len: Int32, _ p: UnsafeMutablePointer<Int32>!) -> EscapableStruct {
  return unsafe lifetimeboundEscapableReturn(len, p)
}

// expected-error@+1{{a function with a ~Escapable result requires '@_lifetime(...)'}}
func call_lifetimeboundNonescapableReturn(_ len: Int32, _ p: UnsafeMutablePointer<Int32>!) -> NonescapableStruct {
  return unsafe lifetimeboundNonescapableReturn(len, p)
}

func call_lifetimeboundNonescapableReturnDoubleBounds(_ len: Int32, _ p: UnsafeMutablePointer<Int32>!, _ s: NonescapableStruct) -> NonescapableStruct {
  return unsafe lifetimeboundNonescapableReturnDoubleBounds(len, p, s)
}

func call_oneLifetimeboundOneEscapable(_ len: Int32, _ len2: Int32, _ p: UnsafeMutablePointer<Int32>!, _ len3: Int32, _ p2: UnsafeMutablePointer<Int32>!) -> UnsafeMutablePointer<Int32>! {
  return unsafe oneLifetimeboundOneEscapable(len, len2, p, len3, p2)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(copy p)
@_lifetime(p: copy p)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_complexExpr(_ len: Int32, _ offset: Int32, _ p: inout MutableSpan<Int32>) -> MutableSpan<Int32> {
  // expected-stable-error@+3{{missing argument for parameter #4 in call}}
  // expected-stable-error@+2{{cannot convert value of type 'MutableSpan<Int32>' to expected argument type 'Int32'}}
  // expected-stable-error@+1{{cannot convert return expression of type 'UnsafeMutablePointer<Int32>?' to return type 'MutableSpan<Int32>'}}
  return complexExpr(len, offset, &p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(copy p)
@_lifetime(p: copy p)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_constant(_ p: inout MutableSpan<Int32>?) -> MutableSpan<Int32>? {
  // expected-stable-error@+2{{cannot convert value of type 'UnsafeMutablePointer<MutableSpan<Int32>?>' to expected argument type 'UnsafeMutablePointer<Int32>'}}
  // expected-stable-error@+1{{cannot convert return expression of type 'UnsafeMutablePointer<Int32>?' to return type 'MutableSpan<Int32>?'}}
  return constant(&p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_lifetimeboundEscapableReturn(_ p: UnsafeMutableBufferPointer<Int32>) -> EscapableStruct {
  // expected-stable-error@+2{{missing argument for parameter #2 in call}}
  // expected-stable-error@+1{{cannot convert value of type 'UnsafeMutableBufferPointer<Int32>' to expected argument type 'Int32'}}
  return unsafe lifetimeboundEscapableReturn(p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(copy p)
@_lifetime(p: copy p)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_lifetimeboundNonescapableReturn(_ p: inout MutableSpan<Int32>) -> NonescapableStruct {
  // expected-stable-error@+3{{missing argument for parameter #2 in call}}
  // expected-stable-error@+2{{cannot convert value of type 'UnsafeMutablePointer<MutableSpan<Int32>>' to expected argument type 'UnsafeMutablePointer<Int32>'}}
  // expected-stable-error@+1{{cannot convert value of type 'MutableSpan<Int32>' to expected argument type 'Int32'}}
  return lifetimeboundNonescapableReturn(&p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(copy p, copy s)
@_lifetime(p: copy p)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_lifetimeboundNonescapableReturnDoubleBounds(_ p: inout MutableSpan<Int32>, _ s: NonescapableStruct) -> NonescapableStruct {
  // expected-stable-error@+3{{missing argument for parameter #3 in call}}
  // expected-stable-error@+2{{cannot convert value of type 'NonescapableStruct' to expected argument type 'UnsafeMutablePointer<Int32>'}}
  // expected-stable-error@+1{{cannot convert value of type 'MutableSpan<Int32>' to expected argument type 'Int32'}}
  return lifetimeboundNonescapableReturnDoubleBounds(&p, s)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(borrow p)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_noncountedLifetime(_ len: Int32, _ p: UnsafeMutablePointer<Int32>!) -> MutableSpan<Int32> {
  // expected-stable-error@+1{{cannot convert return expression of type 'UnsafeMutablePointer<Int32>?' to return type 'MutableSpan<Int32>'}}
  return unsafe noncountedLifetime(len, p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(copy p)
@_lifetime(p: copy p)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_nonnull(_ len: Int32, _ p: inout MutableSpan<Int32>) -> MutableSpan<Int32> {
  // expected-stable-error@+3{{missing argument for parameter #3 in call}}
  // expected-stable-error@+2{{cannot convert value of type 'MutableSpan<Int32>' to expected argument type 'Int32'}}
  // expected-stable-error@+1{{cannot convert return expression of type 'UnsafeMutablePointer<Int32>' to return type 'MutableSpan<Int32>'}}
  return nonnull(len, &p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(copy p)
@_lifetime(p: copy p)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_nullUnspecified(_ len: Int32, _ p: inout MutableSpan<Int32>) -> MutableSpan<Int32> {
  // expected-stable-error@+3{{missing argument for parameter #3 in call}}
  // expected-stable-error@+2{{cannot convert value of type 'MutableSpan<Int32>' to expected argument type 'Int32'}}
  // expected-stable-error@+1{{cannot convert return expression of type 'UnsafeMutablePointer<Int32>?' to return type 'MutableSpan<Int32>'}}
  return nullUnspecified(len, &p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(copy p)
@_lifetime(p: copy p)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_nullable(_ len: Int32, _ p: inout MutableSpan<Int32>?) -> MutableSpan<Int32>? {
  // expected-stable-error@+3{{missing argument for parameter #3 in call}}
  // expected-stable-error@+2{{cannot convert value of type 'MutableSpan<Int32>?' to expected argument type 'Int32'}}
  // expected-stable-error@+1{{cannot convert return expression of type 'UnsafeMutablePointer<Int32>?' to return type 'MutableSpan<Int32>?'}}
  return nullable(len, &p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(copy p)
@_lifetime(p: copy p)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_oneLifetimeboundOneEscapable(_ len: Int32, _ p: inout MutableSpan<Int32>, _ p2: UnsafeMutableBufferPointer<Int32>) -> MutableSpan<Int32> {
  // expected-stable-error@+4{{cannot convert value of type 'UnsafeMutableBufferPointer<Int32>' to expected argument type 'UnsafeMutablePointer<Int32>'}}
  // expected-stable-error@+3{{cannot convert value of type 'MutableSpan<Int32>' to expected argument type 'Int32'}}
  // expected-stable-error@+2{{missing arguments for parameters #4, #5 in call}}
  // expected-stable-error@+1{{cannot convert return expression of type 'UnsafeMutablePointer<Int32>?' to return type 'MutableSpan<Int32>'}}
  return unsafe oneLifetimeboundOneEscapable(len, &p, p2)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(copy p)
@_lifetime(p: copy p)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_shared(_ p: inout MutableSpan<Int32>) -> MutableSpan<Int32> {
  // expected-stable-error@+4{{missing argument for parameter #2 in call}}
  // expected-stable-error@+3{{cannot convert value of type 'UnsafeMutablePointer<MutableSpan<Int32>>' to expected argument type 'UnsafeMutablePointer<Int32>'}}
  // expected-stable-error@+2{{cannot convert value of type 'MutableSpan<Int32>' to expected argument type 'Int32'}}
  // expected-stable-error@+1{{cannot convert return expression of type 'UnsafeMutablePointer<Int32>?' to return type 'MutableSpan<Int32>'}}
  return shared(&p)
}

@available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
@_lifetime(copy p)
@_lifetime(p: copy p)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_simple(_ len: Int32, _ p: inout MutableSpan<Int32>) -> MutableSpan<Int32> {
  // expected-stable-error@+3{{missing argument for parameter #3 in call}}
  // expected-stable-error@+2{{cannot convert value of type 'MutableSpan<Int32>' to expected argument type 'Int32'}}
  // expected-stable-error@+1{{cannot convert return expression of type 'UnsafeMutablePointer<Int32>?' to return type 'MutableSpan<Int32>'}}
  return simple(len, &p)
}
