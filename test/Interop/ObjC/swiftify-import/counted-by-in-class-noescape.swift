// RUN: %empty-directory(%t)
// RUN: split-file %s %t --leading-lines

// RUN: %target-swift-frontend -plugin-path %swift-plugin-dir -I %t/Inputs %t/method.swift -Rmacro-expansions -emit-module \
// RUN:   -verify -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}method.h -verify-additional-prefix stable- -eager-macro-checking \
// RUN:   -Xcc -Wno-nullability-completeness -strict-memory-safety -enable-experimental-feature Lifetimes \
// RUN:   -verify-ignore-macro-note -verify-child-notes

// RUN: %target-swift-frontend -plugin-path %swift-plugin-dir -I %t/Inputs %t/method.swift -Rmacro-expansions -emit-module \
// RUN:   -verify -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}method.h -verify-additional-prefix experimental- -eager-macro-checking \
// RUN:   -Xcc -Wno-nullability-completeness -strict-memory-safety \
// RUN:   -enable-experimental-feature SafeInteropWrappers -enable-experimental-feature SafeInteropWrappersNullAsEmptySpan -enable-experimental-feature Lifetimes \
// RUN:   -verify-ignore-macro-note -verify-child-notes

// REQUIRES: swift_feature_SafeInteropWrappers
// REQUIRES: swift_feature_SafeInteropWrappersNullAsEmptySpan
// REQUIRES: swift_feature_Lifetimes

//--- Inputs/module.modulemap
module Method {
    header "method.h"
}

//--- Inputs/method.h
#pragma once

#define __counted_by(x) __attribute__((__counted_by__(x)))
#define __noescape __attribute__((noescape))
#define __lifetimebound __attribute__((lifetimebound))

@interface Foo
// expected-expansion@+26:2{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload public class final func simple(_ p: inout MutableSpan<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    let _pPtr = p.withUnsafeMutableBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe simple(len, _pPtr.baseAddress)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
// expected-expansion@+13:64{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload public final func simple(_ p: inout MutableSpan<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    let _pPtr = p.withUnsafeMutableBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe simple(len, _pPtr.baseAddress)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
- (void) simple:(int)len :(int * __counted_by(len) __noescape)p;

// expected-expansion@+44:2{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p1: copy p1) @_lifetime(p2: copy p2) @_disfavoredOverload public class final func shared(_ p1: inout MutableSpan<CInt>, _ p2: inout MutableSpan<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: p2.count)!|}}
//   expected-remark@4{{macro content: |    if p1.count != len {|}}
//   expected-remark@5{{macro content: |      fatalError("bounds check failure in shared: expected \\(len) but got \\(p1.count)")|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    let _p1Ptr = p1.withUnsafeMutableBufferPointer {|}}
//   expected-remark@8{{macro content: |        unsafe $0|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    defer {|}}
//   expected-remark@11{{macro content: |        _fixLifetime(p1)|}}
//   expected-remark@12{{macro content: |    }|}}
//   expected-remark@13{{macro content: |    let _p2Ptr = p2.withUnsafeMutableBufferPointer {|}}
//   expected-remark@14{{macro content: |        unsafe $0|}}
//   expected-remark@15{{macro content: |    }|}}
//   expected-remark@16{{macro content: |    defer {|}}
//   expected-remark@17{{macro content: |        _fixLifetime(p2)|}}
//   expected-remark@18{{macro content: |    }|}}
//   expected-remark@19{{macro content: |    return unsafe shared(len, _p1Ptr.baseAddress, _p2Ptr.baseAddress)|}}
//   expected-remark@20{{macro content: |}|}}
// }}
// expected-expansion@+22:105{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p1: copy p1) @_lifetime(p2: copy p2) @_disfavoredOverload public final func shared(_ p1: inout MutableSpan<CInt>, _ p2: inout MutableSpan<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: p2.count)!|}}
//   expected-remark@4{{macro content: |    if p1.count != len {|}}
//   expected-remark@5{{macro content: |      fatalError("bounds check failure in shared: expected \\(len) but got \\(p1.count)")|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    let _p1Ptr = p1.withUnsafeMutableBufferPointer {|}}
//   expected-remark@8{{macro content: |        unsafe $0|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    defer {|}}
//   expected-remark@11{{macro content: |        _fixLifetime(p1)|}}
//   expected-remark@12{{macro content: |    }|}}
//   expected-remark@13{{macro content: |    let _p2Ptr = p2.withUnsafeMutableBufferPointer {|}}
//   expected-remark@14{{macro content: |        unsafe $0|}}
//   expected-remark@15{{macro content: |    }|}}
//   expected-remark@16{{macro content: |    defer {|}}
//   expected-remark@17{{macro content: |        _fixLifetime(p2)|}}
//   expected-remark@18{{macro content: |    }|}}
//   expected-remark@19{{macro content: |    return unsafe shared(len, _p1Ptr.baseAddress, _p2Ptr.baseAddress)|}}
//   expected-remark@20{{macro content: |}|}}
// }}
- (void) shared:(int)len :(int * __counted_by(len) __noescape)p1 :(int * __counted_by(len) __noescape)p2;

// expected-expansion@+30:2{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload public class final func complexExpr(_ len: CInt, _ offset: CInt, _ p: inout MutableSpan<CInt>) {|}}
//   expected-remark@3{{macro content: |    if p.count != (len - offset) {|}}
//   expected-remark@4{{macro content: |      fatalError("bounds check failure in complexExpr: expected \\((len - offset)) but got \\(p.count)")|}}
//   expected-remark@5{{macro content: |    }|}}
//   expected-remark@6{{macro content: |    let _pPtr = p.withUnsafeMutableBufferPointer {|}}
//   expected-remark@7{{macro content: |        unsafe $0|}}
//   expected-remark@8{{macro content: |    }|}}
//   expected-remark@9{{macro content: |    defer {|}}
//   expected-remark@10{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@11{{macro content: |    }|}}
//   expected-remark@12{{macro content: |    return unsafe complexExpr(len, offset, _pPtr.baseAddress)|}}
//   expected-remark@13{{macro content: |}|}}
// }}
// expected-expansion@+15:92{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload public final func complexExpr(_ len: CInt, _ offset: CInt, _ p: inout MutableSpan<CInt>) {|}}
//   expected-remark@3{{macro content: |    if p.count != (len - offset) {|}}
//   expected-remark@4{{macro content: |      fatalError("bounds check failure in complexExpr: expected \\((len - offset)) but got \\(p.count)")|}}
//   expected-remark@5{{macro content: |    }|}}
//   expected-remark@6{{macro content: |    let _pPtr = p.withUnsafeMutableBufferPointer {|}}
//   expected-remark@7{{macro content: |        unsafe $0|}}
//   expected-remark@8{{macro content: |    }|}}
//   expected-remark@9{{macro content: |    defer {|}}
//   expected-remark@10{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@11{{macro content: |    }|}}
//   expected-remark@12{{macro content: |    return unsafe complexExpr(len, offset, _pPtr.baseAddress)|}}
//   expected-remark@13{{macro content: |}|}}
// }}
- (void) complexExpr:(int)len :(int) offset :(int * __counted_by(len - offset) __noescape)p;

// expected-expansion@+26:2{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload public class final func nullUnspecified(_ p: inout MutableSpan<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    let _pPtr = p.withUnsafeMutableBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe nullUnspecified(len, _pPtr.baseAddress)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
// expected-expansion@+13:91{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload public final func nullUnspecified(_ p: inout MutableSpan<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    let _pPtr = p.withUnsafeMutableBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe nullUnspecified(len, _pPtr.baseAddress)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
- (void) nullUnspecified:(int)len :(int * __counted_by(len) _Null_unspecified __noescape)p;

// expected-expansion@+26:2{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload public class final func nonnull(_ p: inout MutableSpan<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    let _pPtr = p.withUnsafeMutableBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe nonnull(len, _pPtr.baseAddress!)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
// expected-expansion@+13:74{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload public final func nonnull(_ p: inout MutableSpan<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    let _pPtr = p.withUnsafeMutableBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe nonnull(len, _pPtr.baseAddress!)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
- (void) nonnull:(int)len :(int * __counted_by(len) __noescape _Nonnull)p;

// expected-expansion@+26:2{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload public class final func nullable(_ p: inout MutableSpan<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    let _pPtr = p.withUnsafeMutableBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe nullable(len, _pPtr.baseAddress)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
// expected-expansion@+13:76{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload public final func nullable(_ p: inout MutableSpan<CInt>) {|}}
//   expected-remark@3 {{macro content: |    let len = CInt(exactly: p.count)!|}}
//   expected-remark@4 {{macro content: |    let _pPtr = p.withUnsafeMutableBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10 {{macro content: |    return unsafe nullable(len, _pPtr.baseAddress)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
- (void) nullable:(int)len :(int * __counted_by(len) _Nullable __noescape)p;


// expected-experimental-expansion@+37:2{{
//   expected-experimental-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-experimental-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_lifetime(p: copy p) @_disfavoredOverload public class final func returnPointerNullable(_ p: inout MutableSpan<CInt>) -> MutableSpan<CInt> {|}}
//   expected-experimental-remark@3{{macro content: |    let len = CInt(exactly: p.count)!|}}
//   expected-experimental-remark@4{{macro content: |    let _pPtr = p.withUnsafeMutableBufferPointer {|}}
//   expected-experimental-remark@5{{macro content: |        unsafe $0|}}
//   expected-experimental-remark@6{{macro content: |    }|}}
//   expected-experimental-remark@7{{macro content: |    defer {|}}
//   expected-experimental-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-experimental-remark@9{{macro content: |    }|}}
//   expected-experimental-remark@10{{macro content: |    let _resultValue = unsafe returnPointerNullable(len, _pPtr.baseAddress)|}}
//   expected-experimental-remark@11{{macro content: |    if unsafe _resultValue == nil {|}}
//   expected-experimental-remark@12{{macro content: |      precondition(len == 0, "counted_by may only be null if count is 0 (unlike counted_by_or_null)")|}}
//   expected-experimental-remark@13{{macro content: |      return MutableSpan<CInt>()|}}
//   expected-experimental-remark@14{{macro content: |    }|}}
//   expected-experimental-remark@15{{macro content: |    return unsafe _swiftifyOverrideLifetime(MutableSpan<CInt>(_unsafeStart: _resultValue!, count: Int(len)), copying: ())|}}
//   expected-experimental-remark@16{{macro content: |}|}}
// }}
// expected-stable-note@+19{{'returnPointerNullable' declared here}}
// expected-experimental-expansion@+18:125{{
//   expected-experimental-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-experimental-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_lifetime(p: copy p) @_disfavoredOverload public final func returnPointerNullable(_ p: inout MutableSpan<CInt>) -> MutableSpan<CInt> {|}}
//   expected-experimental-remark@3 {{macro content: |    let len = CInt(exactly: p.count)!|}}
//   expected-experimental-remark@4 {{macro content: |    let _pPtr = p.withUnsafeMutableBufferPointer {|}}
//   expected-experimental-remark@5{{macro content: |        unsafe $0|}}
//   expected-experimental-remark@6{{macro content: |    }|}}
//   expected-experimental-remark@7{{macro content: |    defer {|}}
//   expected-experimental-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-experimental-remark@9{{macro content: |    }|}}
//   expected-experimental-remark@10 {{macro content: |    let _resultValue = unsafe returnPointerNullable(len, _pPtr.baseAddress)|}}
//   expected-experimental-remark@11{{macro content: |    if unsafe _resultValue == nil {|}}
//   expected-experimental-remark@12 {{macro content: |      precondition(len == 0, "counted_by may only be null if count is 0 (unlike counted_by_or_null)")|}}
//   expected-experimental-remark@13 {{macro content: |      return MutableSpan<CInt>()|}}
//   expected-experimental-remark@14 {{macro content: |    }|}}
//   expected-experimental-remark@15 {{macro content: |    return unsafe _swiftifyOverrideLifetime(MutableSpan<CInt>(_unsafeStart: _resultValue!, count: Int(len)), copying: ())|}}
//   expected-experimental-remark@16 {{macro content: |}|}}
// }}
- (int * __counted_by(len) _Nullable) returnPointerNullable:(int)len : (int * __counted_by(len) _Nullable) __lifetimebound p;


// expected-experimental-expansion@+27:2{{
//   expected-experimental-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-experimental-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_lifetime(p: copy p) @_disfavoredOverload public class final func returnPointerNonnull(_ p: inout MutableSpan<CInt>) -> MutableSpan<CInt> {|}}
//   expected-experimental-remark@3{{macro content: |    let len = CInt(exactly: p.count)!|}}
//   expected-experimental-remark@4{{macro content: |    let _pPtr = p.withUnsafeMutableBufferPointer {|}}
//   expected-experimental-remark@5{{macro content: |        unsafe $0|}}
//   expected-experimental-remark@6{{macro content: |    }|}}
//   expected-experimental-remark@7{{macro content: |    defer {|}}
//   expected-experimental-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-experimental-remark@9{{macro content: |    }|}}
//   expected-experimental-remark@10{{macro content: |    return unsafe _swiftifyOverrideLifetime(MutableSpan<CInt>(_unsafeStart: unsafe returnPointerNonnull(len, _pPtr.baseAddress!), count: Int(len)), copying: ())|}}
//   expected-experimental-remark@11{{macro content: |}|}}
// }}
// expected-stable-note@+14{{'returnPointerNonnull' declared here}}
// expected-experimental-expansion@+13:122{{
//   expected-experimental-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-experimental-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(copy p) @_lifetime(p: copy p) @_disfavoredOverload public final func returnPointerNonnull(_ p: inout MutableSpan<CInt>) -> MutableSpan<CInt> {|}}
//   expected-experimental-remark@3{{macro content: |    let len = CInt(exactly: p.count)!|}}
//   expected-experimental-remark@4{{macro content: |    let _pPtr = p.withUnsafeMutableBufferPointer {|}}
//   expected-experimental-remark@5{{macro content: |        unsafe $0|}}
//   expected-experimental-remark@6{{macro content: |    }|}}
//   expected-experimental-remark@7{{macro content: |    defer {|}}
//   expected-experimental-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-experimental-remark@9{{macro content: |    }|}}
//   expected-experimental-remark@10{{macro content: |    return unsafe _swiftifyOverrideLifetime(MutableSpan<CInt>(_unsafeStart: unsafe returnPointerNonnull(len, _pPtr.baseAddress!), count: Int(len)), copying: ())|}}
//   expected-experimental-remark@11{{macro content: |}|}}
// }}
- (int * __counted_by(len) _Nonnull) returnPointerNonnull:(int)len : (int * __counted_by(len) _Nonnull) __lifetimebound p;

// expected-expansion@+32:2{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p1: copy p1) @_disfavoredOverload public class final func mixedEscapability(_ p1: inout MutableSpan<CInt>, _ p2: UnsafeMutableBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: p2.count)!|}}
//   expected-remark@4{{macro content: |    if p1.count != len {|}}
//   expected-remark@5{{macro content: |      fatalError("bounds check failure in mixedEscapability: expected \\(len) but got \\(p1.count)")|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    let _p1Ptr = p1.withUnsafeMutableBufferPointer {|}}
//   expected-remark@8{{macro content: |        unsafe $0|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    defer {|}}
//   expected-remark@11{{macro content: |        _fixLifetime(p1)|}}
//   expected-remark@12{{macro content: |    }|}}
//   expected-remark@13{{macro content: |    return unsafe mixedEscapability(len, _p1Ptr.baseAddress, p2.baseAddress)|}}
//   expected-remark@14{{macro content: |}|}}
// }}
// expected-expansion@+16:105{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p1: copy p1) @_disfavoredOverload public final func mixedEscapability(_ p1: inout MutableSpan<CInt>, _ p2: UnsafeMutableBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: p2.count)!|}}
//   expected-remark@4{{macro content: |    if p1.count != len {|}}
//   expected-remark@5{{macro content: |      fatalError("bounds check failure in mixedEscapability: expected \\(len) but got \\(p1.count)")|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    let _p1Ptr = p1.withUnsafeMutableBufferPointer {|}}
//   expected-remark@8{{macro content: |        unsafe $0|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    defer {|}}
//   expected-remark@11{{macro content: |        _fixLifetime(p1)|}}
//   expected-remark@12{{macro content: |    }|}}
//   expected-remark@13{{macro content: |    return unsafe mixedEscapability(len, _p1Ptr.baseAddress, p2.baseAddress)|}}
//   expected-remark@14{{macro content: |}|}}
// }}
- (void) mixedEscapability:(int)len :(int * __counted_by(len) __noescape)p1 :(int * __counted_by(len))p2;

// expected-expansion@+13:70{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(p: copy p) @_disfavoredOverload public class final func staticMethod(_ p: inout MutableSpan<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    let _pPtr = p.withUnsafeMutableBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe staticMethod(len, _pPtr.baseAddress)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
+ (void) staticMethod:(int)len :(int * __counted_by(len) __noescape)p;
@end

//--- method.swift
// GENERATED-BY: %target-swift-ide-test -print-module -module-to-print=Method -plugin-path %swift-plugin-dir -I %t/Inputs -source-filename=x -enable-experimental-feature SafeInteropWrappers -enable-experimental-feature SafeInteropWrappersNullAsEmptySpan -enable-experimental-feature Lifetimes > %t/Test-interface.swift && %swift-function-caller-generator Method %t/Test-interface.swift
// GENERATED-HASH: aa625ed717982a6dd015712bc112d86de7b553d76c42e1b97112e0d4363daccb
import Method


extension Foo {
  final func call_simple_Foo_classmethod(_ len: CInt, _ p: UnsafeMutablePointer<CInt>!) {
    return unsafe Foo.simple(len, p)
  }
  @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
    @_lifetime(p: copy p)
    @_alwaysEmitIntoClient @_disfavoredOverload final func call_simple_Foo(_ p: inout MutableSpan<CInt>) {
    return simple(&p)
  }
  @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
    @_lifetime(p: copy p)
    @_alwaysEmitIntoClient @_disfavoredOverload final func call_simple_Foo_classmethod(_ p: inout MutableSpan<CInt>) {
    return Foo.simple(&p)
  }
  final func call_simple_Foo(_ len: CInt, _ p: UnsafeMutablePointer<CInt>!) {
    return unsafe simple(len, p)
  }
  final func call_shared_Foo_classmethod(_ len: CInt, _ p1: UnsafeMutablePointer<CInt>!, _ p2: UnsafeMutablePointer<CInt>!) {
    return unsafe Foo.shared(len, p1, p2)
  }
  @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
    @_lifetime(p1: copy p1)
    @_lifetime(p2: copy p2)
    @_alwaysEmitIntoClient @_disfavoredOverload final func call_shared_Foo(_ p1: inout MutableSpan<CInt>, _ p2: inout MutableSpan<CInt>) {
    return shared(&p1, &p2)
  }
  @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
    @_lifetime(p1: copy p1)
    @_lifetime(p2: copy p2)
    @_alwaysEmitIntoClient @_disfavoredOverload final func call_shared_Foo_classmethod(_ p1: inout MutableSpan<CInt>, _ p2: inout MutableSpan<CInt>) {
    return Foo.shared(&p1, &p2)
  }
  final func call_shared_Foo(_ len: CInt, _ p1: UnsafeMutablePointer<CInt>!, _ p2: UnsafeMutablePointer<CInt>!) {
    return unsafe shared(len, p1, p2)
  }
  final func call_complexExpr_Foo_classmethod(_ len: CInt, _ offset: CInt, _ p: UnsafeMutablePointer<CInt>!) {
    return unsafe Foo.complexExpr(len, offset, p)
  }
  @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
    @_lifetime(p: copy p)
    @_alwaysEmitIntoClient @_disfavoredOverload final func call_complexExpr_Foo(_ len: CInt, _ offset: CInt, _ p: inout MutableSpan<CInt>) {
    return complexExpr(len, offset, &p)
  }
  @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
    @_lifetime(p: copy p)
    @_alwaysEmitIntoClient @_disfavoredOverload final func call_complexExpr_Foo_classmethod(_ len: CInt, _ offset: CInt, _ p: inout MutableSpan<CInt>) {
    return Foo.complexExpr(len, offset, &p)
  }
  final func call_complexExpr_Foo(_ len: CInt, _ offset: CInt, _ p: UnsafeMutablePointer<CInt>!) {
    return unsafe complexExpr(len, offset, p)
  }
  final func call_nullUnspecified_Foo_classmethod(_ len: CInt, _ p: UnsafeMutablePointer<CInt>!) {
    return unsafe Foo.nullUnspecified(len, p)
  }
  @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
    @_lifetime(p: copy p)
    @_alwaysEmitIntoClient @_disfavoredOverload final func call_nullUnspecified_Foo(_ p: inout MutableSpan<CInt>) {
    return nullUnspecified(&p)
  }
  @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
    @_lifetime(p: copy p)
    @_alwaysEmitIntoClient @_disfavoredOverload final func call_nullUnspecified_Foo_classmethod(_ p: inout MutableSpan<CInt>) {
    return Foo.nullUnspecified(&p)
  }
  final func call_nullUnspecified_Foo(_ len: CInt, _ p: UnsafeMutablePointer<CInt>!) {
    return unsafe nullUnspecified(len, p)
  }
  final func call_nonnull_Foo_classmethod(_ len: CInt, _ p: UnsafeMutablePointer<CInt>) {
    return unsafe Foo.nonnull(len, p)
  }
  @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
    @_lifetime(p: copy p)
    @_alwaysEmitIntoClient @_disfavoredOverload final func call_nonnull_Foo(_ p: inout MutableSpan<CInt>) {
    return nonnull(&p)
  }
  @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
    @_lifetime(p: copy p)
    @_alwaysEmitIntoClient @_disfavoredOverload final func call_nonnull_Foo_classmethod(_ p: inout MutableSpan<CInt>) {
    return Foo.nonnull(&p)
  }
  final func call_nonnull_Foo(_ len: CInt, _ p: UnsafeMutablePointer<CInt>) {
    return unsafe nonnull(len, p)
  }
  final func call_nullable_Foo_classmethod(_ len: CInt, _ p: UnsafeMutablePointer<CInt>?) {
    return unsafe Foo.nullable(len, p)
  }
  @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
    @_lifetime(p: copy p)
    @_alwaysEmitIntoClient @_disfavoredOverload final func call_nullable_Foo(_ p: inout MutableSpan<CInt>) {
    return nullable(&p)
  }
  @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
    @_lifetime(p: copy p)
    @_alwaysEmitIntoClient @_disfavoredOverload final func call_nullable_Foo_classmethod(_ p: inout MutableSpan<CInt>) {
    return Foo.nullable(&p)
  }
  final func call_nullable_Foo(_ len: CInt, _ p: UnsafeMutablePointer<CInt>?) {
    return unsafe nullable(len, p)
  }
  final func call_returnPointerNullable_Foo_classmethod(_ len: CInt, _ p: UnsafeMutablePointer<CInt>?) -> UnsafeMutablePointer<CInt>? {
    return unsafe Foo.returnPointerNullable(len, p)
  }
  @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
    @_lifetime(copy p)
    @_lifetime(p: copy p)
    @_alwaysEmitIntoClient @_disfavoredOverload final func call_returnPointerNullable_Foo(_ p: inout MutableSpan<CInt>) -> MutableSpan<CInt> {
    // expected-stable-error@+5{{missing argument for parameter #2 in call}}
    // expected-stable-note@+4{{arguments to generic parameter 'Pointee' ('MutableSpan<CInt>' (aka 'MutableSpan<Int32>') and 'CInt' (aka 'Int32')) are expected to be equal}}
    // expected-stable-error@+3{{cannot convert value of type 'UnsafeMutablePointer<MutableSpan<CInt>>' (aka 'UnsafeMutablePointer<MutableSpan<Int32>>') to expected argument type 'UnsafeMutablePointer<CInt>' (aka 'UnsafeMutablePointer<Int32>')}}
    // expected-stable-error@+2{{cannot convert value of type 'MutableSpan<CInt>' (aka 'MutableSpan<Int32>') to expected argument type 'CInt' (aka 'Int32')}}
    // expected-stable-error@+1{{cannot convert return expression of type 'UnsafeMutablePointer<CInt>?' (aka 'Optional<UnsafeMutablePointer<Int32>>') to return type 'MutableSpan<CInt>' (aka 'MutableSpan<Int32>')}}
    return returnPointerNullable(&p)
  }
  @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
    @_lifetime(copy p)
    @_lifetime(p: copy p)
    @_alwaysEmitIntoClient @_disfavoredOverload final func call_returnPointerNullable_Foo_classmethod(_ p: inout MutableSpan<CInt>) -> MutableSpan<CInt> {
    // expected-stable-error@+2{{cannot convert value of type 'MutableSpan<CInt>' (aka 'MutableSpan<Int32>') to expected argument type 'Foo'}}
    // expected-stable-error@+1{{cannot convert return expression of type '(CInt, UnsafeMutablePointer<CInt>?) -> UnsafeMutablePointer<CInt>?' (aka '(Int32, Optional<UnsafeMutablePointer<Int32>>) -> Optional<UnsafeMutablePointer<Int32>>') to return type 'MutableSpan<CInt>' (aka 'MutableSpan<Int32>')}}
    return Foo.returnPointerNullable(&p)
  }
  final func call_returnPointerNullable_Foo(_ len: CInt, _ p: UnsafeMutablePointer<CInt>?) -> UnsafeMutablePointer<CInt>? {
    return unsafe returnPointerNullable(len, p)
  }
  final func call_returnPointerNonnull_Foo_classmethod(_ len: CInt, _ p: UnsafeMutablePointer<CInt>) -> UnsafeMutablePointer<CInt> {
    return unsafe Foo.returnPointerNonnull(len, p)
  }
  @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
    @_lifetime(copy p)
    @_lifetime(p: copy p)
    @_alwaysEmitIntoClient @_disfavoredOverload final func call_returnPointerNonnull_Foo(_ p: inout MutableSpan<CInt>) -> MutableSpan<CInt> {
    // expected-stable-error@+5{{missing argument for parameter #2 in call}}
    // expected-stable-note@+4{{arguments to generic parameter 'Pointee' ('MutableSpan<CInt>' (aka 'MutableSpan<Int32>') and 'CInt' (aka 'Int32')) are expected to be equal}}
    // expected-stable-error@+3{{cannot convert value of type 'UnsafeMutablePointer<MutableSpan<CInt>>' (aka 'UnsafeMutablePointer<MutableSpan<Int32>>') to expected argument type 'UnsafeMutablePointer<CInt>' (aka 'UnsafeMutablePointer<Int32>')}}
    // expected-stable-error@+2{{cannot convert value of type 'MutableSpan<CInt>' (aka 'MutableSpan<Int32>') to expected argument type 'CInt' (aka 'Int32')}}
    // expected-stable-error@+1{{cannot convert return expression of type 'UnsafeMutablePointer<CInt>' (aka 'UnsafeMutablePointer<Int32>') to return type 'MutableSpan<CInt>' (aka 'MutableSpan<Int32>')}}
    return returnPointerNonnull(&p)
  }
  @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
    @_lifetime(copy p)
    @_lifetime(p: copy p)
    @_alwaysEmitIntoClient @_disfavoredOverload final func call_returnPointerNonnull_Foo_classmethod(_ p: inout MutableSpan<CInt>) -> MutableSpan<CInt> {
    // expected-stable-error@+2{{cannot convert value of type 'MutableSpan<CInt>' (aka 'MutableSpan<Int32>') to expected argument type 'Foo'}}
    // expected-stable-error@+1{{cannot convert return expression of type '(CInt, UnsafeMutablePointer<CInt>) -> UnsafeMutablePointer<CInt>' (aka '(Int32, UnsafeMutablePointer<Int32>) -> UnsafeMutablePointer<Int32>') to return type 'MutableSpan<CInt>' (aka 'MutableSpan<Int32>')}}
    return Foo.returnPointerNonnull(&p)
  }
  final func call_returnPointerNonnull_Foo(_ len: CInt, _ p: UnsafeMutablePointer<CInt>) -> UnsafeMutablePointer<CInt> {
    return unsafe returnPointerNonnull(len, p)
  }
  final func call_mixedEscapability_Foo_classmethod(_ len: CInt, _ p1: UnsafeMutablePointer<CInt>!, _ p2: UnsafeMutablePointer<CInt>!) {
    return unsafe Foo.mixedEscapability(len, p1, p2)
  }
  @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
    @_lifetime(p1: copy p1)
    @_alwaysEmitIntoClient @_disfavoredOverload final func call_mixedEscapability_Foo(_ p1: inout MutableSpan<CInt>, _ p2: UnsafeMutableBufferPointer<CInt>) {
    return unsafe mixedEscapability(&p1, p2)
  }
  @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
    @_lifetime(p1: copy p1)
    @_alwaysEmitIntoClient @_disfavoredOverload final func call_mixedEscapability_Foo_classmethod(_ p1: inout MutableSpan<CInt>, _ p2: UnsafeMutableBufferPointer<CInt>) {
    return unsafe Foo.mixedEscapability(&p1, p2)
  }
  final func call_mixedEscapability_Foo(_ len: CInt, _ p1: UnsafeMutablePointer<CInt>!, _ p2: UnsafeMutablePointer<CInt>!) {
    return unsafe mixedEscapability(len, p1, p2)
  }
  @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *)
    @_lifetime(p: copy p)
    @_alwaysEmitIntoClient @_disfavoredOverload final func call_staticMethod_Foo_classmethod(_ p: inout MutableSpan<CInt>) {
    return Foo.staticMethod(&p)
  }
  final func call_staticMethod_Foo_classmethod(_ len: CInt, _ p: UnsafeMutablePointer<CInt>!) {
    return unsafe Foo.staticMethod(len, p)
  }
}
