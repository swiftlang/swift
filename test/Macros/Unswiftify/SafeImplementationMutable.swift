// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_SafeInteropImplementations
// REQUIRES: swift_feature_SafeInteropWrappers

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/test.swift -emit-module -plugin-path %swift-plugin-dir -import-bridging-header %t/test.h -verify -Rmacro-expansions -enable-experimental-feature SafeInteropImplementations -enable-experimental-feature SafeInteropWrappers -disable-objc-interop -suppress-notes -verify-additional-file %t/test.h

//--- test.h
#define __counted_by(x) __attribute__((__counted_by__(x)))
#define __counted_by_or_null(x) __attribute__((__counted_by_or_null__(x)))
#define __noescape __attribute__((noescape))

// MutableSpan variants (__counted_by + __noescape, mutable pointer):
// expected-expansion@+14:64{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(x: copy x) @_disfavoredOverload public func mspan_default(_ x: inout MutableSpan<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: x.count)!|}}
//   expected-remark@4{{macro content: |    let _xPtr = x.withUnsafeMutableBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(x)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe mspan_default(_xPtr.baseAddress, len)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
// expected-warning@+1{{pointer is missing a nullability type specifier (_Nonnull, _Nullable, or _Null_unspecified)}}
void mspan_default(int *__counted_by(len) __noescape x, int len);
// expected-expansion@+13:74{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(x: copy x) @_disfavoredOverload public func mspan_nonnull(_ x: inout MutableSpan<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: x.count)!|}}
//   expected-remark@4{{macro content: |    let _xPtr = x.withUnsafeMutableBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(x)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe mspan_nonnull(_xPtr.baseAddress!, len)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
void mspan_nonnull(int * _Nonnull __counted_by(len) __noescape x, int len);
// expected-expansion@+13:76{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(x: copy x) @_disfavoredOverload public func mspan_nullable(_ x: inout MutableSpan<CInt>?) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: x?.count ?? 0)!|}}
//   expected-remark@4{{macro content: |    let _xPtr = x?.withUnsafeMutableBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(x)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe mspan_nullable(_xPtr?.baseAddress, len)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
void mspan_nullable(int * _Nullable __counted_by(len) __noescape x, int len);

// MutableSpan + __counted_by_or_null + __noescape:
// expected-expansion@+14:79{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(x: copy x) @_disfavoredOverload public func mspan_ornull_default(_ x: inout MutableSpan<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: x.count)!|}}
//   expected-remark@4{{macro content: |    let _xPtr = x.withUnsafeMutableBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(x)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe mspan_ornull_default(_xPtr.baseAddress, len)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
// expected-warning@+1{{pointer is missing a nullability type specifier (_Nonnull, _Nullable, or _Null_unspecified)}}
void mspan_ornull_default(int *__counted_by_or_null(len) __noescape x, int len);
// expected-expansion@+13:91{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(x: copy x) @_disfavoredOverload public func mspan_ornull_nullable(_ x: inout MutableSpan<CInt>?) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: x?.count ?? 0)!|}}
//   expected-remark@4{{macro content: |    let _xPtr = x?.withUnsafeMutableBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(x)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe mspan_ornull_nullable(_xPtr?.baseAddress, len)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
void mspan_ornull_nullable(int * _Nullable __counted_by_or_null(len) __noescape x, int len);
// expected-expansion@+14:89{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(x: copy x) @_disfavoredOverload public func mspan_ornull_nonnull(_ x: inout MutableSpan<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: x.count)!|}}
//   expected-remark@4{{macro content: |    let _xPtr = x.withUnsafeMutableBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(x)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe mspan_ornull_nonnull(_xPtr.baseAddress!, len)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
// expected-warning@+1{{combining '__counted_by_or_null' and '_Nonnull'; did you mean '__counted_by' instead?}}
void mspan_ornull_nonnull(int * _Nonnull __counted_by_or_null(len) __noescape x, int len);

// UnsafeMutableBufferPointer variants (__counted_by, no __noescape):
// expected-expansion@+8:52{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func mubp_default(_ x: UnsafeMutableBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: x.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe mubp_default(x.baseAddress, len)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
// expected-warning@+1{{pointer is missing a nullability type specifier (_Nonnull, _Nullable, or _Null_unspecified)}}
void mubp_default(int *__counted_by(len) x, int len);
// expected-expansion@+7:62{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func mubp_nonnull(_ x: UnsafeMutableBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: x.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe mubp_nonnull(x.baseAddress!, len)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
void mubp_nonnull(int * _Nonnull __counted_by(len) x, int len);
// expected-expansion@+7:64{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func mubp_nullable(_ x: UnsafeMutableBufferPointer<CInt>?) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: unsafe x?.count ?? 0)!|}}
//   expected-remark@4{{macro content: |    return unsafe mubp_nullable(x?.baseAddress, len)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
void mubp_nullable(int * _Nullable __counted_by(len) x, int len);

// UnsafeMutableBufferPointer + __counted_by_or_null, _Nullable (no __noescape):
// expected-expansion@+7:79{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func mubp_ornull_nullable(_ x: UnsafeMutableBufferPointer<CInt>?) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: unsafe x?.count ?? 0)!|}}
//   expected-remark@4{{macro content: |    return unsafe mubp_ornull_nullable(x?.baseAddress, len)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
void mubp_ornull_nullable(int * _Nullable __counted_by_or_null(len) x, int len);

//--- test.swift

// -----------------------------------------------------------------------
// MutableSpan (__counted_by + __noescape, mutable pointer)
//
// `inout MutableSpan` params get a `var` binding before the call.
// Nullable pointers use the `if let` nil-guard producing an empty
// MutableSpan for nil.
// -----------------------------------------------------------------------

// Default nullable → nil-guarded empty MutableSpan, `var _arg0` for inout
@c @implementation(safe)
public func mspan_default(_ x: inout MutableSpan<CInt>) {
// expected-expansion@+7:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func mspan_default(_ x: UnsafeMutablePointer<CInt>?, _ len: CInt) {|}}
//   expected-remark@3{{macro content: |    var _ptr0 = if x != nil { MutableSpan(_unsafeStart: x!, count: Int(len)) } else { MutableSpan<CInt>() }|}}
//   expected-remark@4{{macro content: |    mspan_default(&_ptr0)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
}

// _Nonnull → direct construction, `var _arg0` for inout
@c @implementation(safe)
public func mspan_nonnull(_ x: inout MutableSpan<CInt>) {
// expected-expansion@+7:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func mspan_nonnull(_ x: UnsafeMutablePointer<CInt>, _ len: CInt) {|}}
//   expected-remark@3{{macro content: |    var _ptr0 = MutableSpan(_unsafeStart: x, count: Int(len))|}}
//   expected-remark@4{{macro content: |    mspan_nonnull(&_ptr0)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
}

// _Nullable → nil-guarded empty MutableSpan
@c @implementation(safe)
public func mspan_nullable(_ x: inout MutableSpan<CInt>?) {
// expected-expansion@+7:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func mspan_nullable(_ x: UnsafeMutablePointer<CInt>?, _ len: CInt) {|}}
//   expected-remark@3{{macro content: |    var _ptr0: MutableSpan? = if x != nil { MutableSpan(_unsafeStart: x!, count: Int(len)) } else { nil }|}}
//   expected-remark@4{{macro content: |    mspan_nullable(&_ptr0)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
}

// -----------------------------------------------------------------------
// MutableSpan + __counted_by_or_null + __noescape
// -----------------------------------------------------------------------

// Default nullable, non-Optional safe param → empty MutableSpan for nil
@c @implementation(safe)
public func mspan_ornull_default(_ x: inout MutableSpan<CInt>) {
// expected-expansion@+7:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func mspan_ornull_default(_ x: UnsafeMutablePointer<CInt>?, _ len: CInt) {|}}
//   expected-remark@3{{macro content: |    var _ptr0 = if x != nil { MutableSpan(_unsafeStart: x!, count: Int(len)) } else { MutableSpan<CInt>() }|}}
//   expected-remark@4{{macro content: |    mspan_ornull_default(&_ptr0)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
}

// _Nullable, Optional safe param → nil for nil pointer
@c @implementation(safe)
public func mspan_ornull_nullable(_ x: inout MutableSpan<CInt>?) {
// expected-expansion@+7:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func mspan_ornull_nullable(_ x: UnsafeMutablePointer<CInt>?, _ len: CInt) {|}}
//   expected-remark@3{{macro content: |    var _ptr0: MutableSpan? = if x != nil { MutableSpan(_unsafeStart: x!, count: Int(len)) } else { nil }|}}
//   expected-remark@4{{macro content: |    mspan_ornull_nullable(&_ptr0)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
}

// _Nonnull, non-Optional → direct construction
@c @implementation(safe)
public func mspan_ornull_nonnull(_ x: inout MutableSpan<CInt>) {
// expected-expansion@+7:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func mspan_ornull_nonnull(_ x: UnsafeMutablePointer<CInt>, _ len: CInt) {|}}
//   expected-remark@3{{macro content: |    var _ptr0 = MutableSpan(_unsafeStart: x, count: Int(len))|}}
//   expected-remark@4{{macro content: |    mspan_ornull_nonnull(&_ptr0)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
}

// -----------------------------------------------------------------------
// UnsafeMutableBufferPointer (__counted_by, no __noescape)
//
// UMBP(start:count:) accepts optional pointers natively.
// -----------------------------------------------------------------------

// Default nullable
@c @implementation(safe)
public func mubp_default(_ x: UnsafeMutableBufferPointer<CInt>) {
// expected-expansion@+7:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func mubp_default(_ x: UnsafeMutablePointer<CInt>?, _ len: CInt) {|}}
//   expected-remark@3{{macro content: |    let _ptr0 = UnsafeMutableBufferPointer(start: x, count: Int(len))|}}
//   expected-remark@4{{macro content: |    mubp_default(_ptr0)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
}

// _Nonnull
@c @implementation(safe)
public func mubp_nonnull(_ x: UnsafeMutableBufferPointer<CInt>) {
// expected-expansion@+7:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func mubp_nonnull(_ x: UnsafeMutablePointer<CInt>, _ len: CInt) {|}}
//   expected-remark@3{{macro content: |    let _ptr0 = UnsafeMutableBufferPointer(start: x, count: Int(len))|}}
//   expected-remark@4{{macro content: |    mubp_nonnull(_ptr0)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
}

// _Nullable
@c @implementation(safe)
public func mubp_nullable(_ x: UnsafeMutableBufferPointer<CInt>?) {
// expected-expansion@+7:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func mubp_nullable(_ x: UnsafeMutablePointer<CInt>?, _ len: CInt) {|}}
//   expected-remark@3{{macro content: |    let _ptr0 = x.map { UnsafeMutableBufferPointer(start: $0, count: Int(len)) }|}}
//   expected-remark@4{{macro content: |    mubp_nullable(_ptr0)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
}

// __counted_by_or_null + _Nullable → Optional safe param, `.map` for UMBP
@c @implementation(safe)
public func mubp_ornull_nullable(_ x: UnsafeMutableBufferPointer<CInt>?) {
// expected-expansion@+7:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func mubp_ornull_nullable(_ x: UnsafeMutablePointer<CInt>?, _ len: CInt) {|}}
//   expected-remark@3{{macro content: |    let _ptr0 = x.map { UnsafeMutableBufferPointer(start: $0, count: Int(len)) }|}}
//   expected-remark@4{{macro content: |    mubp_ornull_nullable(_ptr0)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
}
