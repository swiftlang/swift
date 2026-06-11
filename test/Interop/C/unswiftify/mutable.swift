// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_SafeInteropImplementations

// RUN: %empty-directory(%t)
// RUN: split-file --leading-lines %s %t

// RUN: %target-verify-unswiftify %t/test.swift -import-bridging-header %t/test.h -verify-additional-file %t/test.h \
// RUN:   -enable-experimental-feature SafeInteropImplementations

//--- test.swift
// -----------------------------------------------------------------------
// MutableSpan (__counted_by + __noescape, mutable pointer)
// -----------------------------------------------------------------------
@c @implementation
public func mspan_default(_ x: inout MutableSpan<CInt>) {
// expected-expansion@+10:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func mspan_default(_ x: UnsafeMutablePointer<CInt>?, _ len: CInt) {|}}
//   expected-remark@3{{macro content: |    let _count0 = Int(len)|}}
//   expected-remark@4{{macro content: |    precondition(_count0 >= 0, "buffer with negative count")|}}
//   expected-remark@5{{macro content: |    precondition(unsafe x != nil || _count0 == 0, "null buffer with non-zero count")|}}
//   expected-remark@6{{macro content: |    var _safeArg0 = if unsafe x != nil { unsafe MutableSpan(_unsafeStart: x!, count: _count0) } else { MutableSpan<CInt>() }|}}
//   expected-remark@7{{macro content: |    mspan_default(&_safeArg0)|}}
//   expected-remark@8{{macro content: |}|}}
// }}
}

@c @implementation
public func mspan_nonnull(_ x: inout MutableSpan<CInt>) {
// expected-expansion@+9:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func mspan_nonnull(_ x: UnsafeMutablePointer<CInt>, _ len: CInt) {|}}
//   expected-remark@3{{macro content: |    let _count0 = Int(len)|}}
//   expected-remark@4{{macro content: |    precondition(_count0 >= 0, "buffer with negative count")|}}
//   expected-remark@5{{macro content: |    var _safeArg0 = unsafe MutableSpan(_unsafeStart: x, count: _count0)|}}
//   expected-remark@6{{macro content: |    mspan_nonnull(&_safeArg0)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
}

@c @implementation
public func mspan_nullable(_ x: inout MutableSpan<CInt>) {
// expected-expansion@+10:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func mspan_nullable(_ x: UnsafeMutablePointer<CInt>?, _ len: CInt) {|}}
//   expected-remark@3{{macro content: |    let _count0 = Int(len)|}}
//   expected-remark@4{{macro content: |    precondition(_count0 >= 0, "buffer with negative count")|}}
//   expected-remark@5{{macro content: |    precondition(unsafe x != nil || _count0 == 0, "null buffer with non-zero count")|}}
//   expected-remark@6{{macro content: |    var _safeArg0 = if unsafe x != nil { unsafe MutableSpan(_unsafeStart: x!, count: _count0) } else { MutableSpan<CInt>() }|}}
//   expected-remark@7{{macro content: |    mspan_nullable(&_safeArg0)|}}
//   expected-remark@8{{macro content: |}|}}
// }}
}

// -----------------------------------------------------------------------
// MutableSpan + __counted_by_or_null + __noescape
// -----------------------------------------------------------------------
@c @implementation
public func mspan_ornull_default(_ x: inout MutableSpan<CInt>) {
// expected-expansion@+9:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func mspan_ornull_default(_ x: UnsafeMutablePointer<CInt>?, _ len: CInt) {|}}
//   expected-remark@3{{macro content: |    let _count0 = (unsafe x != nil) ? Int(len) : 0|}}
//   expected-remark@4{{macro content: |    precondition(_count0 >= 0, "non-null buffer with negative count")|}}
//   expected-remark@5{{macro content: |    var _safeArg0 = if unsafe x != nil { unsafe MutableSpan(_unsafeStart: x!, count: _count0) } else { MutableSpan<CInt>() }|}}
//   expected-remark@6{{macro content: |    mspan_ornull_default(&_safeArg0)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
}

@c @implementation
public func mspan_ornull_nullable(_ x: inout MutableSpan<CInt>?) {
// expected-expansion@+9:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func mspan_ornull_nullable(_ x: UnsafeMutablePointer<CInt>?, _ len: CInt) {|}}
//   expected-remark@3{{macro content: |    let _count0 = (unsafe x != nil) ? Int(len) : 0|}}
//   expected-remark@4{{macro content: |    precondition(_count0 >= 0, "non-null buffer with negative count")|}}
//   expected-remark@5{{macro content: |    var _safeArg0: MutableSpan<CInt>? = if unsafe x != nil { unsafe MutableSpan(_unsafeStart: x!, count: _count0) } else { nil }|}}
//   expected-remark@6{{macro content: |    mspan_ornull_nullable(&_safeArg0)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
}

@c @implementation
public func mspan_ornull_nonnull(_ x: inout MutableSpan<CInt>) {
// expected-expansion@+9:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func mspan_ornull_nonnull(_ x: UnsafeMutablePointer<CInt>, _ len: CInt) {|}}
//   expected-remark@3{{macro content: |    let _count0 = Int(len)|}}
//   expected-remark@4{{macro content: |    precondition(_count0 >= 0, "buffer with negative count")|}}
//   expected-remark@5{{macro content: |    var _safeArg0 = unsafe MutableSpan(_unsafeStart: x, count: _count0)|}}
//   expected-remark@6{{macro content: |    mspan_ornull_nonnull(&_safeArg0)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
}

// -----------------------------------------------------------------------
// UnsafeMutableBufferPointer (__counted_by, no __noescape)
// -----------------------------------------------------------------------
@c @implementation
public func mubp_default(_ x: UnsafeMutableBufferPointer<CInt>) {
// expected-expansion@+10:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func mubp_default(_ x: UnsafeMutablePointer<CInt>?, _ len: CInt) {|}}
//   expected-remark@3{{macro content: |    let _count0 = Int(len)|}}
//   expected-remark@4{{macro content: |    precondition(_count0 >= 0, "buffer with negative count")|}}
//   expected-remark@5{{macro content: |    precondition(unsafe x != nil || _count0 == 0, "null buffer with non-zero count")|}}
//   expected-remark@6{{macro content: |    let _safeArg0 = unsafe UnsafeMutableBufferPointer(start: x, count: _count0)|}}
//   expected-remark@7{{macro content: |    unsafe mubp_default(_safeArg0)|}}
//   expected-remark@8{{macro content: |}|}}
// }}
}

@c @implementation
public func mubp_nonnull(_ x: UnsafeMutableBufferPointer<CInt>) {
// expected-expansion@+9:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func mubp_nonnull(_ x: UnsafeMutablePointer<CInt>, _ len: CInt) {|}}
//   expected-remark@3{{macro content: |    let _count0 = Int(len)|}}
//   expected-remark@4{{macro content: |    precondition(_count0 >= 0, "buffer with negative count")|}}
//   expected-remark@5{{macro content: |    let _safeArg0 = unsafe UnsafeMutableBufferPointer(start: x, count: _count0)|}}
//   expected-remark@6{{macro content: |    unsafe mubp_nonnull(_safeArg0)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
}

@c @implementation
public func mubp_nullable(_ x: UnsafeMutableBufferPointer<CInt>) {
// expected-expansion@+10:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func mubp_nullable(_ x: UnsafeMutablePointer<CInt>?, _ len: CInt) {|}}
//   expected-remark@3{{macro content: |    let _count0 = Int(len)|}}
//   expected-remark@4{{macro content: |    precondition(_count0 >= 0, "buffer with negative count")|}}
//   expected-remark@5{{macro content: |    precondition(unsafe x != nil || _count0 == 0, "null buffer with non-zero count")|}}
//   expected-remark@6{{macro content: |    let _safeArg0 = unsafe UnsafeMutableBufferPointer(start: x, count: _count0)|}}
//   expected-remark@7{{macro content: |    unsafe mubp_nullable(_safeArg0)|}}
//   expected-remark@8{{macro content: |}|}}
// }}
}

@c @implementation
public func mubp_ornull_nullable(_ x: UnsafeMutableBufferPointer<CInt>?) {
// expected-expansion@+9:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func mubp_ornull_nullable(_ x: UnsafeMutablePointer<CInt>?, _ len: CInt) {|}}
//   expected-remark@3{{macro content: |    let _count0 = (unsafe x != nil) ? Int(len) : 0|}}
//   expected-remark@4{{macro content: |    precondition(_count0 >= 0, "non-null buffer with negative count")|}}
//   expected-remark@5{{macro content: |    let _safeArg0 = unsafe x.map { unsafe UnsafeMutableBufferPointer(start: $0, count: _count0) }|}}
//   expected-remark@6{{macro content: |    unsafe mubp_ornull_nullable(_safeArg0)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
}

//--- test.h
#define __counted_by(x) __attribute__((__counted_by__(x)))
// expected-note@+1{{expanded from macro '__counted_by_or_null'}}
#define __counted_by_or_null(x) __attribute__((__counted_by_or_null__(x)))
#define __noescape __attribute__((noescape))

// MutableSpan variants (__counted_by + __noescape, mutable pointer):
// expected-expansion@+13:64{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(x: copy x) @_disfavoredOverload public func mspan_default(_ x: inout MutableSpan<CInt>) {|}}
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
void mspan_default(int *__counted_by(len) __noescape x, int len);
// expected-expansion@+13:74{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(x: copy x) @_disfavoredOverload public func mspan_nonnull(_ x: inout MutableSpan<CInt>) {|}}
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
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(x: copy x) @_disfavoredOverload public func mspan_nullable(_ x: inout MutableSpan<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: x.count)!|}}
//   expected-remark@4{{macro content: |    let _xPtr = x.withUnsafeMutableBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(x)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe mspan_nullable(_xPtr.baseAddress, len)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
void mspan_nullable(int * _Nullable __counted_by(len) __noescape x, int len);

// MutableSpan + __counted_by_or_null + __noescape:
// expected-expansion@+13:79{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(x: copy x) @_disfavoredOverload public func mspan_ornull_default(_ x: inout MutableSpan<CInt>) {|}}
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
void mspan_ornull_default(int *__counted_by_or_null(len) __noescape x, int len);
// expected-expansion@+13:91{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(x: copy x) @_disfavoredOverload public func mspan_ornull_nullable(_ x: inout MutableSpan<CInt>?) {|}}
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
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_lifetime(x: copy x) @_disfavoredOverload public func mspan_ornull_nonnull(_ x: inout MutableSpan<CInt>) {|}}
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
// expected-expansion@+7:52{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @_disfavoredOverload public func mubp_default(_ x: UnsafeMutableBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: x.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe mubp_default(x.baseAddress, len)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
void mubp_default(int *__counted_by(len) x, int len);
// expected-expansion@+7:62{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @_disfavoredOverload public func mubp_nonnull(_ x: UnsafeMutableBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: x.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe mubp_nonnull(x.baseAddress!, len)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
void mubp_nonnull(int * _Nonnull __counted_by(len) x, int len);
// expected-expansion@+7:64{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @_disfavoredOverload public func mubp_nullable(_ x: UnsafeMutableBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: x.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe mubp_nullable(x.baseAddress, len)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
void mubp_nullable(int * _Nullable __counted_by(len) x, int len);

// UnsafeMutableBufferPointer + __counted_by_or_null, _Nullable (no __noescape):
// expected-expansion@+7:79{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @_disfavoredOverload public func mubp_ornull_nullable(_ x: UnsafeMutableBufferPointer<CInt>?) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: unsafe x?.count ?? 0)!|}}
//   expected-remark@4{{macro content: |    return unsafe mubp_ornull_nullable(x?.baseAddress, len)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
void mubp_ornull_nullable(int * _Nullable __counted_by_or_null(len) x, int len);

