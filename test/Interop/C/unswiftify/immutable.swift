// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_SafeInteropImplementations

// RUN: %empty-directory(%t)
// RUN: split-file --leading-lines %s %t

// RUN: %target-verify-unswiftify %t/test.swift -import-bridging-header %t/test.h -verify-additional-file %t/test.h -enable-experimental-feature SafeInteropImplementations

//--- test.swift

// -----------------------------------------------------------------------
// UnsafeBufferPointer variants (no __noescape on the C declaration)
// -----------------------------------------------------------------------
@c @implementation
public func ubp_default(_ x: UnsafeBufferPointer<CInt>) {
// expected-expansion@+10:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func ubp_default(_ x: UnsafePointer<CInt>?, _ len: CInt) {|}}
//   expected-remark@3{{macro content: |    let _count0 = Int(len)|}}
//   expected-remark@4{{macro content: |    precondition(_count0 >= 0, "buffer with negative count")|}}
//   expected-remark@5{{macro content: |    precondition(unsafe x != nil || _count0 == 0, "null buffer with non-zero count")|}}
//   expected-remark@6{{macro content: |    let _safeArg0 = unsafe UnsafeBufferPointer(start: x, count: _count0)|}}
//   expected-remark@7{{macro content: |    unsafe ubp_default(_safeArg0)|}}
//   expected-remark@8{{macro content: |}|}}
// }}
}

@c @implementation
public func ubp_nonnull(_ x: UnsafeBufferPointer<CInt>) {
// expected-expansion@+9:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func ubp_nonnull(_ x: UnsafePointer<CInt>, _ len: CInt) {|}}
//   expected-remark@3{{macro content: |    let _count0 = Int(len)|}}
//   expected-remark@4{{macro content: |    precondition(_count0 >= 0, "buffer with negative count")|}}
//   expected-remark@5{{macro content: |    let _safeArg0 = unsafe UnsafeBufferPointer(start: x, count: _count0)|}}
//   expected-remark@6{{macro content: |    unsafe ubp_nonnull(_safeArg0)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
}

@c @implementation
public func ubp_nullable(_ x: UnsafeBufferPointer<CInt>) {
// expected-expansion@+10:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func ubp_nullable(_ x: UnsafePointer<CInt>?, _ len: CInt) {|}}
//   expected-remark@3{{macro content: |    let _count0 = Int(len)|}}
//   expected-remark@4{{macro content: |    precondition(_count0 >= 0, "buffer with negative count")|}}
//   expected-remark@5{{macro content: |    precondition(unsafe x != nil || _count0 == 0, "null buffer with non-zero count")|}}
//   expected-remark@6{{macro content: |    let _safeArg0 = unsafe UnsafeBufferPointer(start: x, count: _count0)|}}
//   expected-remark@7{{macro content: |    unsafe ubp_nullable(_safeArg0)|}}
//   expected-remark@8{{macro content: |}|}}
// }}
}

// -----------------------------------------------------------------------
// Span variants (__noescape on the C declaration)
// -----------------------------------------------------------------------
@c @implementation
public func span_default(_ x: Span<CInt>) {
// expected-expansion@+10:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func span_default(_ x: UnsafePointer<CInt>?, _ len: CInt) {|}}
//   expected-remark@3{{macro content: |    let _count0 = Int(len)|}}
//   expected-remark@4{{macro content: |    precondition(_count0 >= 0, "buffer with negative count")|}}
//   expected-remark@5{{macro content: |    precondition(unsafe x != nil || _count0 == 0, "null buffer with non-zero count")|}}
//   expected-remark@6{{macro content: |    let _safeArg0 = if unsafe x != nil { unsafe Span(_unsafeStart: x!, count: _count0) } else { Span<CInt>() }|}}
//   expected-remark@7{{macro content: |    span_default(_safeArg0)|}}
//   expected-remark@8{{macro content: |}|}}
// }}
}

@c @implementation
public func span_nonnull(_ x: Span<CInt>) {
// expected-expansion@+9:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func span_nonnull(_ x: UnsafePointer<CInt>, _ len: CInt) {|}}
//   expected-remark@3{{macro content: |    let _count0 = Int(len)|}}
//   expected-remark@4{{macro content: |    precondition(_count0 >= 0, "buffer with negative count")|}}
//   expected-remark@5{{macro content: |    let _safeArg0 = unsafe Span(_unsafeStart: x, count: _count0)|}}
//   expected-remark@6{{macro content: |    span_nonnull(_safeArg0)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
}

@c @implementation
public func span_nullable(_ x: Span<CInt>) {
// expected-expansion@+10:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func span_nullable(_ x: UnsafePointer<CInt>?, _ len: CInt) {|}}
//   expected-remark@3{{macro content: |    let _count0 = Int(len)|}}
//   expected-remark@4{{macro content: |    precondition(_count0 >= 0, "buffer with negative count")|}}
//   expected-remark@5{{macro content: |    precondition(unsafe x != nil || _count0 == 0, "null buffer with non-zero count")|}}
//   expected-remark@6{{macro content: |    let _safeArg0 = if unsafe x != nil { unsafe Span(_unsafeStart: x!, count: _count0) } else { Span<CInt>() }|}}
//   expected-remark@7{{macro content: |    span_nullable(_safeArg0)|}}
//   expected-remark@8{{macro content: |}|}}
// }}
}

// -----------------------------------------------------------------------
// UnsafeBufferPointer + __counted_by_or_null
// -----------------------------------------------------------------------
@c @implementation
public func ubp_ornull_default(_ x: UnsafeBufferPointer<CInt>) {
// expected-expansion@+9:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func ubp_ornull_default(_ x: UnsafePointer<CInt>?, _ len: CInt) {|}}
//   expected-remark@3{{macro content: |    let _count0 = (unsafe x != nil) ? Int(len) : 0|}}
//   expected-remark@4{{macro content: |    precondition(_count0 >= 0, "non-null buffer with negative count")|}}
//   expected-remark@5{{macro content: |    let _safeArg0 = unsafe UnsafeBufferPointer(start: x, count: _count0)|}}
//   expected-remark@6{{macro content: |    unsafe ubp_ornull_default(_safeArg0)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
}

@c @implementation
public func ubp_ornull_nullable(_ x: UnsafeBufferPointer<CInt>?) {
// expected-expansion@+9:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func ubp_ornull_nullable(_ x: UnsafePointer<CInt>?, _ len: CInt) {|}}
//   expected-remark@3{{macro content: |    let _count0 = (unsafe x != nil) ? Int(len) : 0|}}
//   expected-remark@4{{macro content: |    precondition(_count0 >= 0, "non-null buffer with negative count")|}}
//   expected-remark@5{{macro content: |    let _safeArg0 = unsafe x.map { unsafe UnsafeBufferPointer(start: $0, count: _count0) }|}}
//   expected-remark@6{{macro content: |    unsafe ubp_ornull_nullable(_safeArg0)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
}

@c @implementation
public func ubp_ornull_nonnull(_ x: UnsafeBufferPointer<CInt>) {
// expected-expansion@+9:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func ubp_ornull_nonnull(_ x: UnsafePointer<CInt>, _ len: CInt) {|}}
//   expected-remark@3{{macro content: |    let _count0 = Int(len)|}}
//   expected-remark@4{{macro content: |    precondition(_count0 >= 0, "buffer with negative count")|}}
//   expected-remark@5{{macro content: |    let _safeArg0 = unsafe UnsafeBufferPointer(start: x, count: _count0)|}}
//   expected-remark@6{{macro content: |    unsafe ubp_ornull_nonnull(_safeArg0)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
}

// -----------------------------------------------------------------------
// Span + __counted_by_or_null + __noescape
// -----------------------------------------------------------------------
@c @implementation
public func span_ornull_default(_ x: Span<CInt>) {
// expected-expansion@+9:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func span_ornull_default(_ x: UnsafePointer<CInt>?, _ len: CInt) {|}}
//   expected-remark@3{{macro content: |    let _count0 = (unsafe x != nil) ? Int(len) : 0|}}
//   expected-remark@4{{macro content: |    precondition(_count0 >= 0, "non-null buffer with negative count")|}}
//   expected-remark@5{{macro content: |    let _safeArg0 = if unsafe x != nil { unsafe Span(_unsafeStart: x!, count: _count0) } else { Span<CInt>() }|}}
//   expected-remark@6{{macro content: |    span_ornull_default(_safeArg0)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
}

@c @implementation
public func span_ornull_nullable(_ x: Span<CInt>?) {
// expected-expansion@+9:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func span_ornull_nullable(_ x: UnsafePointer<CInt>?, _ len: CInt) {|}}
//   expected-remark@3{{macro content: |    let _count0 = (unsafe x != nil) ? Int(len) : 0|}}
//   expected-remark@4{{macro content: |    precondition(_count0 >= 0, "non-null buffer with negative count")|}}
//   expected-remark@5{{macro content: |    let _safeArg0: Span<CInt>? = if unsafe x != nil { unsafe Span(_unsafeStart: x!, count: _count0) } else { nil }|}}
//   expected-remark@6{{macro content: |    span_ornull_nullable(_safeArg0)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
}

@c @implementation
public func span_ornull_nonnull(_ x: Span<CInt>) {
// expected-expansion@+9:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func span_ornull_nonnull(_ x: UnsafePointer<CInt>, _ len: CInt) {|}}
//   expected-remark@3{{macro content: |    let _count0 = Int(len)|}}
//   expected-remark@4{{macro content: |    precondition(_count0 >= 0, "buffer with negative count")|}}
//   expected-remark@5{{macro content: |    let _safeArg0 = unsafe Span(_unsafeStart: x, count: _count0)|}}
//   expected-remark@6{{macro content: |    span_ornull_nonnull(_safeArg0)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
}

// -----------------------------------------------------------------------
// "Safe" implementations that require `unsafe` when called
// -----------------------------------------------------------------------
@c @implementation
public func result_unsafe_nonnull(_ x: Span<CInt>) -> UnsafePointer<CChar> {
  fatalError()
// expected-expansion@+9:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func result_unsafe_nonnull(_ x: UnsafePointer<CInt>, _ len: CInt) -> UnsafePointer<CChar> {|}}
//   expected-remark@3{{macro content: |    let _count0 = Int(len)|}}
//   expected-remark@4{{macro content: |    precondition(_count0 >= 0, "buffer with negative count")|}}
//   expected-remark@5{{macro content: |    let _safeArg0 = unsafe Span(_unsafeStart: x, count: _count0)|}}
//   expected-remark@6{{macro content: |    return unsafe result_unsafe_nonnull(_safeArg0)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
}

@c @implementation
public func result_unsafe_nullable(_ x: Span<CInt>) -> UnsafePointer<CChar>? {
  fatalError()
// expected-expansion@+9:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func result_unsafe_nullable(_ x: UnsafePointer<CInt>, _ len: CInt) -> UnsafePointer<CChar>? {|}}
//   expected-remark@3{{macro content: |    let _count0 = Int(len)|}}
//   expected-remark@4{{macro content: |    precondition(_count0 >= 0, "buffer with negative count")|}}
//   expected-remark@5{{macro content: |    let _safeArg0 = unsafe Span(_unsafeStart: x, count: _count0)|}}
//   expected-remark@6{{macro content: |    return unsafe result_unsafe_nullable(_safeArg0)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
}

@unsafe @c @implementation
public func explicit_unsafe(_ x: Span<CInt>) {
// expected-expansion@+9:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func explicit_unsafe(_ x: UnsafePointer<CInt>, _ len: CInt) {|}}
//   expected-remark@3{{macro content: |    let _count0 = Int(len)|}}
//   expected-remark@4{{macro content: |    precondition(_count0 >= 0, "buffer with negative count")|}}
//   expected-remark@5{{macro content: |    let _safeArg0 = unsafe Span(_unsafeStart: x, count: _count0)|}}
//   expected-remark@6{{macro content: |    unsafe explicit_unsafe(_safeArg0)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
}

//--- test.h
#define __counted_by(x) __attribute__((__counted_by__(x)))
// expected-note@+1 2{{expanded from macro '__counted_by_or_null'}}
#define __counted_by_or_null(x) __attribute__((__counted_by_or_null__(x)))
#define __noescape __attribute__((noescape))

// UnsafeBufferPointer variants (__counted_by without __noescape):
// expected-expansion@+7:57{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @_disfavoredOverload public func ubp_default(_ x: UnsafeBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: x.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe ubp_default(x.baseAddress, len)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
void ubp_default(const int *__counted_by(len) x, int len);
// expected-expansion@+7:67{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @_disfavoredOverload public func ubp_nonnull(_ x: UnsafeBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: x.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe ubp_nonnull(x.baseAddress!, len)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
void ubp_nonnull(const int * _Nonnull __counted_by(len) x, int len);
// expected-expansion@+7:69{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @_disfavoredOverload public func ubp_nullable(_ x: UnsafeBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: x.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe ubp_nullable(x.baseAddress, len)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
void ubp_nullable(const int * _Nullable __counted_by(len) x, int len);

// Span variants (__counted_by with __noescape):
// expected-expansion@+13:69{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func span_default(_ x: Span<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: x.count)!|}}
//   expected-remark@4{{macro content: |    let _xPtr = x.withUnsafeBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(x)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe span_default(_xPtr.baseAddress, len)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
void span_default(const int *__counted_by(len) __noescape x, int len);
// expected-expansion@+13:79{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func span_nonnull(_ x: Span<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: x.count)!|}}
//   expected-remark@4{{macro content: |    let _xPtr = x.withUnsafeBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(x)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe span_nonnull(_xPtr.baseAddress!, len)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
void span_nonnull(const int * _Nonnull __counted_by(len) __noescape x, int len);
// expected-expansion@+13:81{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func span_nullable(_ x: Span<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: x.count)!|}}
//   expected-remark@4{{macro content: |    let _xPtr = x.withUnsafeBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(x)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe span_nullable(_xPtr.baseAddress, len)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
void span_nullable(const int * _Nullable __counted_by(len) __noescape x, int len);

// UnsafeBufferPointer variants (__counted_by_or_null without __noescape):
// expected-expansion@+7:72{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @_disfavoredOverload public func ubp_ornull_default(_ x: UnsafeBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: x.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe ubp_ornull_default(x.baseAddress, len)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
void ubp_ornull_default(const int *__counted_by_or_null(len) x, int len);
// expected-expansion@+7:84{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @_disfavoredOverload public func ubp_ornull_nullable(_ x: UnsafeBufferPointer<CInt>?) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: unsafe x?.count ?? 0)!|}}
//   expected-remark@4{{macro content: |    return unsafe ubp_ornull_nullable(x?.baseAddress, len)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
void ubp_ornull_nullable(const int * _Nullable __counted_by_or_null(len) x, int len);
// expected-warning@+8{{combining '__counted_by_or_null' and '_Nonnull'; did you mean '__counted_by' instead?}}
// expected-expansion@+7:82{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @_disfavoredOverload public func ubp_ornull_nonnull(_ x: UnsafeBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: x.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe ubp_ornull_nonnull(x.baseAddress!, len)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
void ubp_ornull_nonnull(const int * _Nonnull __counted_by_or_null(len) x, int len);

// Span variants (__counted_by_or_null with __noescape):
// expected-expansion@+13:84{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func span_ornull_default(_ x: Span<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: x.count)!|}}
//   expected-remark@4{{macro content: |    let _xPtr = x.withUnsafeBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(x)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe span_ornull_default(_xPtr.baseAddress, len)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
void span_ornull_default(const int *__counted_by_or_null(len) __noescape x, int len);
// expected-expansion@+13:96{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func span_ornull_nullable(_ x: Span<CInt>?) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: x?.count ?? 0)!|}}
//   expected-remark@4{{macro content: |    let _xPtr = x?.withUnsafeBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(x)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe span_ornull_nullable(_xPtr?.baseAddress, len)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
void span_ornull_nullable(const int * _Nullable __counted_by_or_null(len) __noescape x, int len);
// expected-warning@+14{{combining '__counted_by_or_null' and '_Nonnull'; did you mean '__counted_by' instead?}}
// expected-expansion@+13:94{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func span_ornull_nonnull(_ x: Span<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: x.count)!|}}
//   expected-remark@4{{macro content: |    let _xPtr = x.withUnsafeBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(x)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe span_ornull_nonnull(_xPtr.baseAddress!, len)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
void span_ornull_nonnull(const int * _Nonnull __counted_by_or_null(len) __noescape x, int len);

// expected-expansion@+13:105{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func result_unsafe_nonnull(_ x: Span<CInt>) -> UnsafePointer<CChar> {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: x.count)!|}}
//   expected-remark@4{{macro content: |    let _xPtr = x.withUnsafeBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(x)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe result_unsafe_nonnull(_xPtr.baseAddress!, len)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
const char * _Nonnull result_unsafe_nonnull(const int * _Nonnull __counted_by(len) __noescape x, int len);
// expected-expansion@+13:107{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func result_unsafe_nullable(_ x: Span<CInt>) -> UnsafePointer<CChar>? {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: x.count)!|}}
//   expected-remark@4{{macro content: |    let _xPtr = x.withUnsafeBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(x)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe result_unsafe_nullable(_xPtr.baseAddress!, len)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
const char * _Nullable result_unsafe_nullable(const int * _Nonnull __counted_by(len) __noescape x, int len);

// A function that is explicitly `@unsafe` in Swift must be called with `unsafe`
// even though its only argument is a `Span`.
// expected-expansion@+13:82{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func explicit_unsafe(_ x: Span<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: x.count)!|}}
//   expected-remark@4{{macro content: |    let _xPtr = x.withUnsafeBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(x)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe explicit_unsafe(_xPtr.baseAddress!, len)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
void explicit_unsafe(const int * _Nonnull __counted_by(len) __noescape x, int len);
