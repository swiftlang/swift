// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_SafeInteropImplementations
// REQUIRES: swift_feature_SafeInteropWrappers

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/test.swift -emit-module -plugin-path %swift-plugin-dir -import-bridging-header %t/test.h -verify -Rmacro-expansions -enable-experimental-feature SafeInteropImplementations -enable-experimental-feature SafeInteropWrappers -disable-objc-interop -suppress-notes -verify-ignore-unrelated

//--- test.h
#define __counted_by(x) __attribute__((__counted_by__(x)))
#define __noescape __attribute__((noescape))

// UnsafeBufferPointer variants (__counted_by without __noescape):
void ubp_default(const int *__counted_by(len) x, int len);
void ubp_nonnull(const int * _Nonnull __counted_by(len) x, int len);
void ubp_nullable(const int * _Nullable __counted_by(len) x, int len);

// Span variants (__counted_by with __noescape):
void span_default(const int *__counted_by(len) __noescape x, int len);
void span_nonnull(const int * _Nonnull __counted_by(len) __noescape x, int len);
void span_nullable(const int * _Nullable __counted_by(len) __noescape x, int len);

//--- test.swift

// -----------------------------------------------------------------------
// UnsafeBufferPointer variants (no __noescape on the C declaration)
//
// UnsafeBufferPointer(start:count:) accepts an optional pointer natively,
// so no unwrapping or nil-guarding is needed for any nullability variant.
// A nil pointer with count 0 produces a valid empty buffer.
// -----------------------------------------------------------------------

// No nullability annotation (default nullable → optional pointer)
@c @implementation(safe)
public func ubp_default(_ x: UnsafeBufferPointer<CInt>) {
// expected-expansion@+7:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func ubp_default(_ x: UnsafePointer<CInt>?, _ len: CInt) {|}}
//   expected-remark@3{{macro content: |    let _ptr0 = UnsafeBufferPointer(start: x, count: Int(len))|}}
//   expected-remark@4{{macro content: |    ubp_default(_ptr0)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
}

// _Nonnull → non-optional pointer
@c @implementation(safe)
public func ubp_nonnull(_ x: UnsafeBufferPointer<CInt>) {
// expected-expansion@+7:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func ubp_nonnull(_ x: UnsafePointer<CInt>, _ len: CInt) {|}}
//   expected-remark@3{{macro content: |    let _ptr0 = UnsafeBufferPointer(start: x, count: Int(len))|}}
//   expected-remark@4{{macro content: |    ubp_nonnull(_ptr0)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
}

// _Nullable → optional pointer
@c @implementation(safe)
public func ubp_nullable(_ x: UnsafeBufferPointer<CInt>?) {
// expected-expansion@+7:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func ubp_nullable(_ x: UnsafePointer<CInt>?, _ len: CInt) {|}}
//   expected-remark@3{{macro content: |    let _ptr0 = x.map { UnsafeBufferPointer(start: $0, count: Int(len)) }|}}
//   expected-remark@4{{macro content: |    ubp_nullable(_ptr0)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
}

// -----------------------------------------------------------------------
// Span variants (__noescape on the C declaration)
//
// Span's _unsafeStart initializer requires a non-optional pointer.
// For _Nonnull pointers the pointer is passed directly. For optional
// pointers (default nullable or _Nullable) a temp variable is emitted
// with an `if let` that constructs the Span when non-nil and falls
// back to an empty Span when nil, avoiding a force-unwrap trap when
// the C caller legitimately passes (nil, 0).
// -----------------------------------------------------------------------

// No nullability annotation (default nullable → optional pointer, nil-guarded)
@c @implementation(safe)
public func span_default(_ x: Span<CInt>) {
// expected-expansion@+7:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func span_default(_ x: UnsafePointer<CInt>?, _ len: CInt) {|}}
//   expected-remark@3{{macro content: |    let _ptr0 = if x != nil { Span(_unsafeStart: x!, count: Int(len)) } else { Span<CInt>() }|}}
//   expected-remark@4{{macro content: |    span_default(_ptr0)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
}

// _Nonnull → non-optional pointer, passed directly
@c @implementation(safe)
public func span_nonnull(_ x: Span<CInt>) {
// expected-expansion@+7:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func span_nonnull(_ x: UnsafePointer<CInt>, _ len: CInt) {|}}
//   expected-remark@3{{macro content: |    let _ptr0 = Span(_unsafeStart: x, count: Int(len))|}}
//   expected-remark@4{{macro content: |    span_nonnull(_ptr0)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
}

// _Nullable → optional pointer, nil-guarded
@c @implementation(safe)
public func span_nullable(_ x: Span<CInt>?) {
// expected-expansion@+7:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func span_nullable(_ x: UnsafePointer<CInt>?, _ len: CInt) {|}}
//   expected-remark@3{{macro content: |    let _ptr0: Span? = if x != nil { Span(_unsafeStart: x!, count: Int(len)) } else { nil }|}}
//   expected-remark@4{{macro content: |    span_nullable(_ptr0)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
}
