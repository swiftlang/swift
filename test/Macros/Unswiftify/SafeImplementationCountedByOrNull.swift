// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_SafeInteropImplementations
// REQUIRES: swift_feature_SafeInteropWrappers

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/test.swift -emit-module -plugin-path %swift-plugin-dir -import-bridging-header %t/test.h -verify -Rmacro-expansions -enable-experimental-feature SafeInteropImplementations -enable-experimental-feature SafeInteropWrappers -disable-objc-interop -suppress-notes -verify-ignore-unrelated

//--- test.h
#define __counted_by_or_null(x) __attribute__((__counted_by_or_null__(x)))
#define __noescape __attribute__((noescape))

// UnsafeBufferPointer variants (__counted_by_or_null without __noescape):
void ubp_ornull_default(const int *__counted_by_or_null(len) x, int len);
void ubp_ornull_nullable(const int * _Nullable __counted_by_or_null(len) x, int len);
void ubp_ornull_nonnull(const int * _Nonnull __counted_by_or_null(len) x, int len);

// Span variants (__counted_by_or_null with __noescape):
void span_ornull_default(const int *__counted_by_or_null(len) __noescape x, int len);
void span_ornull_nullable(const int * _Nullable __counted_by_or_null(len) __noescape x, int len);
void span_ornull_nonnull(const int * _Nonnull __counted_by_or_null(len) __noescape x, int len);

//--- test.swift

// -----------------------------------------------------------------------
// UnsafeBufferPointer + __counted_by_or_null
// -----------------------------------------------------------------------

// Default nullable, non-Optional safe param: UBP(start:count:) accepts
// optional natively, nil+0 is a valid empty buffer.
@c @implementation(safe)
public func ubp_ornull_default(_ x: UnsafeBufferPointer<CInt>) {
// expected-expansion@+7:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func ubp_ornull_default(_ x: UnsafePointer<CInt>?, _ len: CInt) {|}}
//   expected-remark@3{{macro content: |    let _ptr0 = UnsafeBufferPointer(start: x, count: Int(len))|}}
//   expected-remark@4{{macro content: |    ubp_ornull_default(_ptr0)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
}

// Explicitly _Nullable, Optional safe param: nil maps to nil (not an
// empty buffer). Uses `.map` to produce Optional<UBP>.
@c @implementation(safe)
public func ubp_ornull_nullable(_ x: UnsafeBufferPointer<CInt>?) {
// expected-expansion@+7:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func ubp_ornull_nullable(_ x: UnsafePointer<CInt>?, _ len: CInt) {|}}
//   expected-remark@3{{macro content: |    let _ptr0 = x.map { UnsafeBufferPointer(start: $0, count: Int(len)) }|}}
//   expected-remark@4{{macro content: |    ubp_ornull_nullable(_ptr0)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
}

// _Nonnull, non-Optional safe param: pointer is never nil.
@c @implementation(safe)
public func ubp_ornull_nonnull(_ x: UnsafeBufferPointer<CInt>) {
// expected-expansion@+7:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func ubp_ornull_nonnull(_ x: UnsafePointer<CInt>, _ len: CInt) {|}}
//   expected-remark@3{{macro content: |    let _ptr0 = UnsafeBufferPointer(start: x, count: Int(len))|}}
//   expected-remark@4{{macro content: |    ubp_ornull_nonnull(_ptr0)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
}

// -----------------------------------------------------------------------
// Span + __counted_by_or_null + __noescape
// -----------------------------------------------------------------------

// Default nullable, non-Optional safe param: nil-guard produces empty Span.
@c @implementation(safe)
public func span_ornull_default(_ x: Span<CInt>) {
// expected-expansion@+7:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func span_ornull_default(_ x: UnsafePointer<CInt>?, _ len: CInt) {|}}
//   expected-remark@3{{macro content: |    let _ptr0 = if x != nil { Span(_unsafeStart: x!, count: Int(len)) } else { Span<CInt>() }|}}
//   expected-remark@4{{macro content: |    span_ornull_default(_ptr0)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
}

// Explicitly _Nullable, Optional safe param: nil maps to nil.
@c @implementation(safe)
public func span_ornull_nullable(_ x: Span<CInt>?) {
// expected-expansion@+7:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func span_ornull_nullable(_ x: UnsafePointer<CInt>?, _ len: CInt) {|}}
//   expected-remark@3{{macro content: |    let _ptr0: Span? = if x != nil { Span(_unsafeStart: x!, count: Int(len)) } else { nil }|}}
//   expected-remark@4{{macro content: |    span_ornull_nullable(_ptr0)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
}

// _Nonnull, non-Optional safe param: pointer is never nil.
@c @implementation(safe)
public func span_ornull_nonnull(_ x: Span<CInt>) {
// expected-expansion@+7:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func span_ornull_nonnull(_ x: UnsafePointer<CInt>, _ len: CInt) {|}}
//   expected-remark@3{{macro content: |    let _ptr0 = Span(_unsafeStart: x, count: Int(len))|}}
//   expected-remark@4{{macro content: |    span_ornull_nonnull(_ptr0)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
}
