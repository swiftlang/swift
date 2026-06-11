// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_SafeInteropImplementations
// REQUIRES: swift_feature_SafeInteropWrappers

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/test.swift -emit-module -plugin-path %swift-plugin-dir -import-bridging-header %t/test.h -verify -Rmacro-expansions -enable-experimental-feature SafeInteropImplementations -enable-experimental-feature SafeInteropWrappers -disable-objc-interop -suppress-notes -verify-ignore-unrelated

//--- test.h
#define __sized_by(x) __attribute__((__sized_by__(x)))
#define __noescape __attribute__((noescape))
// A *typed* (non-`void`) pointer with `__sized_by` maps to a raw byte span.
// `__sized_by` byte spans are never generic, so the empty-span fallback must
// emit `RawSpan()`, not `RawSpan<CChar>()`. This regression-tests that
// `SafeCollectionType::forParam` clears the pointee in its `sizedBy` branch
// (otherwise `typeName()` asserts `raw != bool(pointee)` and emits invalid
// `RawSpan<CChar>` Swift).
void byte_span(const char * _Nullable __sized_by(n) __noescape p, int n);

//--- test.swift
@c @implementation(safe)
public func byte_span(_ p: RawSpan?) {
// expected-expansion@+7:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func byte_span(_ p: UnsafePointer<CChar>?, _ n: CInt) {|}}
//   expected-remark@3{{macro content: |    let _ptr0: RawSpan? = if p != nil { RawSpan(_unsafeStart: p!, byteCount: Int(n)) } else { nil }|}}
//   expected-remark@4{{macro content: |    byte_span(_ptr0)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
}
