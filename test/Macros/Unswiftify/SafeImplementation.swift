// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_SafeInteropImplementations
// REQUIRES: swift_feature_SafeInteropWrappers

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/test.swift -emit-module -plugin-path %swift-plugin-dir -import-bridging-header %t/test.h -verify -Rmacro-expansions -enable-experimental-feature SafeInteropImplementations -enable-experimental-feature SafeInteropWrappers -disable-objc-interop -suppress-notes -verify-ignore-unrelated

//--- test.h
#define __counted_by(x) __attribute__((__counted_by__(x)))
#define __noescape __attribute__((noescape))
void foo(const int *__counted_by(len) __noescape x, int len);

//--- test.swift
// `@c @implementation(safe)` matches `foo` against the C header declaration,
// collects the `__counted_by(len)` + `__noescape` annotations, and attaches
// a synthesized `@_Unswiftify` invocation. The macro then expands into a
// C-callable peer that takes the unsafe `(UnsafePointer<Int32>?, Int32)`
// signature (the C pointer is nullable because the header lacks `_Nonnull`)
// and forwards to the safe `Span<CInt>` original after force-unwrapping.
//
// The `_SwiftifyImport`-generated `Span` overload on the imported C decl
// matches the safe Swift function's parameter list exactly, so no signature
// mismatch is diagnosed; that peer is marked universally unavailable so
// overload resolution picks the user's safe implementation.
@c @implementation(safe)
public func foo(_ x: Span<CInt>) {
// expected-expansion@+7:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func foo(_ x: UnsafePointer<CInt>?, _ len: CInt) {|}}
//   expected-remark@3{{macro content: |    let _ptr0 = if x != nil { Span(_unsafeStart: x!, count: Int(len)) } else { Span<CInt>() }|}}
//   expected-remark@4{{macro content: |    foo(_ptr0)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
}
