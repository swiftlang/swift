// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_SafeInteropImplementations

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/test.swift -emit-module -plugin-path %swift-plugin-dir -import-bridging-header %t/test.h -verify -Rmacro-expansions -enable-experimental-feature SafeInteropImplementations -disable-objc-interop -suppress-notes -verify-ignore-unrelated

//--- test.h
#define __counted_by(x) __attribute__((__counted_by__(x)))
// `guard` and `where` are valid C identifiers but Swift keywords. The
// generated unsafe peer references them in identifier position (the parameter
// list, the reconstructed buffer's base pointer, and the count expression), so
// each must be backtick-escaped or the synthesized Swift fails to reparse.
void kwtest(const int *__counted_by(where) guard, int where);

//--- test.swift
@c @implementation(safe)
public func kwtest(_ x: UnsafeBufferPointer<CInt>) {
// expected-expansion@+7:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func kwtest(_ guard: UnsafePointer<CInt>?, _ where: CInt) {|}}
//   expected-remark@3{{macro content: |    let _ptr0 = UnsafeBufferPointer(start: `guard`, count: Int(`where`))|}}
//   expected-remark@4{{macro content: |    kwtest(_ptr0)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
}
