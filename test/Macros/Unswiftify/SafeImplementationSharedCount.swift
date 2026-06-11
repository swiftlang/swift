// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_SafeInteropImplementations

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/test.swift -emit-module -plugin-path %swift-plugin-dir -import-bridging-header %t/test.h -verify -Rmacro-expansions -enable-experimental-feature SafeInteropImplementations -disable-objc-interop -suppress-notes -verify-ignore-unrelated

//--- test.h
#define __counted_by(x) __attribute__((__counted_by__(x)))
// A single count parameter `len` shared by two `__counted_by(len)` pointers.
// Both pointers must be reconstructed from the same count, and `len` is
// dropped from the forwarding call.
void shared(const int *__counted_by(len) p1, const int *__counted_by(len) p2, int len);

//--- test.swift
@c @implementation(safe)
public func shared(_ p1: UnsafeBufferPointer<CInt>, _ p2: UnsafeBufferPointer<CInt>) {
// expected-expansion@+8:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func shared(_ p1: UnsafePointer<CInt>?, _ p2: UnsafePointer<CInt>?, _ len: CInt) {|}}
//   expected-remark@3{{macro content: |    let _ptr0 = UnsafeBufferPointer(start: p1, count: Int(len))|}}
//   expected-remark@4{{macro content: |    let _ptr1 = UnsafeBufferPointer(start: p2, count: Int(len))|}}
//   expected-remark@5{{macro content: |    shared(_ptr0, _ptr1)|}}
//   expected-remark@6{{macro content: |}|}}
// }}
}
