// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_SafeInteropImplementations

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend %t/test.swift -emit-module -plugin-path %swift-plugin-dir -import-bridging-header %t/test.h -verify -Rmacro-expansions -enable-experimental-feature SafeInteropImplementations -disable-objc-interop -verify-additional-file %t/test.h -suppress-notes -eager-macro-checking

//--- test.h
#define __sized_by(x) __attribute__((__sized_by__(x)))
#define __counted_by(x) __attribute__((__counted_by__(x)))
// A raw `void *` pointer with a compound byte-count expression `size * len`
// referencing two parameters. Because the count is not a lone parameter
// reference, neither `size` nor `len` is dropped from the forwarding call.
// expected-expansion@+9:64{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func compound(_ p: UnsafeMutableRawBufferPointer, _ size: CInt, _ len: CInt) {|}}
//   expected-remark@3{{macro content: |    if p.count != (size * len) {|}}
//   expected-remark@4{{macro content: |      fatalError("bounds check failure in compound: expected \\((size * len)) but got \\(p.count)")|}}
//   expected-remark@5{{macro content: |    }|}}
//   expected-remark@6{{macro content: |    return unsafe compound(p.baseAddress, size, len)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
void compound(void *__sized_by(size * len) p, int size, int len);

// expected-expansion@+10:104{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func compound_half_shared(_ p1: UnsafeMutableRawBufferPointer, _ size: CInt, _ p2: UnsafeMutableBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: p2.count)!|}}
//   expected-remark@4{{macro content: |    if p1.count != (size * len) {|}}
//   expected-remark@5{{macro content: |      fatalError("bounds check failure in compound_half_shared: expected \\((size * len)) but got \\(p1.count)")|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    return unsafe compound_half_shared(p1.baseAddress, size, len, p2.baseAddress)|}}
//   expected-remark@8{{macro content: |}|}}
// }}
void compound_half_shared(void *__sized_by(size * len) p1, int size, int len, int *__counted_by(len) p2);

// expected-expansion@+9:45{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func neg(_ p: UnsafeMutableRawBufferPointer, _ size: CInt) {|}}
//   expected-remark@3{{macro content: |    if p.count != -size {|}}
//   expected-remark@4{{macro content: |      fatalError("bounds check failure in neg: expected \\(-size) but got \\(p.count)")|}}
//   expected-remark@5{{macro content: |    }|}}
//   expected-remark@6{{macro content: |    return unsafe neg(p.baseAddress, size)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
void neg(void *__sized_by(-size) p, int size);

// expected-expansion@+9:54{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func neg_mismatch(_ p: UnsafeMutableRawBufferPointer, _ size: CInt) {|}}
//   expected-remark@3{{macro content: |    if p.count != -size {|}}
//   expected-remark@4{{macro content: |      fatalError("bounds check failure in neg_mismatch: expected \\(-size) but got \\(p.count)")|}}
//   expected-remark@5{{macro content: |    }|}}
//   expected-remark@6{{macro content: |    return unsafe neg_mismatch(p.baseAddress, size)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
void neg_mismatch(void *__sized_by(-size) p, int size);

//--- test.swift
@c @implementation(safe)
public func compound(_ p: UnsafeMutableRawBufferPointer, _ size: CInt, _ len: CInt) {
// expected-expansion@+7:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func compound(_ p: UnsafeMutableRawPointer?, _ size: CInt, _ len: CInt) {|}}
//   expected-remark@3{{macro content: |    let _ptr0 = UnsafeMutableRawBufferPointer(start: p, count: Int((size * len)))|}}
//   expected-remark@4{{macro content: |    compound(_ptr0, size, len)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
}

@c @implementation(safe)
public func compound_half_shared(_ p: UnsafeMutableRawBufferPointer, _ size: CInt, _ p2: UnsafeMutableBufferPointer<CInt>) {
// expected-expansion@+8:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func compound_half_shared(_ p1: UnsafeMutableRawPointer?, _ size: CInt, _ len: CInt, _ p2: UnsafeMutablePointer<CInt>?) {|}}
//   expected-remark@3{{macro content: |    let _ptr0 = UnsafeMutableRawBufferPointer(start: p1, count: Int((size * len)))|}}
//   expected-remark@4{{macro content: |    let _ptr2 = UnsafeMutableBufferPointer(start: p2, count: Int(len))|}}
//   expected-remark@5{{macro content: |    compound_half_shared(_ptr0, size, _ptr2)|}}
//   expected-remark@6{{macro content: |}|}}
// }}
}

@c @implementation(safe)
public func neg(_ p: UnsafeMutableRawBufferPointer, _ size: CInt) {
// expected-expansion@+7:2{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func neg(_ p: UnsafeMutableRawPointer?, _ size: CInt) {|}}
//   expected-remark@3{{macro content: |    let _ptr0 = UnsafeMutableRawBufferPointer(start: p, count: Int(-size))|}}
//   expected-remark@4{{macro content: |    neg(_ptr0, size)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
}

@c @implementation(safe)
// expected-error@+1{{'@implementation(safe)' global function 'neg_mismatch' has 1 parameter(s), but the safe wrapper generated by '@_SwiftifyImport' on the matching C declaration has 2 parameter(s)}}
public func neg_mismatch(_ p: UnsafeMutableRawBufferPointer) {
}
