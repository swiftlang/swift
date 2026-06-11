// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_SafeInteropImplementations

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-verify-unswiftify %t/test.swift -import-bridging-header %t/test.h -verify-additional-file %t/test.h -enable-experimental-feature SafeInteropImplementations

//--- test.swift
@c @implementation
// expected-expansion@+10:51{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func foo(_ x: UnsafePointer<CInt>?, _ len: CInt) {|}}
//   expected-remark@3{{macro content: |    let _count0 = Int(len)|}}
//   expected-remark@4{{macro content: |    precondition(_count0 >= 0, "buffer with negative count")|}}
//   expected-remark@5{{macro content: |    precondition(unsafe x != nil || _count0 == 0, "null buffer with non-zero count")|}}
//   expected-remark@6{{macro content: |    let _safeArg0 = unsafe UnsafeBufferPointer(start: x, count: _count0)|}}
//   expected-remark@7{{macro content: |    unsafe foo(_safeArg0)|}}
//   expected-remark@8{{macro content: |}|}}
// }}
public func foo(_ x: UnsafeBufferPointer<CInt>) {}

// `_Unswiftify` is a compiler-internal macro. Even after the MacroDecl has been
// created, it cannot be found using name lookup, so users can't invoke it.
@_Unswiftify // expected-error{{unknown attribute '_Unswiftify'}}
public func bar(_ x: UnsafeBufferPointer<CInt>) {}

//--- test.h
#define __counted_by(x) __attribute__((__counted_by__(x)))
// expected-expansion@+7:49{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @_disfavoredOverload public func foo(_ x: UnsafeBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: x.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe foo(x.baseAddress, len)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
void foo(const int *__counted_by(len) x, int len);
