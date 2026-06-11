// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_SafeInteropImplementations

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-verify-unswiftify %t/test.swift -import-bridging-header %t/test.h -verify-additional-file %t/test.h -verify-additional-prefix on- -enable-experimental-feature SafeInteropImplementations
// RUN: %target-verify-unswiftify %t/test.swift -import-bridging-header %t/test.h -verify-additional-file %t/test.h -verify-additional-prefix off-

//--- test.swift
// expected-off-error@+1{{'@implementation' of global function 'bar' with a safe-interop parameter or result type requires the experimental feature 'SafeInteropImplementations'}}
@c @implementation
public func bar(_ x: UnsafeBufferPointer<CInt>) {
// expected-on-expansion@+10:2{{
//   expected-on-remark@1{{macro content: |@c @implementation|}}
//   expected-on-remark@2{{macro content: |public func bar(_ x: UnsafePointer<CInt>?, _ len: CInt) {|}}
//   expected-on-remark@3{{macro content: |    let _count0 = Int(len)|}}
//   expected-on-remark@4{{macro content: |    precondition(_count0 >= 0, "buffer with negative count")|}}
//   expected-on-remark@5{{macro content: |    precondition(unsafe x != nil || _count0 == 0, "null buffer with non-zero count")|}}
//   expected-on-remark@6{{macro content: |    let _safeArg0 = unsafe UnsafeBufferPointer(start: x, count: _count0)|}}
//   expected-on-remark@7{{macro content: |    unsafe bar(_safeArg0)|}}
//   expected-on-remark@8{{macro content: |}|}}
// }}
}

//--- test.h
#define __counted_by(x) __attribute__((__counted_by__(x)))
// expected-expansion@+7:49{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @_disfavoredOverload public func bar(_ x: UnsafeBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: x.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe bar(x.baseAddress, len)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
void bar(const int *__counted_by(len) x, int len);
