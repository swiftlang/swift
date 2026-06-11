// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_SafeInteropImplementations

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-verify-unswiftify %t/test.swift -import-bridging-header %t/test.h -verify-additional-file %t/test.h -enable-experimental-feature SafeInteropImplementations

//--- test.swift
@c @implementation
// expected-expansion@+14:102{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func param_name_collision(_ _safeArg0: UnsafePointer<CInt>?, _ _safeArg1: UnsafePointer<CInt>?, _ _count0: CInt, _ _count1: CInt) {|}}
//   expected-remark@3{{macro content: |    let _count0_ = Int(_count0)|}}
//   expected-remark@4{{macro content: |    precondition(_count0_ >= 0, "buffer with negative count")|}}
//   expected-remark@5{{macro content: |    precondition(unsafe _safeArg0 != nil || _count0_ == 0, "null buffer with non-zero count")|}}
//   expected-remark@6{{macro content: |    let _safeArg0_ = unsafe UnsafeBufferPointer(start: _safeArg0, count: _count0_)|}}
//   expected-remark@7{{macro content: |    let _count1_ = Int(_count1)|}}
//   expected-remark@8{{macro content: |    precondition(_count1_ >= 0, "buffer with negative count")|}}
//   expected-remark@9{{macro content: |    precondition(unsafe _safeArg1 != nil || _count1_ == 0, "null buffer with non-zero count")|}}
//   expected-remark@10{{macro content: |    let _safeArg1_ = unsafe UnsafeBufferPointer(start: _safeArg1, count: _count1_)|}}
//   expected-remark@11{{macro content: |    unsafe param_name_collision(_safeArg0_, _safeArg1_)|}}
//   expected-remark@12{{macro content: |}|}}
// }}
public func param_name_collision(_ p1: UnsafeBufferPointer<CInt>, _ p2: UnsafeBufferPointer<CInt>) {}

//--- test.h
#define __counted_by(x) __attribute__((__counted_by__(x)))

// expected-expansion@+8:139{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @_disfavoredOverload public func param_name_collision(_ _safeArg0: UnsafeBufferPointer<CInt>, _ _safeArg1: UnsafeBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    let _count0 = CInt(exactly: _safeArg0.count)!|}}
//   expected-remark@4{{macro content: |    let _count1 = CInt(exactly: _safeArg1.count)!|}}
//   expected-remark@5{{macro content: |    return unsafe param_name_collision(_safeArg0.baseAddress, _safeArg1.baseAddress, _count0, _count1)|}}
//   expected-remark@6{{macro content: |}|}}
// }}
void param_name_collision(const int *__counted_by(_count0) _safeArg0, const int *__counted_by(_count1) _safeArg1, int _count0, int _count1);
