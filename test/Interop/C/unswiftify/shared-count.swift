// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_SafeInteropImplementations

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-verify-unswiftify %t/test.swift -import-bridging-header %t/test.h -verify-additional-file %t/test.h -enable-experimental-feature SafeInteropImplementations

//--- test.swift
@c @implementation
// expected-expansion@+14:88{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func shared(_ p1: UnsafePointer<CInt>?, _ p2: UnsafePointer<CInt>?, _ len: CInt) {|}}
//   expected-remark@3{{macro content: |    let _count0 = Int(len)|}}
//   expected-remark@4{{macro content: |    precondition(_count0 >= 0, "buffer with negative count")|}}
//   expected-remark@5{{macro content: |    precondition(unsafe p1 != nil || _count0 == 0, "null buffer with non-zero count")|}}
//   expected-remark@6{{macro content: |    let _safeArg0 = unsafe UnsafeBufferPointer(start: p1, count: _count0)|}}
//   expected-remark@7{{macro content: |    let _count1 = Int(len)|}}
//   expected-remark@8{{macro content: |    precondition(_count1 >= 0, "buffer with negative count")|}}
//   expected-remark@9{{macro content: |    precondition(unsafe p2 != nil || _count1 == 0, "null buffer with non-zero count")|}}
//   expected-remark@10{{macro content: |    let _safeArg1 = unsafe UnsafeBufferPointer(start: p2, count: _count1)|}}
//   expected-remark@11{{macro content: |    unsafe shared(_safeArg0, _safeArg1)|}}
//   expected-remark@12{{macro content: |}|}}
// }}
public func shared(_ p1: UnsafeBufferPointer<CInt>, _ p2: UnsafeBufferPointer<CInt>) {}

//--- test.h
#define __counted_by(x) __attribute__((__counted_by__(x)))
// expected-expansion@+16:86{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @_disfavoredOverload public func shared(_ p1: UnsafeBufferPointer<CInt>, _ p2: UnsafeBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: p2.count)!|}}
//   expected-remark@4{{macro content: |    if p1.count != len {|}}
//   expected-remark@5{{macro content: |      @inline(never) func _boundsCheckFailure<E: BinaryInteger, A: BinaryInteger>(_ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@6{{macro content: |        @inline(never) func _fail(_ function: StaticString, _ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@7{{macro content: |          fatalError("bounds check failure in \\(function): expected \\(expected) but got \\(actual)")|}}
//   expected-remark@8{{macro content: |        }|}}
//   expected-remark@9{{macro content: |        _fail("shared", expected, actual)|}}
//   expected-remark@10{{macro content: |      }|}}
//   expected-remark@11{{macro content: |      _boundsCheckFailure(len, p1.count)|}}
//   expected-remark@12{{macro content: |    }|}}
//   expected-remark@13{{macro content: |    return unsafe shared(p1.baseAddress, p2.baseAddress, len)|}}
//   expected-remark@14{{macro content: |}|}}
// }}
void shared(const int *__counted_by(len) p1, const int *__counted_by(len) p2, int len);
