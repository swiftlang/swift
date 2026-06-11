// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_SafeInteropImplementations

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-verify-unswiftify %t/test.swift -import-bridging-header %t/test.h -verify-additional-file %t/test.h -enable-experimental-feature SafeInteropImplementations

//--- test.swift
@c @implementation
// expected-expansion@+10:53{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func param(_ guard: UnsafePointer<CInt>?, _ where: CInt) {|}}
//   expected-remark@3{{macro content: |    let _count0 = Int(`where`)|}}
//   expected-remark@4{{macro content: |    precondition(_count0 >= 0, "buffer with negative count")|}}
//   expected-remark@5{{macro content: |    precondition(unsafe `guard` != nil || _count0 == 0, "null buffer with non-zero count")|}}
//   expected-remark@6{{macro content: |    let _safeArg0 = unsafe UnsafeBufferPointer(start: `guard`, count: _count0)|}}
//   expected-remark@7{{macro content: |    unsafe param(_safeArg0)|}}
//   expected-remark@8{{macro content: |}|}}
// }}
public func param(_ x: UnsafeBufferPointer<CInt>) {}

// FIXME: don't try to look up an invalid name
// expected-error@+1{{could not find imported function 'func#' matching global function 'func#'; make sure you import the module or header that declares it}}
@c @implementation
// expected-note@+2{{if this name is unavoidable, use backticks to escape it}}
// expected-error@+1{{keyword 'func' cannot be used as an identifier here}}
public func func(_ x: UnsafeBufferPointer<CInt>) {}

@c @implementation
// expected-expansion@+10:54{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func `func`(_ p: UnsafePointer<CInt>?, _ len: CInt) {|}}
//   expected-remark@3{{macro content: |    let _count0 = Int(len)|}}
//   expected-remark@4{{macro content: |    precondition(_count0 >= 0, "buffer with negative count")|}}
//   expected-remark@5{{macro content: |    precondition(unsafe p != nil || _count0 == 0, "null buffer with non-zero count")|}}
//   expected-remark@6{{macro content: |    let _safeArg0 = unsafe UnsafeBufferPointer(start: p, count: _count0)|}}
//   expected-remark@7{{macro content: |    unsafe `func`(_safeArg0)|}}
//   expected-remark@8{{macro content: |}|}}
// }}
public func `func`(_ x: UnsafeBufferPointer<CInt>) {}

// 'async' is not a full keyword; it's only reserved in some contexts.
@c @implementation
// expected-expansion@+10:86{{
//   expected-remark@1{{macro content: |@c @implementation|}}
//   expected-remark@2{{macro content: |public func compound(_ in: UnsafePointer<CInt>?, _ let: CInt, _ async: CInt) {|}}
//   expected-remark@3{{macro content: |    let _count0 = Int((`let` * async))|}}
//   expected-remark@4{{macro content: |    precondition(_count0 >= 0, "buffer with negative count")|}}
//   expected-remark@5{{macro content: |    precondition(unsafe `in` != nil || _count0 == 0, "null buffer with non-zero count")|}}
//   expected-remark@6{{macro content: |    let _safeArg0 = unsafe UnsafeBufferPointer(start: `in`, count: _count0)|}}
//   expected-remark@7{{macro content: |    unsafe compound(_safeArg0, `let`, async)|}}
//   expected-remark@8{{macro content: |}|}}
// }}
public func compound(_ x: UnsafeBufferPointer<CInt>, _ let: CInt, _ `async`: CInt) {}

//--- test.h
#define __counted_by(x) __attribute__((__counted_by__(x)))

// expected-expansion@+7:59{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @_disfavoredOverload public func param(_ `guard`: UnsafeBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    let `where` = CInt(exactly: `guard`.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe param(`guard`.baseAddress, `where`)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
void param(const int *__counted_by(where) guard, int where);

// expected-expansion@+7:50{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @_disfavoredOverload public func `func`(_ p: UnsafeBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe `func`(p.baseAddress, len)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
void func(const int *__counted_by(len) p, int len);

// expected-expansion@+15:74{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @inline(always) @_disfavoredOverload public func compound(_ `in`: UnsafeBufferPointer<CInt>, _ `let`: CInt, _ async: CInt) {|}}
//   expected-remark@3{{macro content: |    if `in`.count != (`let` * async) {|}}
//   expected-remark@4{{macro content: |      @inline(never) func _boundsCheckFailure<E: BinaryInteger, A: BinaryInteger>(_ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@5{{macro content: |        @inline(never) func _fail(_ function: StaticString, _ expected: E, _ actual: A) -> Never {|}}
//   expected-remark@6{{macro content: |          fatalError("bounds check failure in \\(function): expected \\(expected) but got \\(actual)")|}}
//   expected-remark@7{{macro content: |        }|}}
//   expected-remark@8{{macro content: |        _fail("compound", expected, actual)|}}
//   expected-remark@9{{macro content: |      }|}}
//   expected-remark@10{{macro content: |      _boundsCheckFailure((`let` * async), `in`.count)|}}
//   expected-remark@11{{macro content: |    }|}}
//   expected-remark@12{{macro content: |    return unsafe compound(`in`.baseAddress, `let`, async)|}}
//   expected-remark@13{{macro content: |}|}}
// }}
void compound(const int *__counted_by(let * async) in, int let, int async);
