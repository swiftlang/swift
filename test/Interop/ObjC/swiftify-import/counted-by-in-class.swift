// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -plugin-path %swift-plugin-dir -I %t/Inputs %t/method.swift -Rmacro-expansions -emit-module \
// RUN:   -verify -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}method.h -eager-macro-checking \
// RUN:   -Xcc -Wno-nullability-completeness -strict-memory-safety -verify-ignore-macro-note

//--- Inputs/module.modulemap
module Method {
    header "method.h"
}

//--- Inputs/method.h
#pragma once

#define __counted_by(x) __attribute__((__counted_by__(x)))

@interface Foo
// expected-expansion@+14:2{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public class final func bar(_ p: UnsafeMutableBufferPointer<CFloat>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe bar(p.baseAddress, count: len)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
// expected-expansion@+7:37{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public final func bar(_ p: UnsafeMutableBufferPointer<CFloat>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe bar(p.baseAddress, count: len)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
-(void)bar:(float *)p count:(int)len __attribute__((swift_attr("@_SwiftifyImport(.countedBy(pointer: .param(1), count: \"len\"))")));

// expected-expansion@+14:2{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public class final func simple(_ p: UnsafeMutableBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe simple(len, p.baseAddress)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
// expected-expansion@+7:53{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public final func simple(_ p: UnsafeMutableBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe simple(len, p.baseAddress)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
- (void) simple:(int)len :(int * __counted_by(len))p;

// expected-expansion@+20:2{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public class final func shared(_ p1: UnsafeMutableBufferPointer<CInt>, _ p2: UnsafeMutableBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: p2.count)!|}}
//   expected-remark@4{{macro content: |    if p1.count != len {|}}
//   expected-remark@5{{macro content: |      fatalError("bounds check failure in shared: expected \\(len) but got \\(p1.count)")|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    return unsafe shared(len, p1.baseAddress, p2.baseAddress)|}}
//   expected-remark@8{{macro content: |}|}}
// }}
// expected-expansion@+10:83{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public final func shared(_ p1: UnsafeMutableBufferPointer<CInt>, _ p2: UnsafeMutableBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: p2.count)!|}}
//   expected-remark@4{{macro content: |    if p1.count != len {|}}
//   expected-remark@5{{macro content: |      fatalError("bounds check failure in shared: expected \\(len) but got \\(p1.count)")|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    return unsafe shared(len, p1.baseAddress, p2.baseAddress)|}}
//   expected-remark@8{{macro content: |}|}}
// }}
- (void) shared:(int)len :(int * __counted_by(len))p1 :(int * __counted_by(len))p2;

// expected-expansion@+18:2{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public class final func complexExpr(_ len: CInt, _ offset: CInt, _ p: UnsafeMutableBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    if p.count != (len - offset) {|}}
//   expected-remark@4{{macro content: |      fatalError("bounds check failure in complexExpr: expected \\((len - offset)) but got \\(p.count)")|}}
//   expected-remark@5{{macro content: |    }|}}
//   expected-remark@6{{macro content: |    return unsafe complexExpr(len, offset, p.baseAddress)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
// expected-expansion@+9:81{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public final func complexExpr(_ len: CInt, _ offset: CInt, _ p: UnsafeMutableBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    if p.count != (len - offset) {|}}
//   expected-remark@4{{macro content: |      fatalError("bounds check failure in complexExpr: expected \\((len - offset)) but got \\(p.count)")|}}
//   expected-remark@5{{macro content: |    }|}}
//   expected-remark@6{{macro content: |    return unsafe complexExpr(len, offset, p.baseAddress)|}}
//   expected-remark@7{{macro content: |}|}}
// }}
- (void) complexExpr:(int)len :(int) offset :(int * __counted_by(len - offset))p;

// expected-expansion@+14:2{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public class final func nullUnspecified(_ p: UnsafeMutableBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe nullUnspecified(len, p.baseAddress)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
// expected-expansion@+7:80{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public final func nullUnspecified(_ p: UnsafeMutableBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe nullUnspecified(len, p.baseAddress)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
- (void) nullUnspecified:(int)len :(int * __counted_by(len) _Null_unspecified)p;

// expected-expansion@+14:2{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public class final func nonnull(_ p: UnsafeMutableBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe nonnull(len, p.baseAddress!)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
// expected-expansion@+7:63{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public final func nonnull(_ p: UnsafeMutableBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe nonnull(len, p.baseAddress!)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
- (void) nonnull:(int)len :(int * __counted_by(len) _Nonnull)p;

// expected-expansion@+14:2{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public class final func nullable(_ p: UnsafeMutableBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe nullable(len, p.baseAddress)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
// expected-expansion@+7:65{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public final func nullable(_ p: UnsafeMutableBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe nullable(len, p.baseAddress)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
- (void) nullable:(int)len :(int * __counted_by(len) _Nullable)p;

// expected-expansion@+12:2{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public class final func returnPointer(_ len: CInt) -> UnsafeMutableBufferPointer<CInt> {|}}
//   expected-remark@3{{macro content: |    return unsafe UnsafeMutableBufferPointer<CInt>(start: unsafe returnPointer(len), count: Int(len))|}}
//   expected-remark@4{{macro content: |}|}}
// }}
// expected-expansion@+6:51{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public final func returnPointer(_ len: CInt) -> UnsafeMutableBufferPointer<CInt> {|}}
//   expected-remark@3{{macro content: |    return unsafe UnsafeMutableBufferPointer<CInt>(start: unsafe returnPointer(len), count: Int(len))|}}
//   expected-remark@4{{macro content: |}|}}
// }}
- (int * __counted_by(len)) returnPointer:(int)len;

// expected-expansion@+7:59{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public class final func staticMethod(_ p: UnsafeMutableBufferPointer<CInt>) {|}}
//   expected-remark@3{{macro content: |    let len = CInt(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe staticMethod(len, p.baseAddress)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
+ (void) staticMethod:(int)len :(int * __counted_by(len))p;
@end

//--- method.swift
// GENERATED-BY: %target-swift-ide-test -print-module -module-to-print=Method -plugin-path %swift-plugin-dir -I %t/Inputs -source-filename=x > %t/Test-interface.swift && %swift-function-caller-generator Method %t/Test-interface.swift
// GENERATED-HASH: e29b56d448db9d939991c4242d88cdfb3939f556b6b3bab2bdab4915ee8e8997
import Method


extension Foo {
  final func call_bar_Foo_classmethod(_ p: UnsafeMutablePointer<CFloat>!, count len: CInt) {
    return unsafe Foo.bar(p, count: len)
  }
  @_alwaysEmitIntoClient @_disfavoredOverload final func call_bar_Foo(_ p: UnsafeMutableBufferPointer<CFloat>) {
    return unsafe bar(p)
  }
  @_alwaysEmitIntoClient @_disfavoredOverload final func call_bar_Foo_classmethod(_ p: UnsafeMutableBufferPointer<CFloat>) {
    return unsafe Foo.bar(p)
  }
  final func call_bar_Foo(_ p: UnsafeMutablePointer<CFloat>!, count len: CInt) {
    return unsafe bar(p, count: len)
  }
  final func call_simple_Foo_classmethod(_ len: CInt, _ p: UnsafeMutablePointer<CInt>!) {
    return unsafe Foo.simple(len, p)
  }
  @_alwaysEmitIntoClient @_disfavoredOverload final func call_simple_Foo(_ p: UnsafeMutableBufferPointer<CInt>) {
    return unsafe simple(p)
  }
  @_alwaysEmitIntoClient @_disfavoredOverload final func call_simple_Foo_classmethod(_ p: UnsafeMutableBufferPointer<CInt>) {
    return unsafe Foo.simple(p)
  }
  final func call_simple_Foo(_ len: CInt, _ p: UnsafeMutablePointer<CInt>!) {
    return unsafe simple(len, p)
  }
  final func call_shared_Foo_classmethod(_ len: CInt, _ p1: UnsafeMutablePointer<CInt>!, _ p2: UnsafeMutablePointer<CInt>!) {
    return unsafe Foo.shared(len, p1, p2)
  }
  @_alwaysEmitIntoClient @_disfavoredOverload final func call_shared_Foo(_ p1: UnsafeMutableBufferPointer<CInt>, _ p2: UnsafeMutableBufferPointer<CInt>) {
    return unsafe shared(p1, p2)
  }
  @_alwaysEmitIntoClient @_disfavoredOverload final func call_shared_Foo_classmethod(_ p1: UnsafeMutableBufferPointer<CInt>, _ p2: UnsafeMutableBufferPointer<CInt>) {
    return unsafe Foo.shared(p1, p2)
  }
  final func call_shared_Foo(_ len: CInt, _ p1: UnsafeMutablePointer<CInt>!, _ p2: UnsafeMutablePointer<CInt>!) {
    return unsafe shared(len, p1, p2)
  }
  final func call_complexExpr_Foo_classmethod(_ len: CInt, _ offset: CInt, _ p: UnsafeMutablePointer<CInt>!) {
    return unsafe Foo.complexExpr(len, offset, p)
  }
  @_alwaysEmitIntoClient @_disfavoredOverload final func call_complexExpr_Foo(_ len: CInt, _ offset: CInt, _ p: UnsafeMutableBufferPointer<CInt>) {
    return unsafe complexExpr(len, offset, p)
  }
  @_alwaysEmitIntoClient @_disfavoredOverload final func call_complexExpr_Foo_classmethod(_ len: CInt, _ offset: CInt, _ p: UnsafeMutableBufferPointer<CInt>) {
    return unsafe Foo.complexExpr(len, offset, p)
  }
  final func call_complexExpr_Foo(_ len: CInt, _ offset: CInt, _ p: UnsafeMutablePointer<CInt>!) {
    return unsafe complexExpr(len, offset, p)
  }
  final func call_nullUnspecified_Foo_classmethod(_ len: CInt, _ p: UnsafeMutablePointer<CInt>!) {
    return unsafe Foo.nullUnspecified(len, p)
  }
  @_alwaysEmitIntoClient @_disfavoredOverload final func call_nullUnspecified_Foo(_ p: UnsafeMutableBufferPointer<CInt>) {
    return unsafe nullUnspecified(p)
  }
  @_alwaysEmitIntoClient @_disfavoredOverload final func call_nullUnspecified_Foo_classmethod(_ p: UnsafeMutableBufferPointer<CInt>) {
    return unsafe Foo.nullUnspecified(p)
  }
  final func call_nullUnspecified_Foo(_ len: CInt, _ p: UnsafeMutablePointer<CInt>!) {
    return unsafe nullUnspecified(len, p)
  }
  final func call_nonnull_Foo_classmethod(_ len: CInt, _ p: UnsafeMutablePointer<CInt>) {
    return unsafe Foo.nonnull(len, p)
  }
  @_alwaysEmitIntoClient @_disfavoredOverload final func call_nonnull_Foo(_ p: UnsafeMutableBufferPointer<CInt>) {
    return unsafe nonnull(p)
  }
  @_alwaysEmitIntoClient @_disfavoredOverload final func call_nonnull_Foo_classmethod(_ p: UnsafeMutableBufferPointer<CInt>) {
    return unsafe Foo.nonnull(p)
  }
  final func call_nonnull_Foo(_ len: CInt, _ p: UnsafeMutablePointer<CInt>) {
    return unsafe nonnull(len, p)
  }
  final func call_nullable_Foo_classmethod(_ len: CInt, _ p: UnsafeMutablePointer<CInt>?) {
    return unsafe Foo.nullable(len, p)
  }
  @_alwaysEmitIntoClient @_disfavoredOverload final func call_nullable_Foo(_ p: UnsafeMutableBufferPointer<CInt>) {
    return unsafe nullable(p)
  }
  @_alwaysEmitIntoClient @_disfavoredOverload final func call_nullable_Foo_classmethod(_ p: UnsafeMutableBufferPointer<CInt>) {
    return unsafe Foo.nullable(p)
  }
  final func call_nullable_Foo(_ len: CInt, _ p: UnsafeMutablePointer<CInt>?) {
    return unsafe nullable(len, p)
  }
  final func call_returnPointer_Foo_classmethod(_ len: CInt) -> UnsafeMutablePointer<CInt>! {
    return unsafe Foo.returnPointer(len)
  }
  @_alwaysEmitIntoClient @_disfavoredOverload final func call_returnPointer_Foo(_ len: CInt) -> UnsafeMutableBufferPointer<CInt> {
    return unsafe returnPointer(len)
  }
  @_alwaysEmitIntoClient @_disfavoredOverload final func call_returnPointer_Foo_classmethod(_ len: CInt) -> UnsafeMutableBufferPointer<CInt> {
    return unsafe Foo.returnPointer(len)
  }
  final func call_returnPointer_Foo(_ len: CInt) -> UnsafeMutablePointer<CInt>! {
    return unsafe returnPointer(len)
  }
  @_alwaysEmitIntoClient @_disfavoredOverload final func call_staticMethod_Foo_classmethod(_ p: UnsafeMutableBufferPointer<CInt>) {
    return unsafe Foo.staticMethod(p)
  }
  final func call_staticMethod_Foo_classmethod(_ len: CInt, _ p: UnsafeMutablePointer<CInt>!) {
    return unsafe Foo.staticMethod(len, p)
  }
}
