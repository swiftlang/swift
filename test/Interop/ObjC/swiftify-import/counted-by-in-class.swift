// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -plugin-path %swift-plugin-dir -I %t/Inputs %t/method.swift -Rmacro-expansions -emit-module \
// RUN:   -verify -verify-additional-file %t%{fs-sep}Inputs%{fs-sep}method.h -eager-macro-checking \
// RUN:   -Xcc -Wno-nullability-completeness -strict-memory-safety

//--- Inputs/module.modulemap
module Method {
    header "method.h"
}

//--- Inputs/method.h
#pragma once

#define __counted_by(x) __attribute__((__counted_by__(x)))

@interface Foo
// expected-expansion@+10:1{{
//   expected-note@1 5{{in expansion of macro '_SwiftifyImport' on instance method 'bar(_:count:)' here}}
// }}
// expected-expansion@+7:37{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public final func bar(_ p: UnsafeMutableBufferPointer<Float>) {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe bar(p.baseAddress!, count: len)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
-(void)bar:(float *)p count:(int)len __attribute__((swift_attr("@_SwiftifyImport(.countedBy(pointer: .param(1), count: \"len\"))")));

// expected-expansion@+10:2{{
//   expected-note@1 5{{in expansion of macro '_SwiftifyImport' on instance method 'simple' here}}
// }}
// expected-expansion@+7:54{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public final func simple(_ p: UnsafeMutableBufferPointer<Int32>) {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe simple(len, p.baseAddress!)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
- (void) simple:(int)len :(int * __counted_by(len))p;

// expected-expansion@+13:2{{
//   expected-note@1 8{{in expansion of macro '_SwiftifyImport' on instance method 'shared' here}}
// }}
// expected-expansion@+10:84{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public final func shared(_ p1: UnsafeMutableBufferPointer<Int32>, _ p2: UnsafeMutableBufferPointer<Int32>) {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: p1.count)!|}}
//   expected-remark@4{{macro content: |    if p2.count != len {|}}
//   expected-remark@5{{macro content: |      fatalError("bounds check failure in shared: expected \\(len) but got \\(p2.count)")|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    return unsafe shared(len, p1.baseAddress!, p2.baseAddress!)|}}
//   expected-remark@8{{macro content: |}|}}
// }}
- (void) shared:(int)len :(int * __counted_by(len))p1 :(int * __counted_by(len))p2;

// expected-expansion@+13:82{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public final func complexExpr(_ len: Int32, _ offset: Int32, _ p: UnsafeMutableBufferPointer<Int32>) {|}}
//   expected-remark@3{{macro content: |    let _pCount = p.count|}}
//   expected-remark@4{{macro content: |    if _pCount != len - offset {|}}
//   expected-remark@5{{macro content: |      fatalError("bounds check failure in complexExpr: expected \\(len - offset) but got \\(_pCount)")|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    return unsafe complexExpr(len, offset, p.baseAddress!)|}}
//   expected-remark@8{{macro content: |}|}}
// }}
// expected-expansion@+3:2{{
//   expected-note@1 8{{in expansion of macro '_SwiftifyImport' on instance method 'complexExpr' here}}
// }}
- (void) complexExpr:(int)len :(int) offset :(int * __counted_by(len - offset))p;

// expected-expansion@+10:2{{
//   expected-note@1 5{{in expansion of macro '_SwiftifyImport' on instance method 'nullUnspecified' here}}
// }}
// expected-expansion@+7:81{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public final func nullUnspecified(_ p: UnsafeMutableBufferPointer<Int32>) {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe nullUnspecified(len, p.baseAddress!)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
- (void) nullUnspecified:(int)len :(int * __counted_by(len) _Null_unspecified)p;

// expected-expansion@+10:2{{
//   expected-note@1 5{{in expansion of macro '_SwiftifyImport' on instance method 'nonnull' here}}
// }}
// expected-expansion@+7:64{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public final func nonnull(_ p: UnsafeMutableBufferPointer<Int32>) {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe nonnull(len, p.baseAddress!)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
- (void) nonnull:(int)len :(int * __counted_by(len) _Nonnull)p;

// expected-expansion@+10:2{{
//   expected-note@1 5{{in expansion of macro '_SwiftifyImport' on instance method 'nullable' here}}
// }}
// expected-expansion@+7:66{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public final func nullable(_ p: UnsafeMutableBufferPointer<Int32>?) {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: unsafe p?.count ?? 0)!|}}
//   expected-remark@4{{macro content: |    return unsafe nullable(len, p?.baseAddress)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
- (void) nullable:(int)len :(int * __counted_by(len) _Nullable)p;

// expected-expansion@+9:2{{
//   expected-note@1 4{{in expansion of macro '_SwiftifyImport' on instance method 'returnPointer' here}}
// }}
// expected-expansion@+6:52{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public final func returnPointer(_ len: Int32) -> UnsafeMutableBufferPointer<Int32> {|}}
//   expected-remark@3{{macro content: |    return unsafe UnsafeMutableBufferPointer<Int32>(start: unsafe returnPointer(len), count: Int(len))|}}
//   expected-remark@4{{macro content: |}|}}
// }}
- (int * __counted_by(len)) returnPointer:(int)len;

// expected-expansion@+10:2{{
//   expected-note@1 5{{in expansion of macro '_SwiftifyImport' on class method 'staticMethod' here}}
// }}
// expected-expansion@+7:60{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public class final func staticMethod(_ p: UnsafeMutableBufferPointer<Int32>) {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe staticMethod(len, p.baseAddress!)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
+ (void) staticMethod:(int)len :(int * __counted_by(len))p;
@end

//--- method.swift
// GENERATED-BY: %target-swift-ide-test -print-module -module-to-print=Method -plugin-path %swift-plugin-dir -I %t/Inputs -source-filename=x > %t/Test-interface.swift && %swift-function-caller-generator Method %t/Test-interface.swift
// GENERATED-HASH: d8d118fe8a443b7f4ebd98b7929771d2ef6b53e43f5a0b5ffe10cd22ccff0d9b
import Method

func call_bar(_ p: UnsafeMutablePointer<Float>!, count len: Int32) {
  return unsafe Foo.bar(p, count: len)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_bar(_ self: Foo, _ p: UnsafeMutableBufferPointer<Float>) {
  return unsafe self.bar(p)
}

func call_bar(_ self: Foo, _ p: UnsafeMutablePointer<Float>!, count len: Int32) {
  return unsafe self.bar(p, count: len)
}
func call_simple(_ len: Int32, _ p: UnsafeMutablePointer<Int32>!) {
  return unsafe Foo.simple(len, p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_simple(_ self: Foo, _ p: UnsafeMutableBufferPointer<Int32>) {
  return unsafe self.simple(p)
}

func call_simple(_ self: Foo, _ len: Int32, _ p: UnsafeMutablePointer<Int32>!) {
  return unsafe self.simple(len, p)
}
func call_shared(_ len: Int32, _ p1: UnsafeMutablePointer<Int32>!, _ p2: UnsafeMutablePointer<Int32>!) {
  return unsafe Foo.shared(len, p1, p2)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_shared(_ self: Foo, _ p1: UnsafeMutableBufferPointer<Int32>, _ p2: UnsafeMutableBufferPointer<Int32>) {
  return unsafe self.shared(p1, p2)
}

func call_shared(_ self: Foo, _ len: Int32, _ p1: UnsafeMutablePointer<Int32>!, _ p2: UnsafeMutablePointer<Int32>!) {
  return unsafe self.shared(len, p1, p2)
}
func call_complexExpr(_ len: Int32, _ offset: Int32, _ p: UnsafeMutablePointer<Int32>!) {
  return unsafe Foo.complexExpr(len, offset, p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_complexExpr(_ self: Foo, _ len: Int32, _ offset: Int32, _ p: UnsafeMutableBufferPointer<Int32>) {
  return unsafe self.complexExpr(len, offset, p)
}

func call_complexExpr(_ self: Foo, _ len: Int32, _ offset: Int32, _ p: UnsafeMutablePointer<Int32>!) {
  return unsafe self.complexExpr(len, offset, p)
}
func call_nullUnspecified(_ len: Int32, _ p: UnsafeMutablePointer<Int32>!) {
  return unsafe Foo.nullUnspecified(len, p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_nullUnspecified(_ self: Foo, _ p: UnsafeMutableBufferPointer<Int32>) {
  return unsafe self.nullUnspecified(p)
}

func call_nullUnspecified(_ self: Foo, _ len: Int32, _ p: UnsafeMutablePointer<Int32>!) {
  return unsafe self.nullUnspecified(len, p)
}
func call_nonnull(_ len: Int32, _ p: UnsafeMutablePointer<Int32>) {
  return unsafe Foo.nonnull(len, p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_nonnull(_ self: Foo, _ p: UnsafeMutableBufferPointer<Int32>) {
  return unsafe self.nonnull(p)
}

func call_nonnull(_ self: Foo, _ len: Int32, _ p: UnsafeMutablePointer<Int32>) {
  return unsafe self.nonnull(len, p)
}
func call_nullable(_ len: Int32, _ p: UnsafeMutablePointer<Int32>?) {
  return unsafe Foo.nullable(len, p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_nullable(_ self: Foo, _ p: UnsafeMutableBufferPointer<Int32>?) {
  return unsafe self.nullable(p)
}

func call_nullable(_ self: Foo, _ len: Int32, _ p: UnsafeMutablePointer<Int32>?) {
  return unsafe self.nullable(len, p)
}
func call_returnPointer(_ len: Int32) -> UnsafeMutablePointer<Int32>! {
  return unsafe Foo.returnPointer(len)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_returnPointer(_ self: Foo, _ len: Int32) -> UnsafeMutableBufferPointer<Int32> {
  return unsafe self.returnPointer(len)
}

func call_returnPointer(_ self: Foo, _ len: Int32) -> UnsafeMutablePointer<Int32>! {
  return unsafe self.returnPointer(len)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_staticMethod(_ p: UnsafeMutableBufferPointer<Int32>) {
  return unsafe Foo.staticMethod(p)
}
func call_staticMethod(_ len: Int32, _ p: UnsafeMutablePointer<Int32>!) {
  return unsafe Foo.staticMethod(len, p)
}
