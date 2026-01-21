// REQUIRES: swift_feature_SafeInteropWrappers

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -o %t/test.swiftmodule %t/test.swift -I %t -enable-experimental-feature SafeInteropWrappers -strict-memory-safety \
// RUN:   -verify -verify-additional-file %t%{fs-sep}foo.h -verify-additional-file %t%{fs-sep}bar.h -verify-additional-file %t%{fs-sep}baz.h -verify-additional-file %t%{fs-sep}qux.h -Rmacro-expansions

// Macro expansions in foo.h do not have access to the definition of `struct qux`,
// so don't attach macro on functions that use contain that type in foo.h.
// bar.h imports the type, so attach the macro.
// baz.h imports bar.h which re-exports the type, so attach the macro there too.

//--- foo.h
#pragma once

#define __counted_by(x) __attribute__((__counted_by__(x)))

struct qux;
// expected-note@+1{{'foo' declared here}}
void foo(struct qux *x, int * __counted_by(len) p, int len);
// expected-note@+1{{'fooIndirect' declared here}}
void fooIndirect(struct qux * * x, int * __counted_by(len) p, int len);
// expected-note@+1{{'fooIndirectCompleteArray' declared here}}
void fooIndirectCompleteArray(struct qux* (* x)[2], int * __counted_by(len) p, int len);
// expected-note@+1{{'fooReturn' declared here}}
struct qux * fooReturn(int * __counted_by(len) p, int len);

enum fwd_declared_enum : int;
// expected-note@+1{{'testEnum' declared here}}
void testEnum(enum fwd_declared_enum *x, int * __counted_by(len) p, int len);

struct container_t {
  struct qux *item;
};
// expected-expansion@+10:6{{
//   expected-note@1 5{{in expansion of macro '_SwiftifyImport' on global function 'fooWrapped' here}}
// }}
// expected-expansion@+7:75{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func fooWrapped(_ x: UnsafeMutablePointer<container_t>!, _ p: UnsafeMutableBufferPointer<Int32>) {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe fooWrapped(x, p.baseAddress!, len)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
void fooWrapped(struct container_t * x, int * __counted_by(len) p, int len);


//--- bar.h
#pragma once
#include "qux.h"

#define __counted_by(x) __attribute__((__counted_by__(x)))

// expected-expansion@+10:59{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func bar(_ x: UnsafeMutablePointer<qux>!, _ p: UnsafeMutableBufferPointer<Int32>) {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe bar(x, p.baseAddress!, len)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
// expected-expansion@+3:6{{
//   expected-note@1 5{{in expansion of macro '_SwiftifyImport' on global function 'bar' here}}
// }}
void bar(struct qux *x, int * __counted_by(len) p, int len);
// expected-expansion@+10:58{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func barReturn(_ p: UnsafeMutableBufferPointer<Int32>) -> UnsafeMutablePointer<qux>! {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe barReturn(p.baseAddress!, len)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
// expected-expansion@+3:14{{
//   expected-note@1 5{{in expansion of macro '_SwiftifyImport' on global function 'barReturn' here}}
// }}
struct qux * barReturn(int * __counted_by(len) p, int len);


//--- baz.h
#pragma once

#include "bar.h"

struct qux;
// expected-expansion@+10:59{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func baz(_ x: UnsafeMutablePointer<qux>!, _ p: UnsafeMutableBufferPointer<Int32>) {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe baz(x, p.baseAddress!, len)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
// expected-expansion@+3:6{{
//   expected-note@1 5{{in expansion of macro '_SwiftifyImport' on global function 'baz' here}}
// }}
void baz(struct qux *x, int * __counted_by(len) p, int len);
// expected-expansion@+10:58{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func bazReturn(_ p: UnsafeMutableBufferPointer<Int32>) -> UnsafeMutablePointer<qux>! {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe bazReturn(p.baseAddress!, len)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
// expected-expansion@+3:14{{
//   expected-note@1 5{{in expansion of macro '_SwiftifyImport' on global function 'bazReturn' here}}
// }}
struct qux * bazReturn(int * __counted_by(len) p, int len);


//--- qux.h
#pragma once

struct qux { int placeholder; };

enum fwd_declared_enum : int {
  enum_member,
};


//--- test.swift
import Foo
import Bar
import Baz

func callFoo(_ x: UnsafeMutablePointer<qux>, _ y: UnsafeMutablePointer<UnsafeMutablePointer<qux>?>, _ z: UnsafeMutablePointer<container_t>,
             _ zz: UnsafeMutablePointer<(UnsafeMutablePointer<qux>?, UnsafeMutablePointer<qux>?)>,
             _ p: UnsafeMutablePointer<CInt>, _ len: CInt) {
  unsafe foo(x, p, len)
  unsafe fooIndirect(y, p, len)
  unsafe fooIndirectCompleteArray(zz, p, len)
  let _: UnsafeMutablePointer<qux> = unsafe fooReturn(p, len)
  unsafe fooWrapped(z, p, len)
}

func callTestEnum(_ x: UnsafeMutablePointer<fwd_declared_enum>,
                  _ p: UnsafeMutablePointer<CInt>, _ len: CInt) {
  unsafe testEnum(x, p, len)
}

func callFoo2(_ x: UnsafeMutablePointer<qux>, _ y: UnsafeMutablePointer<UnsafeMutablePointer<qux>?>, _ z: UnsafeMutablePointer<container_t>,
              _ zz: UnsafeMutablePointer<(UnsafeMutablePointer<qux>?, UnsafeMutablePointer<qux>?)>,
              _ p: UnsafeMutableBufferPointer<CInt>) {
  // expected-error@+2{{missing argument for parameter #3 in call}}
  // expected-error@+1{{cannot convert value of type 'UnsafeMutableBufferPointer<CInt>' (aka 'UnsafeMutableBufferPointer<Int32>') to expected argument type 'UnsafeMutablePointer<Int32>'}}
  unsafe foo(x, p)
  // expected-error@+2{{missing argument for parameter #3 in call}}
  // expected-error@+1{{cannot convert value of type 'UnsafeMutableBufferPointer<CInt>' (aka 'UnsafeMutableBufferPointer<Int32>') to expected argument type 'UnsafeMutablePointer<Int32>'}}
  unsafe fooIndirect(y, p)
  // expected-error@+2{{missing argument for parameter #3 in call}}
  // expected-error@+1{{cannot convert value of type 'UnsafeMutableBufferPointer<CInt>' (aka 'UnsafeMutableBufferPointer<Int32>') to expected argument type 'UnsafeMutablePointer<Int32>'}}
  unsafe fooIndirectCompleteArray(zz, p)
  // expected-error@+2{{missing argument for parameter #2 in call}}
  // expected-error@+1{{cannot convert value of type 'UnsafeMutableBufferPointer<CInt>' (aka 'UnsafeMutableBufferPointer<Int32>') to expected argument type 'UnsafeMutablePointer<Int32>'}}
  let _: UnsafeMutablePointer<qux> = unsafe fooReturn(p)
  unsafe fooWrapped(z, p)
}

func callTestEnum2(_ x: UnsafeMutablePointer<fwd_declared_enum>,
                  _ p: UnsafeMutableBufferPointer<CInt>) {
  // expected-error@+2{{missing argument for parameter #3 in call}}
  // expected-error@+1{{cannot convert value of type 'UnsafeMutableBufferPointer<CInt>' (aka 'UnsafeMutableBufferPointer<Int32>') to expected argument type 'UnsafeMutablePointer<Int32>'}}
  unsafe testEnum(x, p)
}

func callBar(_ x: UnsafeMutablePointer<qux>, _ p: UnsafeMutablePointer<CInt>, _ len: CInt) {
  unsafe bar(x, p, len)
  let _: UnsafeMutablePointer<qux> = unsafe barReturn(p, len)
}

func callBar2(_ x: UnsafeMutablePointer<qux>, _ p: UnsafeMutableBufferPointer<CInt>) {
  unsafe bar(x, p)
  let _: UnsafeMutablePointer<qux> = unsafe barReturn(p)
}


func callBaz(_ x: UnsafeMutablePointer<qux>, _ p: UnsafeMutablePointer<CInt>, _ len: CInt) {
  unsafe baz(x, p, len)
  let _: UnsafeMutablePointer<qux> = unsafe bazReturn(p, len)
}

func callBaz2(_ x: UnsafeMutablePointer<qux>, _ p: UnsafeMutableBufferPointer<CInt>) {
  unsafe baz(x, p)
  let _: UnsafeMutablePointer<qux> = unsafe bazReturn(p)
}


//--- module.modulemap
module Foo {
  header "foo.h"
}
module Bar {
  header "bar.h"
  export qux
}
module Baz {
  header "baz.h"
  export Bar
}
module qux {
  header "qux.h"
}
