// REQUIRES: swift_feature_SafeInteropWrappers

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -plugin-path %swift-plugin-dir -I %t -cxx-interoperability-mode=default -enable-experimental-feature SafeInteropWrappers %t/namespace.swift -emit-module \
// RUN:   -verify -verify-additional-file %t%{fs-sep}namespace.h -Rmacro-expansions -strict-memory-safety -o %t/out.swiftmodule -import-bridging-header %t/bridging.h

// RUN: %target-swift-frontend -plugin-path %swift-plugin-dir -I %t -cxx-interoperability-mode=default -enable-experimental-feature SafeInteropWrappers %t/namespace.swift -typecheck \
// RUN:   -dump-source-file-imports -import-bridging-header %t/bridging.h 2>&1 | %FileCheck --dry-run > %t/imports.txt
// RUN: %diff %t/imports.txt %t/imports.txt.expected

// This test checks that macro expansions can access symbols from namespaces
// (which are dumped into the __ObjC module) despite not having an implicit
// import of the bridging header module (aka __ObjC).

//--- imports.txt.expected
imports for TMP_DIR/namespace.swift:
	Swift
	__ObjC
	CxxShim
	Cxx
	_StringProcessing
	_SwiftConcurrencyShims
	_Concurrency
	Namespace
imports for Namespace.bar:
imports for @__swiftmacro_So3bar15_SwiftifyImportfMp_.swift:
	Swift
	CxxShim
	Cxx
	_StringProcessing
	_SwiftConcurrencyShims
	_Concurrency
imports for Namespace.bar2:
imports for @__swiftmacro_So4bar215_SwiftifyImportfMp_.swift:
	Swift
	CxxShim
	Cxx
	_StringProcessing
	_SwiftConcurrencyShims
	_Concurrency
imports for __ObjC.baz.baz_func:
imports for @__swiftmacro_So3bazO0A5_func15_SwiftifyImportfMp_.swift:
	Namespace
	Swift
	CxxShim
	Cxx
	_StringProcessing
	_SwiftConcurrencyShims
	_Concurrency

//--- bridging.h
const int FOO = 42; // not used

//--- module.modulemap
module Namespace {
    header "namespace.h"
    requires cplusplus
}

//--- namespace.h
#define __counted_by(x) __attribute__((__counted_by__(x)))
#define __noescape __attribute__((noescape))

namespace foo {
  typedef int foo_t;
}

// expected-expansion@+10:141{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func bar(_ p: UnsafeMutableBufferPointer<Float>, _ extra: foo.foo_t) {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe bar(p.baseAddress!, len, extra)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
// expected-expansion@+3:102{{
//   expected-note@1 5{{in expansion of macro '_SwiftifyImport' on global function 'bar' here}}
// }}
__attribute__((swift_attr("@_SwiftifyImport(.countedBy(pointer: .param(1), count: \"len\"))"))) void bar(float *p, int len, foo::foo_t extra);
// expected-expansion@+12:94{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload public func bar2(_ p: Span<foo.foo_t>, _ extra: foo.foo_t) {|}}
//   expected-remark@3{{macro content: |    let len = foo.foo_t(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe p.withUnsafeBufferPointer { _pPtr in|}}
//   expected-remark@5{{macro content: |      return unsafe bar2(_pPtr.baseAddress!, len, extra)|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |}|}}
// }}
// expected-expansion@+3:6{{
//   expected-note@1 7{{in expansion of macro '_SwiftifyImport' on global function 'bar2' here}}
// }}
void bar2(const foo::foo_t * __counted_by(len) p __noescape, foo::foo_t len, foo::foo_t extra);
namespace baz {
  // expected-expansion@+13:100{{
  //   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
  //   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @available(visionOS 1.0, tvOS 12.2, watchOS 5.2, iOS 12.2, macOS 10.14.4, *) @_disfavoredOverload|}}
  //   expected-remark@3{{macro content: |public static func baz_func(_ p: Span<foo.foo_t>, _ extra: foo.foo_t) {|}}
  //   expected-remark@4{{macro content: |    let len = foo.foo_t(exactly: p.count)!|}}
  //   expected-remark@5{{macro content: |    return unsafe p.withUnsafeBufferPointer { _pPtr in|}}
  //   expected-remark@6{{macro content: |      return unsafe baz_func(_pPtr.baseAddress!, len, extra)|}}
  //   expected-remark@7{{macro content: |    }|}}
  //   expected-remark@8{{macro content: |}|}}
  // }}
  // expected-expansion@+3:8{{
  //   expected-note@1 8{{in expansion of macro '_SwiftifyImport' on static method 'baz_func' here}}
  // }}
  void baz_func(const foo::foo_t * __counted_by(len) p __noescape, foo::foo_t len, foo::foo_t extra);
}

//--- namespace.swift
import Namespace

func test(s: UnsafeMutableBufferPointer<Float>, extra: foo.foo_t) {
  unsafe bar(s, extra)
}

func test2(s: Span<CInt>, extra: foo.foo_t) {
  bar2(s, extra)
}

func test3(s: Span<CInt>, extra: foo.foo_t) {
  baz.baz_func(s, extra)
}

