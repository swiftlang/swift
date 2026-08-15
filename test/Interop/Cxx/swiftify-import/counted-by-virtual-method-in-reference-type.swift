// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck -plugin-path %swift-plugin-dir -I %t -cxx-interoperability-mode=default \
// RUN:   %t/test.swift -verify -verify-additional-file %t%{fs-sep}test.h -Rmacro-expansions -verify-ignore-macro-note -eager-macro-checking

//--- test.h
#define __counted_by(x) __attribute__((__counted_by__(x)))

#define SWIFT_REFERENCE \
    __attribute__((swift_attr("import_reference"))) \
    __attribute__((swift_attr("retain:immortal")))  \
    __attribute__((swift_attr("release:immortal")))

struct ValueType {
  // expected-expansion@+8:66{{
  //   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
  //   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload|}}
  //   expected-remark@3{{macro content: |public func sumNonVirtual(_ values: UnsafeBufferPointer<CInt>) -> CInt {|}}
  //   expected-remark@4{{macro content: |    let len = CInt(exactly: values.count)!|}}
  //   expected-remark@5{{macro content: |    return unsafe sumNonVirtual(values.baseAddress, len)|}}
  //   expected-remark@6{{macro content: |}|}}
  // }}
  int sumNonVirtual(const int * __counted_by(len) values, int len) const;

  // expected-expansion@+8:71{{
  //   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
  //   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload|}}
  //   expected-remark@3{{macro content: |public func sumVirtual(_ values: UnsafeBufferPointer<CInt>) -> CInt {|}}
  //   expected-remark@4{{macro content: |    let len = CInt(exactly: values.count)!|}}
  //   expected-remark@5{{macro content: |    return unsafe sumVirtual(values.baseAddress, len)|}}
  //   expected-remark@6{{macro content: |}|}}
  // }}
  virtual int sumVirtual(const int * __counted_by(len) values, int len) const;
};

struct SWIFT_REFERENCE RefType {
  // expected-expansion@+8:66{{
  //   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
  //   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload|}}
  //   expected-remark@3{{macro content: |public final func sumNonVirtual(_ values: UnsafeBufferPointer<CInt>) -> CInt {|}}
  //   expected-remark@4{{macro content: |    let len = CInt(exactly: values.count)!|}}
  //   expected-remark@5{{macro content: |    return unsafe sumNonVirtual(values.baseAddress, len)|}}
  //   expected-remark@6{{macro content: |}|}}
  // }}
  int sumNonVirtual(const int * __counted_by(len) values, int len) const;

  // expected-expansion@+8:71{{
  //   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
  //   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload|}}
  //   expected-remark@3{{macro content: |public final func sumVirtual(_ values: UnsafeBufferPointer<CInt>) -> CInt {|}}
  //   expected-remark@4{{macro content: |    let len = CInt(exactly: values.count)!|}}
  //   expected-remark@5{{macro content: |    return unsafe sumVirtual(values.baseAddress, len)|}}
  //   expected-remark@6{{macro content: |}|}}
  // }}
  virtual int sumVirtual(const int * __counted_by(len) values, int len) const;
};

//--- module.modulemap
module Test {
  header "test.h"
  requires cplusplus
}

//--- test.swift
import Test

func useValueType(_ v: ValueType, _ p: UnsafePointer<CInt>!) {
  _ = v.sumNonVirtual(p, 1)
  _ = v.sumVirtual(p, 1)
}

@available(SwiftStdlib 5.8, *)
func useRefType(_ r: RefType, _ p: UnsafePointer<CInt>!) {
  _ = r.sumNonVirtual(p, 1)
  _ = r.sumVirtual(p, 1)
}
