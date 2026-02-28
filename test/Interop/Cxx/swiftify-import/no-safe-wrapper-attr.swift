// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -plugin-path %swift-plugin-dir -I %t -cxx-interoperability-mode=default %t/test.swift -emit-module \
// RUN:   -verify -Rmacro-expansions -verify-additional-file %t%{fs-sep}test.h -suppress-notes

//--- test.h
#define __counted_by(x) __attribute__((__counted_by__(x)))
#define SWIFT_NO_SAFE_WRAPPER __attribute__((swift_attr("no_safe_wrapper")));

// expected-expansion@+7:63{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func control_group_function(_ p: UnsafeMutableBufferPointer<Int32>) {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe control_group_function(p.baseAddress!, len)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
void control_group_function(int * __counted_by(len) p, int len);

void foo(int * __counted_by(len) p, int len) SWIFT_NO_SAFE_WRAPPER;

struct Bar {
  // expected-expansion@+8:63{{
  //   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
  //   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload|}}
  //   expected-remark@3{{macro content: |public mutating func control_group_method(_ p: UnsafeMutableBufferPointer<Int32>) {|}}
  //   expected-remark@4{{macro content: |    let len = Int32(exactly: p.count)!|}}
  //   expected-remark@5{{macro content: |    return unsafe control_group_method(p.baseAddress!, len)|}}
  //   expected-remark@6{{macro content: |}|}}
  // }}
  void control_group_method(int * __counted_by(len) p, int len);

  void baz(int * __counted_by(len) p, int len) SWIFT_NO_SAFE_WRAPPER;
};

//--- module.modulemap
module Test {
  header "test.h"
}

//--- test.swift
// GENERATED-BY: %target-swift-ide-test -plugin-path %swift-plugin-dir -I %t -cxx-interoperability-mode=upcoming-swift -print-module -module-to-print=Test -source-filename=x > %t/Test-interface.swift && %swift-function-caller-generator Test %t/Test-interface.swift
// GENERATED-HASH: 52710b9bd39420d372ef8f0bb9a3046b1a22e6056114ef7862674685fc654da5
import Test

func call_control_group_function(_ p: UnsafeMutablePointer<Int32>!, _ len: Int32) {
  return unsafe control_group_function(p, len)
}

func call_foo(_ p: UnsafeMutablePointer<Int32>!, _ len: Int32) {
  return unsafe foo(p, len)
}
func call_control_group_method(_ self: inout Bar, _ p: UnsafeMutablePointer<Int32>!, _ len: Int32) {
  return unsafe self.control_group_method(p, len)
}
func call_baz(_ self: inout Bar, _ p: UnsafeMutablePointer<Int32>!, _ len: Int32) {
  return unsafe self.baz(p, len)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_control_group_method(_ self: inout Bar, _ p: UnsafeMutableBufferPointer<Int32>) {
  return unsafe self.control_group_method(p)
}

@_alwaysEmitIntoClient @_disfavoredOverload public func call_control_group_function(_ p: UnsafeMutableBufferPointer<Int32>) {
  return unsafe control_group_function(p)
}
