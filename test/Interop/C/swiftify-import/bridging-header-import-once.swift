// REQUIRES: swift_feature_SafeInteropWrappers

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module -import-bridging-header %t%{fs-sep}bridging_header.h -I %t -plugin-path %swift-plugin-dir -enable-experimental-feature SafeInteropWrappers %t/test.swift -Rmacro-expansions -verify -verify-additional-file %t%{fs-sep}header.h -disable-objc-interop

//--- module.modulemap
module Module {
    header "header.h"
}

//--- bridging_header.h
const int FOO = 42;

//--- header.h
#pragma once

#define __counted_by(x) __attribute__((__counted_by__(x)))

// expected-expansion@+10:52{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func func1(_ p: UnsafeBufferPointer<Int32>) {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    return unsafe func1(p.baseAddress!, len)|}}
//   expected-remark@5{{macro content: |}|}}
// }}
// expected-expansion@+3:6{{
//   expected-note@1 5{{in expansion of macro '_SwiftifyImport' on global function 'func1' here}}
// }}
void func1(const int * __counted_by(len) p, int len);

//--- test.swift
import Module

func g(s: UnsafeBufferPointer<CInt>) {
    func1(s)
}
