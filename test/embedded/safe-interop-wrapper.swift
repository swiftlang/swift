// REQUIRES: swift_feature_Lifetimes
// REQUIRES: swift_feature_Embedded

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -emit-module -plugin-path %swift-plugin-dir -I %t -enable-experimental-feature Lifetimes -strict-memory-safety %t/test.swift -enable-experimental-feature Embedded \
// RUN:   -verify -verify-additional-file %t%{fs-sep}test.h -Rmacro-expansions -suppress-notes

//--- test.h
#pragma once

#define __counted_by(x) __attribute__((__counted_by__(x)))
#define __noescape __attribute__((noescape))

// expected-expansion@+13:58{{
//   expected-remark@1{{macro content: |/// This is an auto-generated wrapper for safer interop|}}
//   expected-remark@2{{macro content: |@_alwaysEmitIntoClient @_lifetime(p: copy p) @_disfavoredOverload public func simple(_ p: inout MutableSpan<Int32>) {|}}
//   expected-remark@3{{macro content: |    let len = Int32(exactly: p.count)!|}}
//   expected-remark@4{{macro content: |    let _pPtr = p.withUnsafeMutableBufferPointer {|}}
//   expected-remark@5{{macro content: |        unsafe $0|}}
//   expected-remark@6{{macro content: |    }|}}
//   expected-remark@7{{macro content: |    defer {|}}
//   expected-remark@8{{macro content: |        _fixLifetime(p)|}}
//   expected-remark@9{{macro content: |    }|}}
//   expected-remark@10{{macro content: |    return unsafe simple(len, _pPtr.baseAddress!)|}}
//   expected-remark@11{{macro content: |}|}}
// }}
void simple(int len, int * __counted_by(len) __noescape p);

//--- module.modulemap
module Test {
  header "test.h"
}

//--- test.swift
// GENERATED-BY: %target-swift-ide-test -print-module -module-to-print=Test -plugin-path %swift-plugin-dir -I %t -source-filename=x -enable-experimental-feature Lifetimes -enable-experimental-feature Embedded > %t/Test-interface.swift && %swift-function-caller-generator Test %t/Test-interface.swift
// GENERATED-HASH: 2b8bfdc1396a0dd0376855c2b2e2103e5b25c5430ddc4232d58e53ae5f4baff9
import Test

func call_simple(_ len: Int32, _ p: UnsafeMutablePointer<Int32>!) {
  return unsafe simple(len, p)
}

@_lifetime(p: copy p)
@_alwaysEmitIntoClient @_disfavoredOverload public func call_simple(_ p: inout MutableSpan<Int32>) {
  return simple(&p)
}
