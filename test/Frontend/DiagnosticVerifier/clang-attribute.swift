// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: not %target-swift-frontend -typecheck -verify %t/test.swift -I %t -plugin-path %swift-plugin-dir -Rmacro-expansions 2>&1 | \
// RUN: %FileCheck %s --implicit-check-not error: --implicit-check-not note: --implicit-check-not warning: --implicit-check-not remark: --match-full-lines --sanitize TEST_H=%t%{fs-sep}test.h

//--- test.h
void foo(int len, int *p) __attribute__((
    swift_attr("@_SwiftifyImport(.countedBy(pointer: .param(2), count: \"len\"))")));

//--- test.swift
import TestClang

func bar(x: UnsafeMutableBufferPointer<CInt>) {
  foo(x) // trigger macro expansion
}

//--- module.modulemap
module TestClang {
  header "test.h"
  export *
}

// CHECK: @__swiftmacro_So3foo15_SwiftifyImportfMp_.swift:1:1: error: unexpected remark produced: macro content: |/// This is an auto-generated wrapper for safer interop|
// CHECK: TEST_H:1:25: note: in expansion from here
// CHECK: TEST_H:1:25: note: file 'TEST_H' is not parsed for 'expected' statements. Use '-verify-additional-file TEST_H' to enable, or '-verify-ignore-unrelated' to ignore diagnostics in this file
// CHECK: <empty-filename>:1:1: error: unexpected note produced: in expansion of macro '_SwiftifyImport' on global function 'foo' here
//  // CHECK-NEXT: @_SwiftifyImport(.countedBy(pointer: .param(2), count: "len"))
// CHECK: TEST_H:1:6: note: in expansion from here
// CHECK: TEST_H:1:6: note: file 'TEST_H' is not parsed for 'expected' statements. Use '-verify-additional-file TEST_H' to enable, or '-verify-ignore-unrelated' to ignore diagnostics in this file 

// CHECK: @__swiftmacro_So3foo15_SwiftifyImportfMp_.swift:2:1: error: unexpected remark produced: macro content: |@_alwaysEmitIntoClient @_disfavoredOverload public func foo(_ p: UnsafeMutableBufferPointer<Int32>) {|
// CHECK: TEST_H:1:25: note: in expansion from here
// CHECK: TEST_H:1:25: note: file 'TEST_H' is not parsed for 'expected' statements. Use '-verify-additional-file TEST_H' to enable, or '-verify-ignore-unrelated' to ignore diagnostics in this file
// CHECK: <empty-filename>:1:1: error: unexpected note produced: in expansion of macro '_SwiftifyImport' on global function 'foo' here
  // CHECK-NEXT: @_SwiftifyImport(.countedBy(pointer: .param(2), count: "len"))
// CHECK: TEST_H:1:6: note: in expansion from here
// CHECK: TEST_H:1:6: note: file 'TEST_H' is not parsed for 'expected' statements. Use '-verify-additional-file TEST_H' to enable, or '-verify-ignore-unrelated' to ignore diagnostics in this file 

// CHECK: @__swiftmacro_So3foo15_SwiftifyImportfMp_.swift:3:1: error: unexpected remark produced: macro content: |    let len = Int32(exactly: p.count)!|
// CHECK: TEST_H:1:25: note: in expansion from here
// CHECK: TEST_H:1:25: note: file 'TEST_H' is not parsed for 'expected' statements. Use '-verify-additional-file TEST_H' to enable, or '-verify-ignore-unrelated' to ignore diagnostics in this file
// CHECK: <empty-filename>:1:1: error: unexpected note produced: in expansion of macro '_SwiftifyImport' on global function 'foo' here
  // CHECK-NEXT: @_SwiftifyImport(.countedBy(pointer: .param(2), count: "len"))
// CHECK: TEST_H:1:6: note: in expansion from here
// CHECK: TEST_H:1:6: note: file 'TEST_H' is not parsed for 'expected' statements. Use '-verify-additional-file TEST_H' to enable, or '-verify-ignore-unrelated' to ignore diagnostics in this file 

// CHECK: @__swiftmacro_So3foo15_SwiftifyImportfMp_.swift:4:1: error: unexpected remark produced: macro content: |    return unsafe foo(len, p.baseAddress!)|
// CHECK: TEST_H:1:25: note: in expansion from here
// CHECK: TEST_H:1:25: note: file 'TEST_H' is not parsed for 'expected' statements. Use '-verify-additional-file TEST_H' to enable, or '-verify-ignore-unrelated' to ignore diagnostics in this file
// CHECK: <empty-filename>:1:1: error: unexpected note produced: in expansion of macro '_SwiftifyImport' on global function 'foo' here
  // CHECK-NEXT: @_SwiftifyImport(.countedBy(pointer: .param(2), count: "len"))
// CHECK: TEST_H:1:6: note: in expansion from here
// CHECK: TEST_H:1:6: note: file 'TEST_H' is not parsed for 'expected' statements. Use '-verify-additional-file TEST_H' to enable, or '-verify-ignore-unrelated' to ignore diagnostics in this file 

// CHECK: @__swiftmacro_So3foo15_SwiftifyImportfMp_.swift:5:1: error: unexpected remark produced: macro content: |}|
// CHECK: TEST_H:1:25: note: in expansion from here
// CHECK: TEST_H:1:25: note: file 'TEST_H' is not parsed for 'expected' statements. Use '-verify-additional-file TEST_H' to enable, or '-verify-ignore-unrelated' to ignore diagnostics in this file
// CHECK: <empty-filename>:1:1: error: unexpected note produced: in expansion of macro '_SwiftifyImport' on global function 'foo' here
  // CHECK-NEXT: @_SwiftifyImport(.countedBy(pointer: .param(2), count: "len"))
// CHECK: TEST_H:1:6: note: in expansion from here
// CHECK: TEST_H:1:6: note: file 'TEST_H' is not parsed for 'expected' statements. Use '-verify-additional-file TEST_H' to enable, or '-verify-ignore-unrelated' to ignore diagnostics in this file 
