// Test the case when we have a single file in a module.
//
// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift -module-name comments -emit-module -emit-module-path %t/comments.swiftmodule -emit-module-doc -emit-module-doc-path %t/comments.swiftdoc %s
// RUN: llvm-bcanalyzer %t/comments.swiftmodule | FileCheck %s -check-prefix=BCANALYZER
// RUN: llvm-bcanalyzer %t/comments.swiftdoc | FileCheck %s -check-prefix=BCANALYZER
// RUN: %swift-ide-test -print-module-comments -module-to-print=comments -source-filename %s -I %t | FileCheck %s -check-prefix=FIRST

// Test the case when we have a multiple files in a module.
//
// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %swift -module-name comments -emit-module -emit-module-path %t/first.swiftmodule -emit-module-doc -emit-module-doc-path %t/first.swiftdoc -primary-file %s %S/Inputs/def_comments.swift
// RUN: %swift -module-name comments -emit-module -emit-module-path %t/second.swiftmodule -emit-module-doc -emit-module-doc-path %t/second.swiftdoc %s -primary-file %S/Inputs/def_comments.swift
// RUN: %swift -module-name comments -emit-module -emit-module-path %t/comments.swiftmodule -emit-module-doc -emit-module-doc-path %t/comments.swiftdoc %t/first.swiftmodule %t/second.swiftmodule
// RUN: llvm-bcanalyzer %t/comments.swiftmodule | FileCheck %s -check-prefix=BCANALYZER
// RUN: llvm-bcanalyzer %t/comments.swiftdoc | FileCheck %s -check-prefix=BCANALYZER
// RUN: %swift-ide-test -print-module-comments -module-to-print=comments -source-filename %s -I %t > %t.printed.txt
// RUN: FileCheck %s -check-prefix=FIRST < %t.printed.txt
// RUN: FileCheck %s -check-prefix=SECOND < %t.printed.txt

// BCANALYZER-NOT: UnknownCode

/// first_decl_class_1 Aaa.
class first_decl_class_1 {

  /// decl_func_1 Aaa.
  func decl_func_1() {}

  /**
   * decl_func_3 Aaa.
   */
  func decl_func_2() {}

  /// decl_func_3 Aaa.
  /** Bbb. */
  func decl_func_3() {}
}

// FIRST: first_decl_class_1 RawComment=[/// first_decl_class_1 Aaa.\n]
// FIRST: decl_func_1 RawComment=[/// decl_func_1 Aaa.\n]
// FIRST: decl_func_2 RawComment=[/**\n   * decl_func_3 Aaa.\n   */]
// FIRST: decl_func_3 RawComment=[/// decl_func_3 Aaa.\n/** Bbb. */]

// SECOND: second_decl_class_1 RawComment=[/// second_decl_class_1 Aaa.\n]

