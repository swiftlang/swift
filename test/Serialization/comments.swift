// Test the case when we have a single file in a module.
//
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -module-name comments -emit-module -emit-module-path %t/comments.swiftmodule -emit-module-doc -emit-module-doc-path %t/comments.swiftdoc %s
// RUN: llvm-bcanalyzer %t/comments.swiftmodule | %FileCheck %s -check-prefix=BCANALYZER
// RUN: llvm-bcanalyzer %t/comments.swiftdoc | %FileCheck %s -check-prefix=BCANALYZER
// RUN: %target-swift-ide-test -print-module-comments -module-to-print=comments -source-filename %s -I %t | %FileCheck %s -check-prefix=FIRST

// Test the case when we have a multiple files in a module.
//
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -module-name comments -emit-module -emit-module-path %t/first.swiftmodule -emit-module-doc -emit-module-doc-path %t/first.swiftdoc -primary-file %s %S/Inputs/def_comments.swift
// RUN: %target-swift-frontend -module-name comments -emit-module -emit-module-path %t/second.swiftmodule -emit-module-doc -emit-module-doc-path %t/second.swiftdoc %s -primary-file %S/Inputs/def_comments.swift
// RUN: %target-swift-frontend -module-name comments -emit-module -emit-module-path %t/comments.swiftmodule -emit-module-doc -emit-module-doc-path %t/comments.swiftdoc %t/first.swiftmodule %t/second.swiftmodule
// RUN: llvm-bcanalyzer %t/comments.swiftmodule | %FileCheck %s -check-prefix=BCANALYZER
// RUN: llvm-bcanalyzer %t/comments.swiftdoc | %FileCheck %s -check-prefix=BCANALYZER
// RUN: %target-swift-ide-test -print-module-comments -module-to-print=comments -source-filename %s -I %t > %t.printed.txt
// RUN: %FileCheck %s -check-prefix=FIRST < %t.printed.txt
// RUN: %FileCheck %s -check-prefix=SECOND < %t.printed.txt

// BCANALYZER-NOT: UnknownCode

/// first_decl_generic_class_1 Aaa.
public class first_decl_generic_class_1<T> {
  /// deinit of first_decl_generic_class_1 Aaa.
  deinit {
  }
}

/// first_decl_class_1 Aaa.
public class first_decl_class_1 {

  /// decl_func_1 Aaa.
  public func decl_func_1() {}

  /**
   * decl_func_3 Aaa.
   */
  public func decl_func_2() {}

  /// decl_func_3 Aaa.
  /** Bbb. */
  public func decl_func_3() {}
}

/// Comment for bar1
extension first_decl_class_1 {
  func bar1(){}
}

/// Comment for bar2
extension first_decl_class_1 {
  func bar2(){}
}

public protocol P1 { }

/// Comment for no member extension
extension first_decl_class_1 : P1 {}

// FIRST: Class/first_decl_generic_class_1 RawComment=[/// first_decl_generic_class_1 Aaa.\n]
// FIRST: Destructor/first_decl_generic_class_1.deinit RawComment=[/// deinit of first_decl_generic_class_1 Aaa.\n]
// FIRST: Class/first_decl_class_1 RawComment=[/// first_decl_class_1 Aaa.\n]
// FIRST: Func/first_decl_class_1.decl_func_1 RawComment=[/// decl_func_1 Aaa.\n]
// FIRST: Func/first_decl_class_1.decl_func_2 RawComment=[/**\n   * decl_func_3 Aaa.\n   */]
// FIRST: Func/first_decl_class_1.decl_func_3 RawComment=[/// decl_func_3 Aaa.\n/** Bbb. */]

// SECOND: Extension/ RawComment=[/// Comment for bar1\n] BriefComment=[Comment for bar1]
// SECOND: Extension/ RawComment=[/// Comment for bar2\n] BriefComment=[Comment for bar2]
// SECOND: Extension/ RawComment=[/// Comment for no member extension\n] BriefComment=[Comment for no member extension]
// SECOND: Class/second_decl_class_1 RawComment=[/// second_decl_class_1 Aaa.\n]
