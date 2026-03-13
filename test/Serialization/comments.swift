// Test the case when we have a single file in a module.
//
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -module-name comments -package-name comments -emit-module -emit-module-path %t/comments.swiftmodule -emit-module-doc -emit-module-doc-path %t/comments.swiftdoc -emit-module-source-info-path %t/comments.swiftsourceinfo %s
// RUN: llvm-bcanalyzer %t/comments.swiftmodule | %FileCheck %s -check-prefix=BCANALYZER
// RUN: llvm-bcanalyzer %t/comments.swiftdoc | %FileCheck %s -check-prefix=BCANALYZER
// RUN: llvm-bcanalyzer %t/comments.swiftsourceinfo | %FileCheck %s -check-prefix=BCANALYZER
// RUN: %target-swift-ide-test -print-module-comments -module-to-print=comments -enable-swiftsourceinfo -source-filename %s -I %t | %FileCheck %s -check-prefix=FIRST

// Test the case when we have a multiple files in a module.
//
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -module-name comments -package-name comments -emit-module -emit-module-path %t/first.swiftmodule -emit-module-doc -emit-module-doc-path %t/first.swiftdoc -primary-file %s %S/Inputs/def_comments.swift -emit-module-source-info-path %t/first.swiftsourceinfo
// RUN: %target-swift-frontend -module-name comments -package-name comments -emit-module -emit-module-path %t/second.swiftmodule -emit-module-doc -emit-module-doc-path %t/second.swiftdoc %s -primary-file %S/Inputs/def_comments.swift -emit-module-source-info-path %t/second.swiftsourceinfo
// RUN: %target-swift-frontend -module-name comments -package-name comments -emit-module -emit-module-path %t/comments.swiftmodule -emit-module-doc -emit-module-doc-path %t/comments.swiftdoc %t/first.swiftmodule %t/second.swiftmodule -emit-module-source-info-path %t/comments.swiftsourceinfo
// RUN: llvm-bcanalyzer %t/comments.swiftmodule | %FileCheck %s -check-prefix=BCANALYZER
// RUN: llvm-bcanalyzer %t/comments.swiftdoc | %FileCheck %s -check-prefix=BCANALYZER
// RUN: llvm-bcanalyzer %t/comments.swiftsourceinfo | %FileCheck %s -check-prefix=BCANALYZER
// RUN: %target-swift-ide-test -print-module-comments -module-to-print=comments -enable-swiftsourceinfo -source-filename %s -I %t > %t.printed.txt
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

/// Comment on package function
package func first_package_function() {}

/// Comment on SPI function
@_spi(DocSPI) public func first_spi_function() {}

// FIRST: comments.swift:26:14: Class/first_decl_generic_class_1 RawComment=[/// first_decl_generic_class_1 Aaa.\n]
// FIRST: comments.swift:28:3: Destructor/first_decl_generic_class_1.deinit RawComment=[/// deinit of first_decl_generic_class_1 Aaa.\n]
// FIRST: comments.swift:33:14: Class/first_decl_class_1 RawComment=[/// first_decl_class_1 Aaa.\n]
// FIRST: comments.swift:36:15: Func/first_decl_class_1.decl_func_1 RawComment=[/// decl_func_1 Aaa.\n]
// FIRST: comments.swift:41:15: Func/first_decl_class_1.decl_func_2 RawComment=[/**\n   * decl_func_3 Aaa.\n   */]
// FIRST: comments.swift:45:15: Func/first_decl_class_1.decl_func_3 RawComment=[/// decl_func_3 Aaa.\n/** Bbb. */]
// FIRST: comments.swift:64:14: Func/first_package_function RawComment=[/// Comment on package function\n]
// FIRST: comments.swift:67:27: Func/first_spi_function RawComment=[/// Comment on SPI function\n]

// SECOND: comments.swift:49:1: Extension/ RawComment=[/// Comment for bar1\n] BriefComment=[Comment for bar1]
// SECOND: comments.swift:54:1: Extension/ RawComment=[/// Comment for bar2\n] BriefComment=[Comment for bar2]
// SECOND: comments.swift:61:1: Extension/ RawComment=[/// Comment for no member extension\n] BriefComment=[Comment for no member extension]
// SECOND: comments.swift:64:14: Func/first_package_function RawComment=[/// Comment on package function\n]
// SECOND: comments.swift:67:27: Func/first_spi_function RawComment=[/// Comment on SPI function\n]
// SECOND: Inputs/def_comments.swift:2:14: Class/second_decl_class_1 RawComment=[/// second_decl_class_1 Aaa.\n]
// SECOND: Inputs/def_comments.swift:5:15: Struct/second_decl_struct_1
// SECOND: Inputs/def_comments.swift:7:9: Accessor/second_decl_struct_1.<getter for second_decl_struct_1.instanceVar>
// SECOND: Inputs/def_comments.swift:8:9: Accessor/second_decl_struct_1.<setter for second_decl_struct_1.instanceVar>
// SECOND: Inputs/def_comments.swift:10:17: Enum/second_decl_struct_1.NestedEnum
// SECOND: Inputs/def_comments.swift:11:22: TypeAlias/second_decl_struct_1.NestedTypealias
// SECOND: Inputs/def_comments.swift:14:13: Enum/second_decl_enum_1
// SECOND: Inputs/def_comments.swift:15:10: EnumElement/second_decl_enum_1.Case1
// SECOND: Inputs/def_comments.swift:16:10: EnumElement/second_decl_enum_1.Case2
// SECOND: Inputs/def_comments.swift:20:12: Constructor/second_decl_class_2.init
// SECOND: Inputs/def_comments.swift:23:17: Protocol/second_decl_protocol_1
// SECOND: Inputs/def_comments.swift:24:20: AssociatedType/second_decl_protocol_1.NestedTypealias
// SECOND: Inputs/def_comments.swift:25:5: Subscript/second_decl_protocol_1.subscript
// SECOND: Inputs/def_comments.swift:25:35: Accessor/second_decl_protocol_1.<getter for second_decl_protocol_1.subscript>
// SECOND: Inputs/def_comments.swift:25:39: Accessor/second_decl_protocol_1.<setter for second_decl_protocol_1.subscript>
// SECOND: Inputs/def_comments.swift:28:13: Var/decl_var_2 RawComment=none BriefComment=none DocCommentAsXML=none
// SECOND: Inputs/def_comments.swift:28:25: Var/decl_var_3 RawComment=none BriefComment=none DocCommentAsXML=none
// SECOND: Inputs/def_comments.swift:28:25: Var/decl_var_3 RawComment=none BriefComment=none DocCommentAsXML=none
// SECOND: Inputs/def_comments.swift:31:14: Func/second_package_function RawComment=[/// Comment on package function\n]
// SECOND: Inputs/def_comments.swift:34:27: Func/second_spi_function RawComment=[/// Comment on SPI function\n]
// SECOND: NonExistingSource.swift:100000:13: Func/functionAfterPoundSourceLoc

// Package and SPI functions won't show up in the (public) swiftinterface
// INTERFACE: comments.swift:26:14: Class/first_decl_generic_class_1 RawComment=[/// first_decl_generic_class_1 Aaa.\n]
// INTERFACE: comments.swift:28:3: Destructor/first_decl_generic_class_1.deinit RawComment=[/// deinit of first_decl_generic_class_1 Aaa.\n]
// INTERFACE: comments.swift:33:14: Class/first_decl_class_1 RawComment=[/// first_decl_class_1 Aaa.\n]
// INTERFACE: comments.swift:36:15: Func/first_decl_class_1.decl_func_1 RawComment=[/// decl_func_1 Aaa.\n]
// INTERFACE: comments.swift:41:15: Func/first_decl_class_1.decl_func_2 RawComment=[/**\n   * decl_func_3 Aaa.\n   */]
// INTERFACE: comments.swift:45:15: Func/first_decl_class_1.decl_func_3 RawComment=[/// decl_func_3 Aaa.\n/** Bbb. */]

// Test the case when we have to import via a .swiftinterface file.
//
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/Hidden)
// RUN: %target-swift-frontend -module-name comments -package-name comments -emit-module -emit-module-path %t/Hidden/comments.swiftmodule -emit-module-interface-path %t/comments.swiftinterface -emit-module-doc -emit-module-doc-path %t/comments.swiftdoc -emit-module-source-info-path %t/comments.swiftsourceinfo %s -enable-library-evolution -swift-version 5
// RUN: llvm-bcanalyzer %t/comments.swiftdoc | %FileCheck %s -check-prefix=BCANALYZER
// RUN: llvm-bcanalyzer %t/comments.swiftsourceinfo | %FileCheck %s -check-prefix=BCANALYZER
// RUN: %target-swift-ide-test -print-module-comments -module-to-print=comments -enable-swiftsourceinfo -source-filename %s -I %t -swift-version 5 | %FileCheck %s -check-prefix=INTERFACE
