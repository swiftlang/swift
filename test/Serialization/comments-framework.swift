// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/comments.framework/Modules/comments.swiftmodule)

// RUN: %target-swift-frontend -module-name comments -emit-module -emit-module-path %t/comments.framework/Modules/comments.swiftmodule/%target-swiftmodule-name -emit-module-doc-path %t/comments.framework/Modules/comments.swiftmodule/%target-swiftdoc-name %s
// RUN: %target-swift-ide-test -print-module-comments -module-to-print=comments -source-filename %s -F %t | %FileCheck %s

// RUN: cp -r %t/comments.framework/Modules/comments.swiftmodule %t/comments.swiftmodule
// RUN: %target-swift-ide-test -print-module-comments -module-to-print=comments -source-filename %s -I %t | %FileCheck %s

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

// CHECK: Class/first_decl_class_1 RawComment=[/// first_decl_class_1 Aaa.\n]
// CHECK: Func/first_decl_class_1.decl_func_1 RawComment=[/// decl_func_1 Aaa.\n]
// CHECK: Func/first_decl_class_1.decl_func_2 RawComment=[/**\n   * decl_func_3 Aaa.\n   */]
// CHECK: Func/first_decl_class_1.decl_func_3 RawComment=[/// decl_func_3 Aaa.\n/** Bbb. */]

