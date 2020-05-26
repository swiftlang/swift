// Swiftdoc emission used to include entries for all the function parameters in
// the file too if a group info file was provided.

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -module-name comments -emit-module -emit-module-path %t/comments.swiftmodule -emit-module-doc -emit-module-doc-path %t/comments.swiftdoc -group-info-path %S/Inputs/comments-params.json %s
// RUN: %llvm-strings %t/comments.swiftdoc > %t.txt
// RUN: %FileCheck %s < %t.txt
// RUN: %FileCheck -check-prefix NEGATIVE %s < %t.txt

// CHECK-DAG: s:8comments4good
// CHECK-DAG: Good comment
// NEGATIVE-NOT: BAD_PARAM_NAME
// NEGATIVE-NOT: BAD_LOCAL_FUNC
// NEGATIVE-NOT: Bad comment

/// Good comment
public func good<T>(_ BAD_PARAM_NAME: T) -> T {
  /// Bad comment
  func BAD_LOCAL_FUNC() {}
  return BAD_PARAM_NAME
}
