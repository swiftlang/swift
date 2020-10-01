// Check that we generate the right strings.
// RUN: %target-swift-emit-silgen -module-name Foo %/s | %FileCheck %s

// Even if concise #file is not available, we now allow you to write #filePath.
// Check that we don't diagnose any errors in this file.
// RUN: %target-typecheck-verify-swift -module-name Foo %s

// #filePath should appear in swiftinterfaces with this change.
// RUN: %target-swift-frontend-typecheck -module-name Foo -emit-module-interface-path %t.swiftinterface %s
// RUN: %FileCheck -check-prefix SWIFTINTERFACE %s < %t.swiftinterface

// FIXME: Once this feature has been fully staged in, we should duplicate
// existing #file tests and delete this file.

func directUse() {
  print(#filePath)

// CHECK-LABEL: sil {{.*}} @$s3Foo9directUseyyF
// CHECK: string_literal utf8 "SOURCE_DIR/test/SILGen/magic_identifier_filepath.swift"
}

func indirectUse() {
  functionWithFilePathDefaultArgument()

// CHECK-LABEL: sil {{.*}} @$s3Foo11indirectUseyyF
// CHECK: string_literal utf8 "SOURCE_DIR/test/SILGen/magic_identifier_filepath.swift"
}

public func functionWithFilePathDefaultArgument(file: String = #filePath) {}
// SWIFTINTERFACE: public func functionWithFilePathDefaultArgument(file: Swift.String = #filePath)
