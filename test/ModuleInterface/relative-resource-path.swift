// REQUIRES: OS=macosx

// RUN: %empty-directory(%t.relative_resource_path)
// RUN: %empty-directory(%t.mcp)
// RUN: cp -R %S/Inputs/resource_dir %t.relative_resource_path/
// RUN: cd %t.relative_resource_path

// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs/stdlib_rebuild -module-cache-path %t.mcp) -target arm64-apple-macosx15.0 \
// RUN:   -resource-dir resource_dir -compile-module-from-interface -o OtherModule.swiftmodule \
// RUN:   %S/Inputs/stdlib_rebuild/usr/lib/swift/OtherModule.swiftmodule/arm64-apple-macos.swiftinterface
// RUN: llvm-bcanalyzer -dump OtherModule.swiftmodule | %FileCheck %s

// CHECK-NOT: <DEPENDENCY_DIRECTORY abbrevid=11/> blob data = '/
