// REQUIRES: VENDOR=apple
// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-module %s -emit-module-path %t/Foo.swiftmodule -module-name Foo -emit-module-semantic-info-path %t/Foo.swiftsemanticinfo

// RUN: ls %t/Foo.swiftmodule
// RUN: ls %t/Foo.swiftsemanticinfo
