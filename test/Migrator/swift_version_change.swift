// REQUIRES: OS=macosx
// RUN: %empty-directory(%t) && %target-swift-frontend -typecheck -update-code -primary-file %s -emit-migrated-file-path %t/dummpy.result -swift-version 3
// RUN: %target-swift-frontend -typecheck -update-code -primary-file %s -emit-migrated-file-path %t/dummpy.result -swift-version 4

func foo() {}