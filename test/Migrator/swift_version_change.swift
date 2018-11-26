// REQUIRES: OS=macosx
// RUN: %empty-directory(%t) && %target-swift-frontend -typecheck -update-code -primary-file %s -emit-migrated-file-path %t/dummpy.result -swift-version 4
// RUN: %target-swift-frontend -typecheck -update-code -primary-file %s -emit-migrated-file-path %t/dummpy.result -swift-version 5

func foo() {}