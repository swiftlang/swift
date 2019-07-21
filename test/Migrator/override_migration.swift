// REQUIRES: objc_interop
// RUN: %target-swift-frontend -typecheck -swift-version 4 %s
// RUN: %empty-directory(%t) && %target-swift-frontend -c -primary-file %s -swift-version 4 -emit-migrated-file-path %t/override_migration.swift.result -emit-remap-file-path %t/override_migration.swift.remap -migrate-keep-objc-visibility -o /dev/null -api-diff-data-file %S/Inputs/override.json
// RUN: diff -u %s %t/override_migration.swift.result

import Foundation

@objc class Foo : NSObject {
  var foo: Int = 10
  override init() {}
}
