// REQUIRES: objc_interop
// RUN: %target-swift-frontend -typecheck -swift-version 4 %s
// RUN: %empty-directory(%t) && %target-swift-frontend -c -primary-file %s -swift-version 4 -emit-migrated-file-path %t/objc_inference_noop.swift.result -emit-remap-file-path %t/objc_inference_noop.swift.remap -migrate-keep-objc-visibility -o /dev/null
// RUN: diff -u %s %t/objc_inference_noop.swift.result

import Foundation

@objc class Foo : NSObject {
  var foo: Int = 10
}
