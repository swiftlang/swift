// REQUIRES: objc_interop
// RUN: %target-swift-frontend -typecheck -swift-version 3 -enable-swift3-objc-inference %s
// RUN: %empty-directory(%t) && %target-swift-frontend -c -primary-file %S/Inputs/objc_inference.swift -swift-version 3 -emit-migrated-file-path %t/complete_objc_inference.swift.result -emit-remap-file-path %t/complete_objc_inference.swift.remap -migrate-keep-objc-visibility -o /dev/null
// RUN: diff -u %S/complete_objc_inference.swift.expected %t/complete_objc_inference.swift.result
// RUN: %target-swift-frontend -typecheck -swift-version 4 -enable-swift3-objc-inference %t/complete_objc_inference.swift.result
