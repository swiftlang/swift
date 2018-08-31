// REQUIRES: objc_interop
// RUN: %target-swift-frontend -typecheck -swift-version 3 %s
// RUN: %empty-directory(%t) && %target-swift-frontend -c -primary-file %S/Inputs/objc_inference.swift -swift-version 3 -emit-migrated-file-path %t/minimal_objc_inference.swift.result -emit-remap-file-path %t/minimal_objc_inference.swift.remap -o /dev/null
// RUN: diff -u %S/minimal_objc_inference.swift.expected %t/minimal_objc_inference.swift.result
// RUN: %target-swift-frontend -typecheck -swift-version 4 %t/minimal_objc_inference.swift.result
