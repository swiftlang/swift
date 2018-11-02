// REQUIRES: objc_interop
// RUN: %target-swift-frontend -typecheck -swift-version 3 -primary-file %S/Inputs/objc_inference_cross_file_A.swift %S/Inputs/objc_inference_cross_file_B.swift -swift-version 3
// RUN: %empty-directory(%t) && %target-swift-frontend -c -update-code -primary-file %S/Inputs/objc_inference_cross_file_A.swift %S/Inputs/objc_inference_cross_file_B.swift -emit-migrated-file-path %t/objc_inference_cross_file.swift.result -migrate-keep-objc-visibility -o /dev/null -swift-version 3
// RUN: diff -u %S/objc_inference_cross_file.swift.expected %t/objc_inference_cross_file.swift.result
// RUN: %target-swift-frontend -typecheck -swift-version 4 -primary-file %t/objc_inference_cross_file.swift.result %S/Inputs/objc_inference_cross_file_B.swift
