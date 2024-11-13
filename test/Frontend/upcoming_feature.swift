// Make sure that hasFeature(ConciseMagicFile) evaluates true when provided
// explicitly.
// RUN: %target-typecheck-verify-swift -enable-upcoming-feature ConciseMagicFile

// Make sure that hasFeature(ConciseMagicFile) evaluates true in Swift 6.
// RUN: %target-typecheck-verify-swift -swift-version 6

// Make sure that hasFeature(ConciseMagicFile) is off prior to Swift 6
// RUN: %target-typecheck-verify-swift -verify-additional-prefix swift5-

// It's fine to provide a feature that we don't know about
// RUN: %target-typecheck-verify-swift -enable-upcoming-feature ConciseMagicFile -enable-upcoming-feature UnknownFeature
// RUN: %target-typecheck-verify-swift -enable-upcoming-feature UnknownFeature -enable-upcoming-feature ConciseMagicFile

// For compatibility when a feature graduates, it's fine to refer to an
// upcoming feature as an experimental feature.
// RUN: %target-typecheck-verify-swift -enable-experimental-feature ConciseMagicFile

// It's not fine to provide a feature that's in the specified language version.
// RUN: not %target-swift-frontend -typecheck -enable-upcoming-feature ConciseMagicFile -swift-version 6 %s 2>&1 | %FileCheck %s --check-prefix=CHECK-ERROR
// RUN: %target-swift-frontend -typecheck -enable-experimental-feature ConciseMagicFile -swift-version 6 %s 2>&1 | %FileCheck %s --check-prefix=CHECK-WARN

// REQUIRES: swift_feature_ConciseMagicFile
// REQUIRES: !swift_feature_UnknownFeature

// CHECK-ERROR: error: upcoming feature 'ConciseMagicFile' is already enabled as of Swift version 6
// CHECK-WARN: warning: upcoming feature 'ConciseMagicFile' is already enabled as of Swift version 6

#if hasFeature(ConciseMagicFile)
let x = 0
#else
let y = boom // expected-swift5-error{{'boom'}}
#endif
