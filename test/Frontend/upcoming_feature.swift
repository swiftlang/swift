// Make sure that hasFeature(ConciseMagicFile) evaluates true when provided
// explicitly.
// RUN: %target-typecheck-verify-swift -enable-upcoming-feature ConciseMagicFile

// Make sure that hasFeature(ConciseMagicFile) evaluates true in Swift 6.
// RUN: %target-typecheck-verify-swift -swift-version 6

// Make sure that hasFeature(ConciseMagicFile) is off prior to Swift 6.
// RUN: %target-typecheck-verify-swift -verify-additional-prefix swift5-

// It's fine to provide a feature that we don't know about.
// RUN: %target-typecheck-verify-swift -enable-upcoming-feature ConciseMagicFile -enable-upcoming-feature UnknownFeature
// RUN: %target-typecheck-verify-swift -enable-upcoming-feature UnknownFeature -enable-upcoming-feature ConciseMagicFile

// When -disable-upcoming-feature is specified, leave the feature disabled.
// RUN: %target-typecheck-verify-swift -disable-upcoming-feature ConciseMagicFile -verify-additional-prefix swift5-

// When both -enable-upcoming-feature and -disable-upcoming-feature are
// specified, the result depends on the order.
// RUN: %target-typecheck-verify-swift -enable-upcoming-feature ConciseMagicFile -disable-upcoming-feature ConciseMagicFile -verify-additional-prefix swift5-
// RUN: %target-typecheck-verify-swift -disable-upcoming-feature ConciseMagicFile -enable-upcoming-feature ConciseMagicFile

// For compatibility when a feature graduates, it's fine to refer to an
// upcoming feature as an experimental feature.
// RUN: %target-typecheck-verify-swift -enable-experimental-feature ConciseMagicFile

// A feature that has graduated can also be disabled as an experimental feature.
// RUN: %target-typecheck-verify-swift -disable-experimental-feature ConciseMagicFile -verify-additional-prefix swift5-
// RUN: %target-typecheck-verify-swift -enable-experimental-feature ConciseMagicFile -disable-experimental-feature ConciseMagicFile -verify-additional-prefix swift5-
// RUN: %target-typecheck-verify-swift -disable-experimental-feature ConciseMagicFile -enable-experimental-feature ConciseMagicFile
// RUN: %target-typecheck-verify-swift -enable-upcoming-feature ConciseMagicFile -disable-experimental-feature ConciseMagicFile -verify-additional-prefix swift5-

// Warn about enabled features that are implied by the specified language version.
// RUN: %target-swift-frontend -typecheck -enable-upcoming-feature ConciseMagicFile -swift-version 6 %s 2>&1 | %FileCheck %s
// RUN: %target-swift-frontend -typecheck -disable-upcoming-feature ConciseMagicFile -swift-version 6 %s 2>&1 | %FileCheck %s
// RUN: %target-swift-frontend -typecheck -enable-experimental-feature ConciseMagicFile -swift-version 6 %s 2>&1 | %FileCheck %s
// RUN: %target-swift-frontend -typecheck -disable-experimental-feature ConciseMagicFile -swift-version 6 %s 2>&1 | %FileCheck %s

// REQUIRES: swift_feature_ConciseMagicFile

// CHECK: warning: upcoming feature 'ConciseMagicFile' is already enabled as of Swift version 6

#if hasFeature(ConciseMagicFile)
let x = 0
#else
let y = boom // expected-swift5-error{{'boom'}}
#endif
