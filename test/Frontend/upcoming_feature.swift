// Make sure that hasFeature(ConciseMagicFile) evaluates true when provided
// explicitly.
// RUN: %target-swift-frontend -typecheck -enable-upcoming-feature ConciseMagicFile %s

// Make sure that hasFeature(ConciseMagicFile) evaluates true in Swift 6.
// RUN: %target-swift-frontend -typecheck -swift-version 6 %s

// Make sure that hasFeature(ConciseMagicFile) is off prior to Swift 6
// RUN: %target-typecheck-verify-swift %s

// It's fine to provide a feature that we don't know about
// RUN: %target-swift-frontend -typecheck -enable-upcoming-feature ConciseMagicFile -enable-upcoming-feature UnknownFeature %s
// RUN: %target-swift-frontend -typecheck -enable-upcoming-feature UnknownFeature -enable-upcoming-feature ConciseMagicFile %s

// For compatibility when a feature graduates, it's fine to refer to an
// upcoming feature as an experimental feature.
// RUN: %target-swift-frontend -typecheck -enable-experimental-feature ConciseMagicFile %s

// It's not fine to provide a feature that's in the specified language version.
// RUN: not %target-swift-frontend -typecheck -enable-upcoming-feature ConciseMagicFile -swift-version 6 %s 2>&1 | %FileCheck %s
// RUN: not %target-swift-frontend -typecheck -enable-experimental-feature ConciseMagicFile -swift-version 6 %s 2>&1 | %FileCheck %s

// CHECK: error: upcoming feature 'ConciseMagicFile' is already enabled as of Swift version 6

#if hasFeature(ConciseMagicFile)
let x = 0
#else
let y = boom // expected-error{{'boom'}}
#endif
