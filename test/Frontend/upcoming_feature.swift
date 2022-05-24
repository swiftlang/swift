
// It's fine to provide a feature that we don't know about
// RUN: %target-swift-frontend -typecheck -enable-upcoming-feature UnknownFeature %s

// It's not fine to provide a feature that's in the specified language version.
// RUN: not %target-swift-frontend -typecheck -enable-upcoming-feature ConciseMagicFile -swift-version 6 %s 2>&1 | %FileCheck %s
// REQUIRES: asserts

// CHECK: error: upcoming feature 'ConciseMagicFile' is already enabled as of Swift version 6
