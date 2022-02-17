// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name UnavailableOnAllPlatforms -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name UnavailableOnAllPlatforms -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/UnavailableOnAllPlatforms.symbols.json

// REQUIRES: OS=macosx

// CHECK: ShouldAppear
@available(Linux, unavailable)
public struct ShouldAppear {}

// CHECK-NOT: ShouldntAppear
@available(*, unavailable)
public struct ShouldntAppear {

    // CHECK-NOT: shouldntAppearFunc
    public func shouldntAppearFunc() {}
}

// CHECK-NOT: shouldntAppearGlobalFunc
@available(*, unavailable)
public func shouldntAppearGlobalFunc() {}
