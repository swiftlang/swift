// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Core -target arm64-apple-macos15.0 -enable-experimental-feature Embedded -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/core.h
// RUN: %FileCheck %s < %t/core.h

// REQUIRES: OS=macosx
// REQUIRES: embedded_stdlib
// REQUIRES: swift_feature_Embedded

public func id(_ x: Int) -> Int {
    return x
}

// CHECK-NOT: TypeMetadataTrait<bool>
