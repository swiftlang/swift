// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Core -target arm64-apple-macos15.0 -enable-experimental-feature Embedded -clang-header-expose-decls=all-public -emit-clang-header-path %t/core.h
// RUN: %FileCheck %s < %t/core.h

// REQUIRES: OS=macosx

public func id(_ x: Int) -> Int {
    return x
}

// CHECK-NOT: TypeMetadataTrait<bool>