// REQUIRES: objc_interop
// REQUIRES: OS=macosx

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache)

// RUN: %target-swift-frontend -scan-dependencies -enable-objc-interop -module-load-mode prefer-interface -module-cache-path %t.module-cache %s -o %t.deps.json -I %S/Inputs/CHeaders -I %S/Inputs/Swift -target arm64e-apple-ios16.4-macabi -clang-target arm64e-apple-ios17.0-macabi -target-variant arm64e-apple-macosx14.4 -clang-target-variant arm64e-apple-macosx15.0

// RUN: %validate-json %t.deps.json | %FileCheck %s

// Ensure the flag affects Clang dependencies in the expected way
// CHECK: "clang": "X"
// CHECK: "modulePath": "{{.*}}X-{{.*}}.pcm"
// CHECK: "-darwin-target-variant-triple"
// CHECK-NEXT: "-Xcc"
// CHECK-NEXT: "arm64e-apple-macosx15.0"

// Ensure the flag is propagated to Swift dependencies
// CHECK: "-clang-target-variant"
// CHECK-NEXT: "arm64e-apple-macosx15.0"

import X

