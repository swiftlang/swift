// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -scan-dependencies -Xcc -v %s -o - | %validate-json | %FileCheck %s

// We want to explicitly import WinSDK's CRT.
// REQUIRES: OS=windows-msvc

import CRT

// CHECK:        "modulePath": "{{.*}}\\ucrt-{{.*}}.pcm",
// CHECK-NEXT:   "sourceFiles": [
// CHECK-NEXT:      "{{.*}}\\ucrt\\module.modulemap"
