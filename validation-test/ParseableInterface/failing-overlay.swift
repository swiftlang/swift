// RUN: %empty-directory(%t)
// RUN: not %target-swift-frontend -I %S/Inputs/failing-overlay/ -typecheck %s -diagnostic-style llvm 2>&1 | %FileCheck %s

import ImportsOverlay

// CHECK: HasOverlay.swiftinterface:1:1: error: failed to build module 'HasOverlay'; this SDK is not supported by the compiler (the SDK is built with '(unspecified, file possibly handwritten)', while this compiler is '{{.*Swift version.*}}'). Please select a toolchain which matches the SDK.
