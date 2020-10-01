// RUN: %empty-directory(%t)
// RUN: not %target-swift-frontend -I %S/Inputs/failing-overlay/ -typecheck %s 2>&1 | %FileCheck %s

import ImportsOverlay

// CHECK: HasOverlay.swiftinterface:1:1: error: failed to build module 'HasOverlay' from its module interface; the compiler that produced it, '(unspecified, file possibly handwritten)', may have used features that aren't supported by this compiler, '{{.*}}Swift version{{.*}}'
