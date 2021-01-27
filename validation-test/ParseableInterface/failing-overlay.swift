// RUN: %empty-directory(%t)
// RUN: not %target-swift-frontend -I %S/Inputs/failing-overlay/ -typecheck %s 2>&1 | %FileCheck %s

import ImportsOverlay

// CHECK: HasOverlay.swiftinterface:1:1: error: failed to build module 'HasOverlay' for importation due to the errors above; the textual interface may be broken by project issues, differences between compilers (the producer '(unspecified, file possibly handwritten)' and this compiler '{{.*Swift version.*}}') or a compiler bug
