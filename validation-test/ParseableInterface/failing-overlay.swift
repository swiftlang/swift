// RUN: %empty-directory(%t)
// RUN: not %target-swift-frontend -I %S/Inputs/failing-overlay/ -typecheck %s 2>&1 | %FileCheck %s

import ImportsOverlay

// FIXME: It would be better if this had a useful location, like inside the
// C header that caused the importing of the overlay.
// CHECK: <unknown>:0: error: failed to build module 'HasOverlay' from its module interface; the compiler that produced it, '(unspecified, file possibly handwritten)', may have used features that aren't supported by this compiler, '{{.*}}Swift version{{.*}}'
