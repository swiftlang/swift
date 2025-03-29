// NB: Repetitions are intentional. We are testing that a diagnostic is emitted
// for each option, not just the last one found.

// RUN: %target-swift-frontend -parse -swift-version 6                         \
// RUN:   -enable-upcoming-feature      ExistentialAny:invalid1                \
// RUN:   -enable-upcoming-feature      ExistentialAny:adoption                \
// RUN:   -enable-experimental-feature  ExistentialAny:invalid2                \
// RUN:   -enable-experimental-feature  ExistentialAny:adoption                \
// RUN:   -disable-upcoming-feature     ExistentialAny:adoption                \
// RUN:   -disable-experimental-feature ExistentialAny:adoption                \
// RUN:   %s 2>&1 | %FileCheck %s --check-prefix=CHECK-SWIFT-5

// RUN: %target-swift-frontend -parse -swift-version 7                         \
// RUN:   -enable-upcoming-feature      ExistentialAny:invalid1                \
// RUN:   -enable-upcoming-feature      ExistentialAny:adoption                \
// RUN:   -enable-experimental-feature  ExistentialAny:invalid2                \
// RUN:   -enable-experimental-feature  ExistentialAny:adoption                \
// RUN:   -disable-upcoming-feature     ExistentialAny:adoption                \
// RUN:   -disable-experimental-feature ExistentialAny:adoption                \
// RUN:   %s 2>&1 | %FileCheck %s --check-prefix=CHECK-SWIFT-7

// REQUIRES: swift_feature_ExistentialAny
// REQUIRES: asserts

// CHECK-NOT: error:

// CHECK-SWIFT-5-NOT: warning:
// CHECK-SWIFT-5: warning: '-disable-experimental-feature' argument 'ExistentialAny:adoption' cannot specify a mode{{$}}
// CHECK-SWIFT-5-NEXT: warning: '-disable-upcoming-feature' argument 'ExistentialAny:adoption' cannot specify a mode{{$}}
// CHECK-SWIFT-5-NEXT: warning: 'invalid2' is not a recognized mode for feature 'ExistentialAny'; did you mean 'adoption'?{{$}}
// CHECK-SWIFT-5-NEXT: warning: 'invalid1' is not a recognized mode for feature 'ExistentialAny'; did you mean 'adoption'?{{$}}
// CHECK-SWIFT-5-NOT: warning:

// CHECK-SWIFT-7-NOT: warning:
// CHECK-SWIFT-7-COUNT-6: warning: upcoming feature 'ExistentialAny' is already enabled as of Swift version 7{{$}}
// CHECK-SWIFT-7-NOT: warning:

// CHECK-NOT: error:
