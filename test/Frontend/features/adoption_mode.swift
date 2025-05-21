// NB: Repetitions are intentional. We are testing that a diagnostic is emitted
// for each option, not just the last one found.

// RUN: %target-swift-frontend -parse -swift-version 5                         \
// RUN:   -enable-upcoming-feature      DynamicActorIsolation:invalid1         \
// RUN:   -enable-upcoming-feature      DynamicActorIsolation:invalid2         \
// RUN:   -enable-upcoming-feature      DynamicActorIsolation:migrate         \
// RUN:   -enable-upcoming-feature      DynamicActorIsolation:migrate         \
// RUN:   -enable-experimental-feature  DynamicActorIsolation:invalid3         \
// RUN:   -enable-experimental-feature  DynamicActorIsolation:invalid4         \
// RUN:   -enable-experimental-feature  DynamicActorIsolation:migrate         \
// RUN:   -enable-experimental-feature  DynamicActorIsolation:migrate         \
// RUN:   -disable-upcoming-feature     DynamicActorIsolation:invalid5         \
// RUN:   -disable-upcoming-feature     DynamicActorIsolation:invalid6         \
// RUN:   -disable-upcoming-feature     DynamicActorIsolation:migrate         \
// RUN:   -disable-experimental-feature DynamicActorIsolation:invalid7         \
// RUN:   -disable-experimental-feature DynamicActorIsolation:invalid8         \
// RUN:   -disable-experimental-feature DynamicActorIsolation:migrate         \
// RUN:   %s 2>&1 | %FileCheck %s --check-prefix=CHECK-SWIFT-5

// RUN: %target-swift-frontend -parse -swift-version 6                         \
// RUN:   -enable-upcoming-feature      DynamicActorIsolation:invalid1         \
// RUN:   -enable-upcoming-feature      DynamicActorIsolation:migrate         \
// RUN:   -enable-experimental-feature  DynamicActorIsolation:invalid2         \
// RUN:   -enable-experimental-feature  DynamicActorIsolation:migrate         \
// RUN:   -disable-upcoming-feature     DynamicActorIsolation:invalid3         \
// RUN:   -disable-upcoming-feature     DynamicActorIsolation:migrate         \
// RUN:   -disable-experimental-feature DynamicActorIsolation:invalid4         \
// RUN:   -disable-experimental-feature DynamicActorIsolation:migrate         \
// RUN:   %s 2>&1 | %FileCheck %s --check-prefix=CHECK-SWIFT-6

// REQUIRES: swift_feature_DynamicActorIsolation

// CHECK-NOT: error:

// CHECK-SWIFT-5-NOT: warning:
// CHECK-SWIFT-5: warning: '-disable-experimental-feature' argument 'DynamicActorIsolation:migrate' cannot specify a mode [#StrictLanguageFeatures]{{$}}
// CHECK-SWIFT-5-NEXT: warning: '-disable-experimental-feature' argument 'DynamicActorIsolation:invalid8' cannot specify a mode [#StrictLanguageFeatures]{{$}}
// CHECK-SWIFT-5-NEXT: warning: '-disable-experimental-feature' argument 'DynamicActorIsolation:invalid7' cannot specify a mode [#StrictLanguageFeatures]{{$}}
// CHECK-SWIFT-5-NEXT: warning: '-disable-upcoming-feature' argument 'DynamicActorIsolation:migrate' cannot specify a mode [#StrictLanguageFeatures]{{$}}
// CHECK-SWIFT-5-NEXT: warning: '-disable-upcoming-feature' argument 'DynamicActorIsolation:invalid6' cannot specify a mode [#StrictLanguageFeatures]{{$}}
// CHECK-SWIFT-5-NEXT: warning: '-disable-upcoming-feature' argument 'DynamicActorIsolation:invalid5' cannot specify a mode [#StrictLanguageFeatures]{{$}}
// CHECK-SWIFT-5-NEXT: warning: feature 'DynamicActorIsolation' does not support migration mode [#StrictLanguageFeatures]{{$}}
// CHECK-SWIFT-5-NEXT: warning: feature 'DynamicActorIsolation' does not support migration mode [#StrictLanguageFeatures]{{$}}
// CHECK-SWIFT-5-NEXT: warning: 'invalid4' is not a recognized mode for feature 'DynamicActorIsolation' [#StrictLanguageFeatures]{{$}}
// CHECK-SWIFT-5-NEXT: warning: 'invalid3' is not a recognized mode for feature 'DynamicActorIsolation' [#StrictLanguageFeatures]{{$}}
// CHECK-SWIFT-5-NEXT: warning: feature 'DynamicActorIsolation' does not support migration mode [#StrictLanguageFeatures]{{$}}
// CHECK-SWIFT-5-NEXT: warning: feature 'DynamicActorIsolation' does not support migration mode [#StrictLanguageFeatures]{{$}}
// CHECK-SWIFT-5-NEXT: warning: 'invalid2' is not a recognized mode for feature 'DynamicActorIsolation' [#StrictLanguageFeatures]{{$}}
// CHECK-SWIFT-5-NEXT: warning: 'invalid1' is not a recognized mode for feature 'DynamicActorIsolation' [#StrictLanguageFeatures]{{$}}
// CHECK-SWIFT-5-NOT: warning:

// CHECK-SWIFT-6-NOT: warning:
// CHECK-SWIFT-6-COUNT-8: warning: upcoming feature 'DynamicActorIsolation' is already enabled as of Swift version 6{{$}}
// CHECK-SWIFT-6-NOT: warning:

// CHECK-NOT: error:
