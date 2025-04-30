// RUN: %target-swift-frontend -typecheck -enable-upcoming-feature UnknownFeature %s 2>&1 | %FileCheck %s --check-prefix=CHECK-NO-DIAGS --allow-empty

// With -Wwarning StrictLanguageFeatures, emit extra diagnostics for
// misspecified features.
// RUN: %target-swift-frontend -typecheck -Wwarning StrictLanguageFeatures -enable-upcoming-feature UnknownFeature %s 2>&1 | %FileCheck %s --check-prefix=CHECK-UNKNOWN-UPCOMING
// RUN: %target-swift-frontend -typecheck -Wwarning StrictLanguageFeatures -enable-experimental-feature UnknownFeature %s 2>&1 | %FileCheck %s --check-prefix=CHECK-UNKNOWN-EXPERIMENTAL
// RUN: %target-swift-frontend -typecheck -Wwarning StrictLanguageFeatures -swift-version 5 -enable-experimental-feature ConciseMagicFile %s 2>&1 | %FileCheck %s --check-prefix=CHECK-NOT-EXPERIMENTAL-ENABLE
// RUN: %target-swift-frontend -typecheck -Wwarning StrictLanguageFeatures -swift-version 5 -enable-experimental-feature ConciseMagicFile -disable-experimental-feature ConciseMagicFile %s 2>&1 | %FileCheck %s --check-prefix=CHECK-NOT-EXPERIMENTAL-DISABLE

// REQUIRES: swift_feature_ConciseMagicFile

// CHECK-NO-DIAGS-NOT: warning:
// CHECK-NO-DIAGS-NOT: error:

// CHECK-UNKNOWN-UPCOMING: warning: 'UnknownFeature' is not a recognized upcoming feature
// CHECK-UNKNOWN-EXPERIMENTAL: warning: 'UnknownFeature' is not a recognized experimental feature
// CHECK-NOT-EXPERIMENTAL-ENABLE: warning: 'ConciseMagicFile' is not an experimental feature, use -enable-upcoming-feature instead
// CHECK-NOT-EXPERIMENTAL-DISABLE: warning: 'ConciseMagicFile' is not an experimental feature, use -disable-upcoming-feature instead
