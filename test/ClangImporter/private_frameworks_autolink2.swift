// RUN: %empty-directory(%t)

// Check that the autolink information is appropriate (do not link against SomeKitCore).
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-ir -o %t/private_frameworks_autolink2.ll -F %S/Inputs/privateframeworks/withprivate-autolink %s
// RUN: %FileCheck %s < %t/private_frameworks_autolink2.ll
// CHECK-NOT: !{!"-framework", !"SomeKit"}
// CHECK: !{!"-framework", !"SomeKitCore"}

// REQUIRES: objc_interop

import SomeKitCore
