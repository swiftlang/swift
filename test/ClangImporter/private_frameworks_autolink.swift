// RUN: %empty-directory(%t)

// Check that the autolink information is appropriate (do not link against SomeKitCore).
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-ir -o %t/private_frameworks_autolink.ll -F %S/Inputs/privateframeworks/withprivate-autolink %s
// RUN: %FileCheck %s < %t/private_frameworks_autolink.ll
// CHECK-NOT: !{!"-framework", !"SomeKitCore"}

// REQUIRES: objc_interop

import SomeKitCore
import SomeKit
