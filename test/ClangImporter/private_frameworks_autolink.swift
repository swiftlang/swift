// RUN: %empty-directory(%t)

// FIXME: BEGIN -enable-source-import hackaround
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource) -emit-module -o %t %clang-importer-sdk-path/swift-modules/CoreGraphics.swift
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -emit-module -o %t %clang-importer-sdk-path/swift-modules/Foundation.swift
// FIXME: END -enable-source-import hackaround

// Check that the autolink information is appropriate (do not link against SomeKitCore).
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk-nosource -I %t) -emit-ir -o %t/private_frameworks_autolink.ll -F %S/Inputs/privateframeworks/withprivate-autolink %s
// RUN: %FileCheck %s < %t/private_frameworks_autolink.ll
// CHECK-NOT: !{!"-framework", !"SomeKitCore"}

// REQUIRES: objc_interop

import SomeKitCore
import SomeKit
