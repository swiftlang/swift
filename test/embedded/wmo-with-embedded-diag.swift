// RUN: %target-swift-frontend -c %s -o %t -module-name wmowithembedded -parse-stdlib -enable-experimental-feature Embedded
// RUN: not %target-swift-frontend -c %s -o %t -module-name wmowithembedded -parse-stdlib -primary-file %s -enable-experimental-feature Embedded 2>&1 | %FileCheck %s

// RUN: %target-swiftc_driver -c %s -o %t -module-name wmowithembedded -parse-stdlib -Xfrontend -disable-objc-interop -enable-experimental-feature Embedded -wmo
// RUN: not %target-swiftc_driver -c %s -o %t -module-name wmowithembedded -parse-stdlib -Xfrontend -disable-objc-interop -enable-experimental-feature Embedded 2>&1 | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded

// CHECK: error: Whole module optimization (wmo) must be enabled with embedded Swift.
