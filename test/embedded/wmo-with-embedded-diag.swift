// RUN: %target-swift-frontend -typecheck -parse-stdlib %s -enable-experimental-feature Embedded
// RUN: not %target-swift-frontend -typecheck -parse-stdlib %s -primary-file %s -enable-experimental-feature Embedded 2>&1 | %FileCheck %s

// RUN: %target-swiftc_driver -typecheck -parse-stdlib %s -Xfrontend -disable-objc-interop -enable-experimental-feature Embedded -wmo
// RUN: not %target-swiftc_driver -typecheck -parse-stdlib %s -Xfrontend -disable-objc-interop -enable-experimental-feature Embedded 2>&1 | %FileCheck %s

// CHECK: error: Whole module optimization (wmo) must be enabled with embedded Swift.
