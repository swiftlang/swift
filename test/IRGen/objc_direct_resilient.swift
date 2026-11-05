// REQUIRES: objc_interop
// REQUIRES: swift_feature_ObjCDirect

// Clients call a direct member's native entry point statically, so in a
// resilient class it keeps the member's visibility, and the TBD agrees.

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-ir -enable-library-evolution \
// RUN:   -module-name Lib -enable-experimental-feature ObjCDirect \
// RUN:   -validate-tbd-against-ir=all -emit-tbd -emit-tbd-path %t/Lib.tbd \
// RUN:   -tbd-install_name Lib %s | %FileCheck %s
// RUN: %llvm-nm %t/Lib.tbd | %FileCheck %s --check-prefix=TBD

import Foundation

public class R: NSObject {
  // CHECK-DAG: define swiftcc {{i32|i64}} @"$s3Lib1RC1mSiyF"(
  @objcDirect public func m() -> Int { 1 }

  // CHECK-DAG: define swiftcc {{i32|i64}} @"$s3Lib1RC1xSivg"(
  @objcDirect public var x: Int { 2 }

  // _modify has no selector, so the TBD must not get a '-[R ]D' for it.
  // TBD-NOT: -[R ]D
  @objcDirect public var a = 0
  @objcDirect public var b = 0
}
