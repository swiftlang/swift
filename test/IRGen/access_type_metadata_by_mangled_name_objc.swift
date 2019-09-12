// RUN: %empty-directory(%t)
// RUN: %build-irgen-test-overlays
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) %s -emit-ir | %FileCheck %s

// REQUIRES: objc_interop

import Foundation
import CoreCooling

// CHECK-LABEL: define {{.*}} @"$s41access_type_metadata_by_mangled_name_objc4testyyF"()
public func test() {
  var x: Any.Type

  // Access ObjC classes by mangled name.
  // CHECK: @"$sSo8NSObjectCMD"
  x = NSObject.self

  // Use the metadata accessor for CF classes that already has to exist.
  // CHECK: @"$sSo17CCRefrigeratorRefaMa"
  x = CCRefrigerator.self
}
