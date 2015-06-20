// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -I %S/Inputs/custom-modules -Xcc -w -parse %s 2>&1 | FileCheck %s

// REQUIRES: objc_interop

// CHECK-NOT: warning:

import ObjCIRExtras

func test() {
  _ = SwiftNameTest()
  
  // We only see these two warnings because Clang can catch the other invalid
  // cases, and marks the attribute as invalid ahead of time.
  
  // CHECK: warning: too few parameters in swift_name attribute (expected 2; got 1)
  // CHECK: + (instancetype)g:(id)x outParam:(int *)foo SWIFT_NAME(init(g:));
  
  // CHECK: warning: too few parameters in swift_name attribute (expected 2; got 1)
  // CHECK: + (instancetype)testW:(id)x out:(id *)outObject SWIFT_NAME(ww(_:));
}

// CHECK-NOT: warning:
