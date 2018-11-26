// Test Objective-C bridging for a non-Foundation type that is
// otherwise unknown to the compiler.

// RUN: %empty-directory(%t)

// Build the Appliances module
// RUN: %target-clang -fobjc-arc -I %S/../../Inputs/ObjCBridging %S/../../Inputs/ObjCBridging/Appliances.m -c -o %t/AppliancesObjC.o
// RUN: %target-build-swift -emit-module -I %S/../../Inputs/ObjCBridging %S/../../Inputs/ObjCBridging/Appliances.swift -module-name Appliances -o %t
// RUN: %target-build-swift -c -parse-as-library -I %S/../../Inputs/ObjCBridging %S/../../Inputs/ObjCBridging/Appliances.swift -module-name Appliances -o %t/AppliancesSwift.o

// RUN: %target-build-swift -I %t -I %S/../../Inputs/ObjCBridging %s %t/AppliancesSwift.o %t/AppliancesObjC.o -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation
import Appliances

let home = APPHouse()

// CHECK: 50
print(home.fridge.temperature)

// CHECK: 75
home.fridge.temperature += 25.0
print(home.fridge.temperature)

var fridge: Refrigerator = home.fridge
fridge.temperature += 25.0

// CHECK: 100
print(fridge)

// CHECK: 75
print(home.fridge.temperature)

home.fridge = fridge

// CHECK: 100
print(home.fridge.temperature)

// Check dynamic casting
let obj: AnyObject = home.fridge as APPRefrigerator
if let f2 = obj as? Refrigerator {
  // CHECK: Fridge has temperature 100
  print("Fridge has temperature \(f2.temperature)")
}

// Check improper nullability auditing of `id` interfaces. `nil` should come
// through as a nonnull `Any` without crashing.
autoreleasepool {
  let broken = APPBroken()
  let thing = broken.thing
}

// CHECK: DONE
print("DONE")
