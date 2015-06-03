// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test
// REQUIRES: OS=ios

import UIKit

func printOrientation(o: UIDeviceOrientation) {
  print("\(o.isPortrait) \(UIDeviceOrientationIsPortrait(o)), ", appendNewline: false)
  print("\(o.isLandscape) \(UIDeviceOrientationIsLandscape(o)), ", appendNewline: false)
  print("\(o.isFlat), ", appendNewline: false)
  print("\(o.isValidInterfaceOrientation) \(UIDeviceOrientationIsValidInterfaceOrientation(o))")
}

print("Device orientations")
printOrientation(UIDeviceOrientation.Unknown)
printOrientation(UIDeviceOrientation.Portrait)
printOrientation(UIDeviceOrientation.PortraitUpsideDown)
printOrientation(UIDeviceOrientation.LandscapeLeft)
printOrientation(UIDeviceOrientation.LandscapeRight)
printOrientation(UIDeviceOrientation.FaceUp)
printOrientation(UIDeviceOrientation.FaceDown)
// CHECK:      Device orientations
// CHECK-NEXT: false false, false false, false, false false
// CHECK-NEXT: true true, false false, false, true true
// CHECK-NEXT: true true, false false, false, true true
// CHECK-NEXT: false false, true true, false, true true
// CHECK-NEXT: false false, true true, false, true true
// CHECK-NEXT: false false, false false, true, false false
// CHECK-NEXT: false false, false false, true, false false


func printOrientation(o: UIInterfaceOrientation) {
  print("\(o.isPortrait) \(UIInterfaceOrientationIsPortrait(o)), ", appendNewline: false)
  print("\(o.isLandscape) \(UIInterfaceOrientationIsLandscape(o))")
}

print("Interface orientations")
printOrientation(UIInterfaceOrientation.Unknown)
printOrientation(UIInterfaceOrientation.Portrait)
printOrientation(UIInterfaceOrientation.PortraitUpsideDown)
printOrientation(UIInterfaceOrientation.LandscapeLeft)
printOrientation(UIInterfaceOrientation.LandscapeRight)
// CHECK:      Interface orientations
// CHECK-NEXT: false false, false false
// CHECK-NEXT: true true, false false
// CHECK-NEXT: true true, false false
// CHECK-NEXT: false false, true true
// CHECK-NEXT: false false, true true
