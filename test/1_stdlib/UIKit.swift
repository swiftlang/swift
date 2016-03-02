// RUN: %target-run-simple-swift | FileCheck %s
// REQUIRES: executable_test
// REQUIRES: OS=ios

import UIKit

func printOrientation(o: UIDeviceOrientation) {
  print("\(o.isPortrait) \(UIDeviceOrientationIsPortrait(o)), ", terminator: "")
  print("\(o.isLandscape) \(UIDeviceOrientationIsLandscape(o)), ", terminator: "")
  print("\(o.isFlat), ", terminator: "")
  print("\(o.isValidInterfaceOrientation) \(UIDeviceOrientationIsValidInterfaceOrientation(o))")
}

print("Device orientations")
printOrientation(UIDeviceOrientation.unknown)
printOrientation(UIDeviceOrientation.portrait)
printOrientation(UIDeviceOrientation.portraitUpsideDown)
printOrientation(UIDeviceOrientation.landscapeLeft)
printOrientation(UIDeviceOrientation.landscapeRight)
printOrientation(UIDeviceOrientation.faceUp)
printOrientation(UIDeviceOrientation.faceDown)
// CHECK:      Device orientations
// CHECK-NEXT: false false, false false, false, false false
// CHECK-NEXT: true true, false false, false, true true
// CHECK-NEXT: true true, false false, false, true true
// CHECK-NEXT: false false, true true, false, true true
// CHECK-NEXT: false false, true true, false, true true
// CHECK-NEXT: false false, false false, true, false false
// CHECK-NEXT: false false, false false, true, false false


func printOrientation(o: UIInterfaceOrientation) {
  print("\(o.isPortrait) \(UIInterfaceOrientationIsPortrait(o)), ", terminator: "")
  print("\(o.isLandscape) \(UIInterfaceOrientationIsLandscape(o))")
}

print("Interface orientations")
printOrientation(UIInterfaceOrientation.unknown)
printOrientation(UIInterfaceOrientation.portrait)
printOrientation(UIInterfaceOrientation.portraitUpsideDown)
printOrientation(UIInterfaceOrientation.landscapeLeft)
printOrientation(UIInterfaceOrientation.landscapeRight)
// CHECK:      Interface orientations
// CHECK-NEXT: false false, false false
// CHECK-NEXT: true true, false false
// CHECK-NEXT: true true, false false
// CHECK-NEXT: false false, true true
// CHECK-NEXT: false false, true true

var inset1 = UIEdgeInsets(top: 1.0, left: 2.0, bottom: 3.0, right: 4.0)
var inset2 = UIEdgeInsets(top: 1.0, left: 2.0, bottom: 3.1, right: 4.0)
print("inset1 == inset1: \(inset1 == inset1)")
print("inset1 != inset1: \(inset1 != inset1)")
print("inset1 == inset2: \(inset1 == inset2)")
// CHECK: inset1 == inset1: true
// CHECK: inset1 != inset1: false
// CHECK: inset1 == inset2: false

var offset1 = UIOffset(horizontal: 1.0, vertical: 2.0)
var offset2 = UIOffset(horizontal: 1.0, vertical: 3.0)
print("offset1 == offset1: \(offset1 == offset1)")
print("offset1 != offset1: \(offset1 != offset1)")
print("offset1 == offset2: \(offset1 == offset2)")
// CHECK: offset1 == offset1: true
// CHECK: offset1 != offset1: false
// CHECK: offset1 == offset2: false

