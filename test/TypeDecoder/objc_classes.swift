// RUN: %empty-directory(%t)

// RUN: %target-build-swift -emit-executable %s -g -o %t/objc_classes -emit-module
// RUN: sed -ne '/\/\/ *DEMANGLE: /s/\/\/ *DEMANGLE: *//p' < %s > %t/input
// RUN: %lldb-moduleimport-test-with-sdk %t/objc_classes -type-from-mangled=%t/input | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

func blackHole(_: Any...) {}

@objc protocol OurObjCProtocol {}
class OurObjCClass : NSObject, OurObjCProtocol {}

do {
  let x1: NSSet = NSSet()
  let x2: NSFastEnumeration = x1
  let x3: OurObjCProtocol = OurObjCClass()

  blackHole(x1, x2, x3)
}

do {
  let x1: NSSet.Type = NSSet.self
  let x2: NSFastEnumeration.Type = x1
  let x3: OurObjCProtocol.Type = OurObjCClass.self

  blackHole(x1, x2, x3)
}

do {
  let x1: NSFastEnumeration.Protocol = NSFastEnumeration.self
  let x2: OurObjCProtocol.Protocol = OurObjCProtocol.self

  blackHole(x1, x2)
}

// DEMANGLE: $sSo5NSSetCD
// DEMANGLE: $sSo17NSFastEnumeration_pD
// DEMANGLE: $s12objc_classes15OurObjCProtocol_pD

// CHECK: NSSet
// CHECK: NSFastEnumeration
// CHECK: OurObjCProtocol

// DEMANGLE: $sSo5NSSetCmD
// DEMANGLE: $sSo5NSSetCXMTD
// DEMANGLE: $sSo17NSFastEnumeration_pXpD
// DEMANGLE: $s12objc_classes15OurObjCProtocol_pXpD

// CHECK: NSSet.Type
// CHECK: @thick NSSet.Type

// CHECK: NSFastEnumeration.Type
// CHECK: OurObjCProtocol.Type

// DEMANGLE: $sSo17NSFastEnumeration_pmD
// DEMANGLE: $s12objc_classes15OurObjCProtocol_pmD

// CHECK: NSFastEnumeration.Protocol
// CHECK: OurObjCProtocol.Protocol
