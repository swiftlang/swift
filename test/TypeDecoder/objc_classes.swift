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
  let x4: NSCache = NSCache<NSNumber, NSString>()
  let x5: PropertyListSerialization.WriteOptions = 0

  blackHole(x1, x2, x3, x4, x5)
}

do {
  let x1: NSSet.Type = NSSet.self
  let x2: NSFastEnumeration.Type = x1
  let x3: OurObjCProtocol.Type = OurObjCClass.self
  let x4: NSCache.Type = NSCache<NSNumber, NSString>.self
  let x5: PropertyListSerialization.WriteOptions = 0

  blackHole(x1, x2, x3, x4, x5)
}

do {
  let x1: NSFastEnumeration.Protocol = NSFastEnumeration.self
  let x2: OurObjCProtocol.Protocol = OurObjCProtocol.self

  blackHole(x1, x2)
}

// DEMANGLE: $sSo5NSSetCD
// DEMANGLE: $sSo17NSFastEnumeration_pD
// DEMANGLE: $s12objc_classes15OurObjCProtocol_pD
// DEMANGLE: $sSo7NSCacheCySo8NSNumberCSo8NSStringCGD
// DEMANGLE: $sSo26NSPropertyListWriteOptionsaD

// CHECK: NSSet
// CHECK: NSFastEnumeration
// CHECK: OurObjCProtocol
// CHECK: NSCache<NSNumber, NSString>
// CHECK: PropertyListSerialization.WriteOptions

// DEMANGLE: $sSo5NSSetCmD
// DEMANGLE: $sSo5NSSetCXMTD
// DEMANGLE: $sSo17NSFastEnumeration_pXpD
// DEMANGLE: $s12objc_classes15OurObjCProtocol_pXpD
// DEMANGLE: $sSo7NSCacheCySo8NSNumberCSo8NSStringCGmD
// DEMANGLE: $sSo26NSPropertyListWriteOptionsamD

// CHECK: NSSet.Type
// CHECK: @thick NSSet.Type

// CHECK: NSFastEnumeration.Type
// CHECK: OurObjCProtocol.Type

// CHECK: NSCache<NSNumber, NSString>.Type

// CHECK: PropertyListSerialization.WriteOptions.Type

// DEMANGLE: $sSo17NSFastEnumeration_pmD
// DEMANGLE: $s12objc_classes15OurObjCProtocol_pmD

// CHECK: NSFastEnumeration.Protocol
// CHECK: OurObjCProtocol.Protocol
