// RUN: %target-swift-remoteast-test -sdk %S/../IRGen/Inputs %s | %FileCheck %s

// REQUIRES: objc_interop

import CoreCooling

@_silgen_name("printMetadataType")
func printType(_: Any.Type)

printType(CCRefrigerator.self)
// CHECK: found type: CCRefrigerator
