// RUN: %target-swift-remoteast-test-with-sdk %s | %FileCheck %s

// REQUIRES: swift-remoteast-test
// REQUIRES: objc_interop

import Foundation

@_silgen_name("printMetadataType")
func printType(_: Any.Type)

printType(NSString.self)
// CHECK: NSString
