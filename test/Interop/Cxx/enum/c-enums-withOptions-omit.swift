// RUN: %target-swift-ide-test -print-module -module-to-print=CenumsWithOptionsOmit -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s
// REQUIRES: objc_interop

import CenumsWithOptionsOmit

// CHECK: struct NSEnumerationOptions : OptionSet {
// CHECK-NEXT:   init(rawValue: Int32)
// CHECK-NEXT:   let rawValue: Int32
// CHECK-NEXT:   typealias RawValue = Int32
// CHECK-NEXT:   typealias Element = NSEnumerationOptions
// CHECK-NEXT:   typealias ArrayLiteralElement = NSEnumerationOptions

// CHECK: class NSSet {
// CHECK-NEXT: class func enumerateObjects(options
// CHECK-NEXT: func enumerateObjects(options
// CHECK-NEXT: @available(swift, obsoleted: 3, renamed: "enumerateObjects(options:)")
