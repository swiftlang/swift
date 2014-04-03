// RUN: %target-run-simple-swift | FileCheck %s

import Foundation
#if os(OSX)
import AppKit
typealias XXColor = NSColor
#endif
#if os(iOS)
import UIKit
typealias XXColor = UIColor
#endif


// Exercise some common APIs that make use of C pointer arguments.

//
// Typed C pointers
//

let rgb = CGColorSpaceCreateDeviceRGB()
let cgRed = CGColorCreate(rgb, [1.0, 0.0, 0.0, 1.0])

let nsRed = XXColor.colorWithCGColor(cgRed)

CGColorRelease(cgRed)
CGColorSpaceRelease(rgb)

var r: CGFloat = 0.5, g: CGFloat = 0.5, b: CGFloat = 0.5, a: CGFloat = 0.5
nsRed.getRed(&r, green: &g, blue: &b, alpha: &a)

// CHECK-LABEL: Red is:
println("Red is:")
println(r) // CHECK-NEXT: 1.0
println(g) // CHECK-NEXT: 0.0
println(b) // CHECK-NEXT: 0.0
println(a) // CHECK-NEXT: 1.0

//
// Void C pointers
//

let data = NSData.dataWithBytes([1.5, 2.25, 3.125],
                                length: sizeof(Double.self) * 3)
var fromData = [0.0, 0.0, 0.0]
data.getBytes(&fromData, length: sizeof(Double.self) * 3)

// CHECK-LABEL: Data is:
println("Data is:")
println(fromData[0]) // CHECK-NEXT: 1.5
println(fromData[1]) // CHECK-NEXT: 2.25
println(fromData[2]) // CHECK-NEXT: 3.125

//
// TODO: test NSError bridging
//
