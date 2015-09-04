// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse -verify -I %S/Inputs/custom-modules %s
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse -verify -I %S/Inputs/custom-modules -D REDECL_FIRST %s

// REQUIRES: objc_interop

// Do not import Foundation! This tests indirect visibility.
#if REDECL_FIRST
import Redeclaration
import AppKit
#else
import AppKit
import Redeclaration
#endif

let encoding: UInt = NSUTF8StringEncoding

let fromTypedef: Redeclaration.NSPoint = AppKit.NSPoint(x: 0, y: 0)
let fromStruct: Redeclaration.Point3D = AppKit.Point3D()
Redeclaration.NSStringToNSString(AppKit.NSStringToNSString("abc"))
