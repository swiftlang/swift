// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse -verify -I %S/Inputs/custom-modules %s

// REQUIRES: objc_interop

// Do not import Foundation! This tests indirect visibility.
import Redeclaration
import AppKit

let encoding: UInt = NSUTF8StringEncoding

let point: Redeclaration.NSPoint = AppKit.NSPoint(x: 0, y: 0)
Redeclaration.NSStringToNSString(AppKit.NSStringToNSString("abc"))
