// RUN: %target-swift-frontend -parse -verify %clang-importer-sdk -I %S/Inputs/custom-modules %s

// Do not import Foundation! This tests indirect visibility.
import Redeclaration
import AppKit

let encoding: UInt = NSUTF8StringEncoding

let point: Redeclaration.NSPoint = AppKit.NSPoint(x: 0, y: 0)
Redeclaration.NSStringToNSString(AppKit.NSStringToNSString("abc"))
