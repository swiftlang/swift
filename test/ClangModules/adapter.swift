// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -parse -verify -module-cache-path %t/clang-module-cache %clang-importer-sdk -I %S/Inputs/custom-modules %s

// Do not import Foundation! This tests indirect visibility.
import Redeclaration
import AppKit

let encoding: UInt = NSUTF8StringEncoding

let point: Redeclaration.NSPoint = AppKit.NSPoint(0, 0)
Redeclaration.NSStringToNSString(AppKit.NSStringToNSString("abc"))
