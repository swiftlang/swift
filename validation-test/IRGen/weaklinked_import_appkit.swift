// RUN: %target-swift-frontend -emit-ir %s | %FileCheck %s

// REQUIRES: OS=macosx

@_weakLinked import AppKit
import Foundation

public func test() {
  // CHECK-DAG: @"OBJC_CLASS_$_NSView" = extern_weak global %objc_class
  _ = NSView()

  // CHECK-DAG: @"OBJC_CLASS_$_NSTextContainer" = extern_weak global %objc_class
  _ = NSTextContainer()

  // CHECK-DAG: declare extern_weak ptr @CGColorSpaceCreateDeviceRGB()
  _ = CGColorSpaceCreateDeviceRGB()

  // CHECK-DAG: @"OBJC_CLASS_$_NSNotification" = external global %objc_class
  _ = NSNotification(name: .init(""), object: nil)
}
