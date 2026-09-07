// RUN: %target-swift-frontend -emit-ir %s | %FileCheck %s

// REQUIRES: OS=ios

@_weakLinked import UIKit
import Foundation

public func test() {
  // CHECK-DAG: @"OBJC_CLASS_$_UIPasteboard" = extern_weak global %objc_class
  _ = UIPasteboard.general

  // CHECK-DAG: @"OBJC_CLASS_$_UIView" = extern_weak global %objc_class
  _ = UIView()

  // CHECK-DAG: @"OBJC_CLASS_$_NSNotification" = external global %objc_class
  _ = NSNotification(name: .init(""), object: nil)
}
