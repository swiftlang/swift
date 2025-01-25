// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/Library.swiftinterface) %s -alias-module-names-in-module-interface -module-name Library
// RUN: %target-swift-typecheck-module-from-interface(%t/Library.swiftinterface) -module-name Library
// RUN: %FileCheck %s < %t/Library.swiftinterface

// REQUIRES: objc_interop

// CHECK: import Foundation
import Foundation

public class C: NSObject {
  // CHECK: @objc override dynamic public func observeValue(forKeyPath keyPath: Swift.String?, of object: Any?, change: [Foundation.NSKeyValueChangeKey : Any]?, context: Swift.UnsafeMutableRawPointer?)
  public override func observeValue(
    forKeyPath keyPath: String?,
    of object: Any?,
    change: [NSKeyValueChangeKey : Any]?,
    context: UnsafeMutableRawPointer?
  ){}
}
