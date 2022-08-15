// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -module-name Module -target %target-pre-stable-abi-triple
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name Module
// RUN: %FileCheck %s < %t.swiftinterface
// REQUIRES: objc_interop

import Foundation

// To infer @_staticInitializeObjCMetadata, the following needs to be true
// Our class needs to be:
// - A subclass of a generic Objective-C class
// - That inherits a conformance to a protocol
// - Declared in a module with a deployment target before the stable ABI

public class Super<T>: NSObject, NSCoding {
  required public init(coder: NSCoder) {}
  public func encode(with: NSCoder) {}
}

// CHECK-NOT: @_staticInitializeObjCMetadata
// CHECK: public class Sub : Module.Super<Swift.Int>
public class Sub: Super<Int> {
  required public init(coder: NSCoder) {}
  override public func encode(with: NSCoder) {}
}
