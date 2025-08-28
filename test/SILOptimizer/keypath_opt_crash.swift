// RUN: %target-swift-frontend -O -emit-sil %s | %FileCheck %s

// REQUIRES: objc_interop

// Check if the optimizer does not crash.

import Foundation

@dynamicMemberLookup
public struct S {
  private let x: NSXPCConnection

  public subscript<T>(dynamicMember property: ReferenceWritableKeyPath<NSXPCConnection, T>) -> T {
    get {
      x[keyPath: property]
    }
    nonmutating set {
      x[keyPath: property] = newValue
    }
  }

}

// CHECK: sil {{.*}}test_set
public func test_set(s: S) {
  s.invalidationHandler = {}
}

// CHECK: sil {{.*}}test_get
public func test_get(s: S) -> (() -> ())? {
  return s.invalidationHandler
}


