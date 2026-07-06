// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -target %module-target-future -o %t/anyobject-cast-crash
// RUN: %target-codesign %t/anyobject-cast-crash
// RUN: %target-run %t/anyobject-cast-crash
// REQUIRES: executable_test
// REQUIRES: objc_interop

// Test that it doesn't crash

import Foundation

enum Unconstrained<P> {
  static var `protocol`: Protocol? {
    P.self as AnyObject as? Protocol
  }
}

enum Constrained<P: AnyObject> {
  static var `protocol`: Protocol? {
    P.self as AnyObject as? Protocol
  }
}

@objc protocol TestProtocol {

}

print(Unconstrained<TestProtocol>.protocol)
print(Constrained<TestProtocol>.protocol)
