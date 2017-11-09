// RUN: %target-swift-frontend -O -emit-sil %s | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

protocol FooProtocol : class {
}

class FooProtocolImplementation : NSObject, FooProtocol {
}

// CHECK-LABEL: sil @{{.*}}testit
// CHECK: checked_cast_addr_br {{.*}} AnyHashable {{.*}} to FooProtocol 
public func testit(_ x: AnyHashable) -> Bool {
  return (x as? FooProtocol) != nil
}

