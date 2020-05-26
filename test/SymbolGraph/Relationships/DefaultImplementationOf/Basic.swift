// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name DefaultImplementationOf -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name DefaultImplementationOf -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/DefaultImplementationOf.symbols.json

public protocol P {
  var x: Int { get }
}

extension P {
  public var x: Int {
    return 2
  }
}  

// CHECK: "kind": "defaultImplementationOf",
// CHECK-NEXT: "source": "s:23DefaultImplementationOf1PPAAE1xSivp",
// CHECK-NEXT: "target": "s:23DefaultImplementationOf1PP1xSivp"

// Since x is a default implementation of a requirement, we don't consider this to be a "member".
// CHECK-NOT: memberOf
