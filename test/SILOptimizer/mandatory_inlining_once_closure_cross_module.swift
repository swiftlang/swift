// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -enable-experimental-feature OnceFunctions -O -parse-as-library -emit-module %t/Lib.swift -module-name Lib -emit-module-path %t/Lib.swiftmodule
// RUN: %target-swift-frontend -enable-experimental-feature OnceFunctions -Onone -parse-as-library -I %t -emit-sil -sil-verify-all %t/Client.swift -module-name Client | %FileCheck %s

// REQUIRES: swift_feature_OnceFunctions

// The library is optimized, so the closure parameter doesn't live in a stack
// slot, and MandatoryInlining inlines the call of the client's autoclosure.

//--- Lib.swift

public struct Resource: ~Copyable {
  public let value: Int
  public init(_ value: Int) { self.value = value }
  deinit {}
}

@inline(never)
public func consume(_ resource: consuming Resource) -> Bool {
  resource.value > 0
}

@inline(never)
public func borrow(_ resource: borrowing Resource) -> Bool {
  resource.value > 0
}

@_transparent
public func and(_ lhs: Bool, _ rhs: @autoclosure @callable(once) () throws -> Bool) rethrows -> Bool {
  lhs ? try rhs() : false
}

//--- Client.swift

import Lib

// CHECK-LABEL: sil @$s6Client12trivialValueySbSi_SitF :
// CHECK-NOT:     partial_apply
// CHECK:       } // end sil function '$s6Client12trivialValueySbSi_SitF'
public func trivialValue(_ a: Int, _ b: Int) -> Bool {
  and(a > 0, b > 0)
}

// CHECK-LABEL: sil @$s6Client13copyableValueySbSi_SStF :
// CHECK-NOT:     partial_apply
// CHECK:       } // end sil function '$s6Client13copyableValueySbSi_SStF'
public func copyableValue(_ a: Int, _ s: String) -> Bool {
  and(a > 0, s.isEmpty)
}

// CHECK-LABEL: sil @$s6Client13borrowedValueySbSi_SitF :
// CHECK-NOT:     partial_apply
// CHECK:       } // end sil function '$s6Client13borrowedValueySbSi_SitF'
public func borrowedValue(_ a: Int, _ x: Int) -> Bool {
  let resource = Resource(x)
  return and(a > 0, borrow(resource))
}

// The inlined call consumes the resource, and the path that drops the closure
// destroys the resource instead.
//
// CHECK-LABEL: sil @$s6Client13consumedValueySbSi_SitF :
// CHECK:         [[RESOURCE:%.*]] = load
// CHECK-NOT:     partial_apply
// CHECK:         cond_br {{%.*}}, [[CALL:bb[0-9]+]], [[DROP:bb[0-9]+]]
// CHECK:       [[CALL]]:
// CHECK:         apply {{%.*}}([[RESOURCE]])
// CHECK:       [[DROP]]:
// CHECK:         release_value [[RESOURCE]]
// CHECK:       } // end sil function '$s6Client13consumedValueySbSi_SitF'
public func consumedValue(_ a: Int, _ x: Int) -> Bool {
  let resource = Resource(x)
  return and(a > 0, consume(resource))
}

