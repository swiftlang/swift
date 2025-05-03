// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %S/Inputs/opaque_result_type_retroactive_other.swift -emit-module -emit-module-path %t/opaque_result_type_retroactive_other.swiftmodule
// RUN: %target-swift-emit-silgen %s -I %t | %FileCheck %s

import opaque_result_type_retroactive_other

extension G: @retroactive P where T: P {
  public func a() -> T {
    fatalError()
  }
}

public struct S: P {
  public func a() -> S {
    return self
  }
}

public func f() -> some P {
  return S()
}

public struct OfP<T: P> {
  public init(_: T) {}
}

// CHECK-LABEL: sil_global @$s30opaque_result_type_retroactive1xAA3OfPVy0a1_b1_c1_D6_other1GVyAA1fQryFQOyQo_GAjE1PAAxAeKHD1_AIHO_HCg_Gvp
public var x = OfP(G(f()))

// CHECK-LABEL: sil_global @$s30opaque_result_type_retroactive1yAA3OfPVy0a1_b1_c1_D6_other1GVyAA1fQryFQOyQo_1AAE1PPQxGAnekAxAeKHD1_AJQzAeKHA1_AMHO_HCg_Gvp
public var y = OfP(G(f().a()))
