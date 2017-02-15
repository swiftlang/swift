// RUN: rm -rf %t && mkdir %t
// RUN: %target-swift-frontend -emit-module -enable-resilience -emit-module-path=%t/resilient_struct.swiftmodule -module-name=resilient_struct %S/../../Inputs/resilient_struct.swift
// RUN: %target-swift-frontend -emit-module -enable-resilience -emit-module-path=%t/resilient_protocol.swiftmodule -module-name=resilient_protocol %S/../../Inputs/resilient_protocol.swift
// RUN: %target-swift-frontend -typecheck -verify -enable-resilience -I %t %s

import resilient_struct
import resilient_protocol

// Point is @_fixed_layout -- this is OK
extension Point {
  init(xx: Int, yy: Int) {
    self.x = xx
    self.y = yy
  }
}

// Size is not @_fixed_layout, so we cannot define a new designated initializer
extension Size {
  init(ww: Int, hh: Int) {
  // expected-error@-1 {{initializer declared in an extension of non-'@_fixed_layout' type 'Size' must delegate to another initializer}}
    self.w = ww
    self.h = hh
  }

  // This is OK
  init(www: Int, hhh: Int) {
    self.init(w: www, h: hhh)
  }

  // FIXME: This should be allowed, but Sema doesn't distinguish this
  // case from memberwise initialization, and DI explodes the value type
  init(other: Size) {
  // expected-error@-1 {{initializer declared in an extension of non-'@_fixed_layout' type 'Size' must delegate to another initializer}}
    self = other
  }
}

// Animal is not @_fixed_layout, so we cannot define an @_inlineable
// designated initializer
public struct Animal {
  public let name: String

  @_inlineable public init(name: String) {
  // expected-error@-1 {{initializer for non-'@_fixed_layout' type 'Animal' is '@_inlineable' and must delegate to another initializer}}
    self.name = name
  }

  @inline(__always) public init(dog: String) {
  // expected-error@-1 {{initializer for non-'@_fixed_layout' type 'Animal' is '@inline(__always)' and must delegate to another initializer}}
    self.name = dog
  }

  @_transparent public init(cat: String) {
  // expected-error@-1 {{initializer for non-'@_fixed_layout' type 'Animal' is '@_transparent' and must delegate to another initializer}}
    self.name = cat
  }

  // This is OK
  @_inlineable public init(cow: String) {
    self.init(name: cow)
  }

  // FIXME: This should be allowed, but Sema doesn't distinguish this
  // case from memberwise initialization, and DI explodes the value type
  @_inlineable public init(other: Animal) {
  // expected-error@-1 {{initializer for non-'@_fixed_layout' type 'Animal' is '@_inlineable' and must delegate to another initializer}}
    self = other
  }
}

public class Widget {
  public let name: String

  @_inlineable public init(name: String) {
    // expected-error@-1 {{initializer for class 'Widget' is '@_inlineable' and must delegate to another initializer}}
    self.name = name
  }
}

// Protocol extension initializers are OK too
extension OtherResilientProtocol {
  public init(other: Self) {
    self = other
  }
}
