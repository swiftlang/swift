// RUN: %empty-directory(%t)
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
  // FIXME: Produce a decent diagnostic here
  init(ww: Int, hh: Int) {
    self.w = ww
    self.h = hh // expected-error {{cannot assign to property: 'h' is a 'let' constant}}
  }

  // This is OK
  init(www: Int, hhh: Int) {
    self.init(w: www, h: hhh)
  }

  // This is OK
  init(other: Size) {
    self = other
  }
}

// Animal is not @_fixed_layout, so we cannot define an @_inlineable
// designated initializer
//
// FIXME: Crap diagnostics
public struct Animal {
  public let name: String // expected-note 3{{change 'let' to 'var' to make it mutable}}

  @_inlineable public init(name: String) {
    self.name = name // expected-error {{cannot assign to property: 'name' is a 'let' constant}}
  }

  @inline(__always) public init(dog: String) {
    self.name = dog // expected-error {{cannot assign to property: 'name' is a 'let' constant}}
  }

  @_transparent public init(cat: String) {
    self.name = cat // expected-error {{cannot assign to property: 'name' is a 'let' constant}}
  }

  // This is OK
  @_inlineable public init(cow: String) {
    self.init(name: cow)
  }

  // This is OK
  @_inlineable public init(other: Animal) {
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
