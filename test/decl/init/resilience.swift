// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-resilience -emit-module-path=%t/resilient_struct.swiftmodule -module-name=resilient_struct %S/../../Inputs/resilient_struct.swift
// RUN: %target-swift-frontend -emit-module -enable-resilience -emit-module-path=%t/resilient_protocol.swiftmodule -module-name=resilient_protocol %S/../../Inputs/resilient_protocol.swift
// RUN: %target-swift-frontend -typecheck -verify -enable-resilience -I %t %s

import resilient_struct
import resilient_protocol

// Size is not @_fixed_layout, so we cannot define a new designated initializer
extension Size {
  init(ww: Int, hh: Int) {
    self.w = ww
    self.h = hh // expected-error {{'let' property 'h' may not be initialized directly; use "self.init(...)" or "self = ..." instead}}
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
public struct Animal {
  public let name: String // expected-note 3 {{declared here}}

  @_inlineable public init(name: String) {
    self.name = name // expected-error {{'let' property 'name' may not be initialized directly; use "self.init(...)" or "self = ..." instead}}
  }

  @inline(__always) public init(dog: String) {
    self.name = dog // expected-error {{'let' property 'name' may not be initialized directly; use "self.init(...)" or "self = ..." instead}}
  }

  @_transparent public init(cat: String) {
    self.name = cat // expected-error {{'let' property 'name' may not be initialized directly; use "self.init(...)" or "self = ..." instead}}
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
  
  public init(nonInlinableName name: String) {
    self.name = name
  }

  @_inlineable public init(name: String) {
    // expected-error@-1 {{initializer for class 'Widget' is '@_inlineable' and must delegate to another initializer}}
    self.name = name
  }

  @_inlineable public convenience init(goodName name: String) {
    // This is OK
    self.init(nonInlinableName: name)
  }
}

public protocol Gadget {
  init()
}

extension Gadget {
  @_inlineable public init(unused: Int) {
    // This is OK
    self.init()
  }
}

// Protocol extension initializers are OK too
extension OtherResilientProtocol {
  public init(other: Self) {
    self = other
  }
}
