// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-module -enable-library-evolution -emit-module-path=%t/resilient_struct.swiftmodule -module-name=resilient_struct %S/../../Inputs/resilient_struct.swift
// RUN: %target-swift-frontend -emit-module -enable-library-evolution -emit-module-path=%t/resilient_protocol.swiftmodule -module-name=resilient_protocol %S/../../Inputs/resilient_protocol.swift

// RUN: %target-swift-frontend -typecheck -swift-version 4 -verify -I %t %s
// RUN: %target-swift-frontend -typecheck -swift-version 4 -verify -enable-library-evolution -I %t %s

// RUN: %target-swift-frontend -typecheck -swift-version 5 -verify -I %t %s
// RUN: %target-swift-frontend -typecheck -swift-version 5 -verify -enable-library-evolution -I %t %s

import resilient_struct
import resilient_protocol

// Size is not @frozen, so we cannot define a new designated initializer
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

// Protocol extension initializers are OK too
extension OtherResilientProtocol {
  public init(other: Self) {
    self = other
  }
}
