// RUN: %target-swift-frontend -typecheck -swift-version 4 -verify -enable-resilience %s -DRESILIENT
// RUN: %target-swift-frontend -typecheck -swift-version 5 -verify -enable-resilience %s -DRESILIENT

// There should be no errors when run without resilience enabled.
// RUN: %target-swift-frontend -typecheck -swift-version 4 %s
// RUN: %target-swift-frontend -typecheck -swift-version 5 %s

// Animal is not @_fixed_layout, so we cannot define an @inlinable
// designated initializer
public struct Animal {
  public let name: String // expected-note 3 {{declared here}}

  @inlinable public init(name: String) {
    self.name = name // expected-error {{'let' property 'name' may not be initialized directly; use "self.init(...)" or "self = ..." instead}}
  }

  @inline(__always) public init(dog: String) {
    self.name = dog // expected-error {{'let' property 'name' may not be initialized directly; use "self.init(...)" or "self = ..." instead}}
  }

  @_transparent public init(cat: String) {
    self.name = cat // expected-error {{'let' property 'name' may not be initialized directly; use "self.init(...)" or "self = ..." instead}}
  }

  // This is OK
  @inlinable public init(cow: String) {
    self.init(name: cow)
  }

  // This is OK
  @inlinable public init(other: Animal) {
    self = other
  }
}

public class Widget {
  public let name: String
  
  public init(nonInlinableName name: String) {
    self.name = name
  }

  @inlinable public init(name: String) {
    // expected-error@-1 {{initializer for class 'Widget' is '@inlinable' and must delegate to another initializer}}
    self.name = name
  }

  @inlinable public convenience init(goodName name: String) {
    // This is OK
    self.init(nonInlinableName: name)
  }
}

public protocol Gadget {
  init()
}

extension Gadget {
  @inlinable public init(unused: Int) {
    // This is OK
    self.init()
  }
}
