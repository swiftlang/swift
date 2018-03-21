// RUN: %target-swift-frontend -typecheck -swift-version 4 -verify -enable-resilience %s -DRESILIENT
// RUN: %target-swift-frontend -typecheck -swift-version 5 -verify -enable-resilience %s -DRESILIENT

// There should be no errors when run without resilience enabled.
// RUN: %target-swift-frontend -typecheck -swift-version 4 %s
// RUN: %target-swift-frontend -typecheck -swift-version 5 %s

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
