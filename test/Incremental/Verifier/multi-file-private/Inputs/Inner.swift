// expected-provides{{Inner}}
// expected-member{{main.Inner.init}}
// expected-member{{main.Inner.deinit}}
public struct Inner {}

// expected-provides{{Foo}}
public typealias Foo = () -> (Inner)

// expected-provides{{blah}}
public func blah(foo: Foo) {}

// expected-provides{{defaultFoo}}
public var defaultFoo: Foo = {
  return Inner()
}
