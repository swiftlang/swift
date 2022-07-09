import RoundTrip

typealias Alias = Int

struct Outer {
  typealias Alias = Int

  struct Inner {
    typealias Alias = Int
  }
}

struct GenericOuter<T> {
  typealias Alias = Int

  struct Inner {
    typealias Alias = Int
  }
}

protocol Proto {
  typealias Alias = Int
}

extension Proto {
  typealias OtherAlias = Int
}

extension GenericOuter where T : Proto {
  typealias ConditionalAlias = Int
}

struct Conforms : Proto {}

public func test() {
    roundTripType(Alias.self)
    roundTripType(Outer.Alias.self)
    roundTripType(Outer.Inner.Alias.self)
    roundTripType(GenericOuter<Int>.Alias.self)
    roundTripType(GenericOuter<Int>.Inner.Alias.self)
    roundTripType(Proto.Alias.self)
    roundTripType(Proto.OtherAlias.self)
    roundTripType(GenericOuter<Conforms>.ConditionalAlias.self)
}
