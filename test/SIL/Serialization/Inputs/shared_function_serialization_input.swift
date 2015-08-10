
public struct X {
  public init() { }
}

@inline(never)
public func the_thing<T>(t t : T) { }

@inline(never)
public func the_thing_it_does(x x : X) {
  the_thing(t: x)
}
