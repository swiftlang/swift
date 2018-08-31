@_fixed_layout
public struct X {
  @inlinable
  public init() { }
}

@inlinable
@inline(never)
public func the_thing<T>(t t : T) { }

@inlinable
@inline(never)
public func the_thing_it_does(x x : X) {
  the_thing(t: x)
}
