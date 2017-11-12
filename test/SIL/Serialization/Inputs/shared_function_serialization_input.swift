
public struct X {
  @_inlineable
  public init() { }
}

@_inlineable
@inline(never)
public func the_thing<T>(t t : T) { }

@_inlineable
@inline(never)
public func the_thing_it_does(x x : X) {
  the_thing(t: x)
}
