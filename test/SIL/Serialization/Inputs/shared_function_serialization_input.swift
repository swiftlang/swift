
@public struct X {}

@public func the_thing<T>(#t : T) { }

@public func the_thing_it_does(#x : X) {
  the_thing(t: x)
}
