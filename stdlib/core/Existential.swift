//////////////////////////////////////////
// FIXME: Workaround for inability to create existentials of protocols
// with associated types <rdar://problem/11689181> and for the
// inability to constrain nested generics based on the containing type
// <rdar://problem/11700999>
//

// This file contains "existentials" for the protocols defined in
// Policy.swift.  Similar components should usually be defined next to
// their respective protocols.
struct GeneratorOf<T> : Generator {
  def next() -> T? {
    return _next()
  }
  var _next : ()->T?
}

def existential<G: Generator>(base: G)
  -> GeneratorOf<G.Element>
{
  return GeneratorOf( { base.next() } )
}

struct EnumerableOf<T> : Enumerable {
  def enumerate() -> GeneratorOf<T> {
    return _enumerate()
  }
  var _enumerate : ()->GeneratorOf<T>
}

def existential<E: Enumerable>(base: E)
  -> EnumerableOf<E.GeneratorType.Element>
{
  return EnumerableOf<E.GeneratorType.Element>(
    { existential(base.enumerate()) }
  )
}

struct SinkOf<T> : Sink {
  def put(x: T) {
    _put(x)
  }
  var _put : (T)->()
}

def existential<S: Sink>(base: S) -> SinkOf<S.Element> {
  return SinkOf( { base.put($0) } )
}
