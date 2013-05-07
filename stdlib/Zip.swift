struct ZipEnumerator2<E0 : Enumerator, E1 : Enumerator> : Enumerator
{
  typealias Element = (E0.Element,E1.Element)

  constructor(e0: E0, e1: E1) {
    baseEnumerator0.value = e0
    baseEnumerator1.value = e1
  }

  func isEmpty() -> Bool {
    if baseEnumerator0.value.isEmpty() {
      return true
    }
    return baseEnumerator1.value.isEmpty()
  }

  func next() -> Element {
    var b = (baseEnumerator0.value, baseEnumerator1.value)
    var ret = (b.0.next(), b.1.next())
    baseEnumerator0.value = b.0
    baseEnumerator1.value = b.1
    return ret
  }

  // WORKAROUND: we'd store these in a tuple if that didn't crash.
  // See <rdar://problem/13822463> Out of bounds access
  var baseEnumerator0 : GenericIVar<E0>
  var baseEnumerator1 : GenericIVar<E1>
}

struct Zip2<S0: Enumerable, S1: Enumerable> : Enumerable
{
  typealias Enumerator1 = S0.EnumeratorType
  typealias Enumerator2 = S1.EnumeratorType
  typealias EnumeratorType = ZipEnumerator2<Enumerator1, Enumerator2>

  constructor(s0: S0, s1: S1) {
    sequence0.value = s0
    sequence1.value = s1
  }

  func getEnumeratorType() -> EnumeratorType {
    return EnumeratorType(
      sequence0.value.getEnumeratorType(), 
      sequence1.value.getEnumeratorType())
  }

  // WORKAROUND: we'd store these in a tuple if that didn't crash.
  // See <rdar://problem/13822463> Out of bounds access
  var sequence0: GenericIVar<S0>
  var sequence1: GenericIVar<S1>
}
