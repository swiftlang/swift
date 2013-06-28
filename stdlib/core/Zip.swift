struct ZipEnumerator2<E0 : Enumerator, E1 : Enumerator> : Enumerator
{
  typealias Element = (E0.Element,E1.Element)

  constructor(e0: E0, e1: E1) {
    baseEnumerators.value = (e0,e1)
  }

  func isEmpty() -> Bool {
    if baseEnumerators.value.0.isEmpty() {
      return true
    }
    return baseEnumerators.value.1.isEmpty()
  }

  func next() -> Element {
    return (
      baseEnumerators.value.0.next(), 
      baseEnumerators.value.1.next())
  }

  var baseEnumerators : GenericIVar<(E0,E1)>
}

struct Zip2<S0: Enumerable, S1: Enumerable> : Enumerable
{
  typealias Enumerator1 = S0.EnumeratorType
  typealias Enumerator2 = S1.EnumeratorType
  typealias EnumeratorType = ZipEnumerator2<Enumerator1, Enumerator2>

  constructor(s0: S0, s1: S1) {
    sequences.value = (s0,s1)
  }

  func getEnumeratorType() -> EnumeratorType {
    return EnumeratorType(
      sequences.value.0.getEnumeratorType(), 
      sequences.value.1.getEnumeratorType())
  }

  var sequences: GenericIVar<(S0,S1)>
}
