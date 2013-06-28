/// \brief The once and future sequence traversal protocol
protocol Generator {
  typealias Element
  func next() -> Optional<Element>
}

/// \brief Adapt an old-style Enumerator into a Generator
struct EnumeratorAsGenerator<T: Enumerator> : Generator {
  typealias Element = T.Element
  constructor(x: T) { this.x.value = x }
  func next() -> Optional<Element> {
    if x.value.isEmpty() {
      return None
    }
    return Some(x.value.next())
  }
  var x : GenericIVar<T>
}

func asGenerator<T: Enumerator>(x: T) -> EnumeratorAsGenerator<T> {
  typealias Ret = EnumeratorAsGenerator<T>
  return Ret(x)
}

/// \brief Adapt a Generator into an old-style Enumerator
struct GeneratorAsEnumerator<T: Generator> : Enumerable, Enumerator {
  constructor(x: T) { this.x.value = x }

  // Satisfy Enumerable protocol
  typealias EnumeratorType = GeneratorAsEnumerator<T>
  func getEnumeratorType() -> EnumeratorType { return this }

  // Satisfy Enumerator protocol
  typealias Element = T.Element
  func isEmpty() -> Bool {
    if buffer.value.isNone() {
      buffer.value = Some(x.value.next())
    }
    return buffer.value.get() == None
  }
  func next() -> Element {
    if buffer.value == None {
      buffer.value = Some(x.value.next())
    }
    var ret = buffer.value.get().get()
    buffer.value = None
    return ret
  }
  var buffer: GenericIVar<Optional<Optional<Element>>>
  var x : GenericIVar<T>
}

func asEnumerator<T: Generator>(x: T) -> GeneratorAsEnumerator<T> {
  typealias Ret = GeneratorAsEnumerator<T>
  return Ret(x)
}

