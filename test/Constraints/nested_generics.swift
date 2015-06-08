// RUN: %target-parse-verify-swift

struct G<T> {
  init() {}
  init<U>(x:G<U>) { }

  func foo<U>(x: G<U>) { }

  func bar<U>(x: U) { }

  static func static_foo<U>(x: G<U>) { }
  static func static_bar<U>(x: U) { }
}

typealias GInt = G<Int>
typealias GChar = G<UnicodeScalar>
GInt(x: GChar()) // expected-warning{{unused}}

GInt().foo(GChar())
GInt().bar(0)

GInt.static_foo(GChar())
GInt.static_bar(0)

// <rdar://problem/12895793>
struct AnyStream<T : SequenceType> {
  struct StreamRange<S : GeneratorType> { // expected-error{{generic type 'StreamRange' nested}}
    var index : Int
    var elements : S

    // Conform to the GeneratorType protocol.
    typealias Element = (Int, S.Element)
    mutating
    func next() -> Element? {
      let result = (index, elements.next())
      if result.1 == nil { return .None }
      ++index
      return (result.0, result.1!)
    }
  }

  var input : T

  // Conform to the enumerable protocol.
  typealias Elements = StreamRange<T.Generator>
  func getElements() -> Elements {
    return Elements(index: 0, elements: input.generate())
  }
}

func enumerate<T : SequenceType>(arg: T) -> AnyStream<T> {
  return AnyStream<T>(input: arg)
}
