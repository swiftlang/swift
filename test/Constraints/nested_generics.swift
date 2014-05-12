// RUN: %swift -parse -verify %s

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
GInt(x: GChar())

GInt().foo(GChar())
GInt().bar(0)

GInt.static_foo(GChar())
GInt.static_bar(0)

// <rdar://problem/12895793>
struct AnyStream<T : Sequence> {
  struct StreamRange<S : Generator> {
    var index : Int
    var elements : S

    // Conform to the Generator protocol.
    typealias Element = (Int, S.Element)
    mutating
    func next() -> Element? {
      var result = (index, elements.next())
      if !result.1 { return .None }
      ++index
      return (result.0, result.1!)
    }
  }

  var input : T

  // Conform to the enumerable protocol.
  typealias Elements = StreamRange<T.GeneratorType>
  func getElements() -> Elements {
    return Elements(index: 0, elements: input.generate())
  }
}

func enumerate<T : Sequence>(arg: T) -> AnyStream<T> {
  return AnyStream<T>(input: arg)
}
