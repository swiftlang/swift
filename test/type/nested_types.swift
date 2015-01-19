// RUN: %target-parse-verify-swift

struct X {
  typealias MyInt = Int
  func getInt() -> MyInt { return 7 }
}

extension X {
  typealias MyReal = Double
  func getFloat() -> MyReal { return 3.14 }
}

struct GeneratorTypeGeneratorType<G : GeneratorType> {
  var index : Int
  var elements : G
}

struct Generator<T : SequenceType> {
  var input : T

  typealias Generator = GeneratorTypeGeneratorType<T.Generator>
  func generate() -> Generator {
    return Generator(index: 0, elements: input.generate())
  }
}
