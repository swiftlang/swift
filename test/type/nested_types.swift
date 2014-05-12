// RUN: %swift -parse %s -verify

struct X {
  typealias MyInt = Int
  func getInt() -> MyInt { return 7 }
}

extension X {
  typealias MyReal = Double
  func getFloat() -> MyReal { return 3.14 }
}

struct GeneratorTypeGeneratorType<G : Generator> {
  var index : Int
  var elements : G
}

struct GeneratorType<T : Sequence> {
  var input : T

  typealias GeneratorType = GeneratorTypeGeneratorType<T.GeneratorType>
  func generate() -> GeneratorType {
    return GeneratorType(index: 0, elements: input.generate())
  }
}
