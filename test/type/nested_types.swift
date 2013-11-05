// RUN: %swift -parse %s -verify

struct X {
  typealias MyInt = Int
  def getInt() -> MyInt { return 7 }
}

extension X {
  typealias MyReal = Double
  def getFloat() -> MyReal { return 3.14 }
}

struct GeneratorTypeGeneratorType<S : Generator> {
  var index : Int
  var elements : S
}

struct GeneratorType<T : Enumerable> {
  var input : T

  typealias GeneratorType = GeneratorTypeGeneratorType<T.GeneratorType>
  def enumerate() -> GeneratorType {
    return GeneratorType(0, input.enumerate())
  }
}
