import Swift

public protocol GeneratorProtocol {
  associatedtype Element
    
  mutating func next() async -> Element?
}

public protocol AsyncSequence {
  associatedtype Element
  associatedtype Generator: GeneratorProtocol where Generator.Element == Element

  func makeGenerator() -> Generator
}