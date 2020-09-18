import Swift

public protocol GeneratorProtocol {
  associatedtype Element
    
  mutating func next() async -> Element?
}

public protocol AsyncSequence {
  associatedtype Generator: GeneratorProtocol
  func makeGenerator() -> Generator
}
