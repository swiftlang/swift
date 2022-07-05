public protocol MySequence<Element> {
  associatedtype Element
}

public struct MySequenceHolder<Element> {
  public var seq: any MySequence<Element>

  public init(seq: any MySequence<Element>) {
    self.seq = seq
  }
}
