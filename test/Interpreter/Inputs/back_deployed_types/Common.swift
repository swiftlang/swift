// An ordinary module whose generic metadata is keyed on witness tables
// supplied by its callers.
public struct Needs<T: Equatable> {}

public func needsType<T: Equatable>(_: T.Type) -> Any.Type { Needs<T>.self }

public func needsTypeDynamic(_ value: Any) -> Any.Type? {
  guard let e = value as? any Equatable else { return nil }
  func open<T: Equatable>(_: T) -> Any.Type { Needs<T>.self }
  return open(e)
}

// A protocol with generic requirements, along with generic metadata keyed on
// the witness tables for it.
public protocol Transformer {
  associatedtype Element
  func transform<U>(_ f: (Element) -> U) -> [U]
  func count<S: Sequence>(matching s: S) -> Int where S.Element == Element
  static func make<C: Collection>(from c: C) -> Self where C.Element == Element
}

public struct NeedsTransformer<X: Transformer> {}

public func needsTransformer<X: Transformer>(_: X.Type) -> Any.Type {
  NeedsTransformer<X>.self
}

public func describe<X: Transformer>(_ x: X) -> [String] {
  x.transform { "\($0)" }
}

public func countMatching<X: Transformer, S: Sequence>(
    _ x: X, _ s: S
) -> Int where S.Element == X.Element {
  x.count(matching: s)
}

public func make<X: Transformer, C: Collection>(
    _: X.Type, from c: C
) -> X where C.Element == X.Element {
  X.make(from: c)
}

public func describeDynamic(_ value: Any) -> [String]? {
  guard let t = value as? any Transformer else { return nil }
  return describe(t)
}

public func needsTransformerDynamic(_ value: Any) -> Any.Type? {
  guard let t = value as? any Transformer else { return nil }
  func open<X: Transformer>(_: X) -> Any.Type { NeedsTransformer<X>.self }
  return open(t)
}
