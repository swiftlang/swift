public func baz() {}

public protocol BazProtocol {
  associatedtype Ret
  func method1() -> Ret?
}

public enum Baz<T: Collection> : BazProtocol {
  case collection(T)
  case other

  public func method1() -> T.SubSequence? {
    switch self {
    case .collection(let collection):
      return makeX(collection)
    default:
      return nil
    }
  }
}
