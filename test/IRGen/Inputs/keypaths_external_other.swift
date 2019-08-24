
public struct G<T> {
  public var x: T { get { fatalError() } set { } }
  public subscript<U: Hashable>(x: U) -> T { get { fatalError() } set { } }
}
