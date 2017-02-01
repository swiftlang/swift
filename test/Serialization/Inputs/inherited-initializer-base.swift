open class Base {
  public init(_ value: Int = 0) {}
}

public protocol Initializable {
  init()
}
open class GenericBase<T: Initializable> {
  public init(_ value: T = T()) {}
}
