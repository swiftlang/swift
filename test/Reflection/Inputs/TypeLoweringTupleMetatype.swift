public final class C {}

public struct HasTupleMetatype {
  public var tuple: (C, Int).Type
  public var x: Int
}

public struct HasGenericTupleMetatype<T> {
  public var tuple: (T, Int).Type
  public var x: Int
}
