public struct Box<T> {
  public var t: T
}

public final class Klass<T> {}

public struct HasSubstitutedMetatypes<T> {
  public var box: Box<T>.Type
  public var array: [T].Type
  public var param: T.Type
  public var klass: Klass<T>.Type
  public var x: Int
}
