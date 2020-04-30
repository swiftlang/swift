public struct First<T> {
  public let value: Int32
}

public struct Second {
  public let value: Int64
}

@frozen
public enum FixedPayloadSize<T> {
  case first(First<T>)
  case second(Second)
}

public enum DynamicPayloadSize<T> {
  case first(First<T>)
  case second(Second)
}
