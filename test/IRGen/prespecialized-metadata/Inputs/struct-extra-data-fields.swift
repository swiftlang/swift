public struct FixedFieldOffsets<Tag> {
  public let first: Int64
  public let second: Int64
}

public struct DynamicFieldOffsets<Value> {
  public let first: Value
  public let second: Value
}
