public struct InnerStruct {
  let field: Int = 0

  public init() {}
}

public enum InnerEnum {
  case field(Int)
}

public struct OuterStruct {
  let first: InnerStruct = InnerStruct()
  let second: InnerEnum = InnerEnum.field(0)

  public init() {}
}

public final class Burger {
  public let onions: Bool = true
  public let complex: OuterStruct = OuterStruct()
  public let cheeseSlices: Int = 0
}

@_fixed_layout
public final class Burrito {
  public let filling: OuterStruct = OuterStruct()
  public let cilantro: Int = 0
}
