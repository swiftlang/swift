// Large enum passed indirectly.
public enum LargeEnum {
    case A(x1: Int64, x2: Int64, x3: Int64, x4: Int64, x5: Int64)
    case B
}

public func passThroughLargeEnum(_ x: LargeEnum) -> LargeEnum {
    return x
}
