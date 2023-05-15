
public struct IntTuple {
    let values: (Int64, Int64, Int64, Int64, Int64, Int64)
}

// Large enum passed indirectly.
public enum LargeEnum {
    case A(IntTuple)
    case B
}

public func passThroughLargeEnum(_ x: LargeEnum) -> LargeEnum {
    return x
}
