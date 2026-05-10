// Large structure passed indirectly.
public struct StructSeveralI64 {
    var x1, x2, x3, x4, x5: Int64
}

public func passThroughStructSeveralI64(i: Int64, _ x: StructSeveralI64, j: Float) -> StructSeveralI64 {
    return StructSeveralI64(x1: x.x1, x2: x.x2 + i, x3: x.x3, x4: -x.x4, x5: x.x5 + Int64(j))
}

public func inoutStructSeveralI64(_ s: inout StructSeveralI64) {
    s.x1 = -1
    s.x2 = -2
    s.x3 = -3
    s.x4 = -4
    s.x5 = -5
}

public struct SmallStructDirectPassing {
    var x1, x2: Int16
}
