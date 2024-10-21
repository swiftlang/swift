@inlinable @inline(__always)
public func foo(x: UInt64) -> UInt64 {
    if (x > 100) {
        return 100
    }
    return 1
}

@_transparent
public func specializedGenericInlined() -> Int {
    return id(1)
}

@_transparent
@inline(__always)
public func id<T: Equatable>(_ x: T) -> T{
    return x
}

@_alwaysEmitIntoClient
public func barGeneric<T: Numeric>(_ x: [T], sum: T) -> T {
    var temp = sum
    for i in x {
        temp += i
    }
    return temp
}
