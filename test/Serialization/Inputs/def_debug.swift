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

//Copied from opaque_result_alwaysInlineIntoClient.swift
public protocol P {
}

extension Int : P {
}

extension Double : P {
}
@_alwaysEmitIntoClient
public func testInlineWithOpaque() -> some P {
  if #available(macOS 9.0, *) {
    return 1
  }
  return 2.0
}
