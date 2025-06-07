public var x = Int64()

public func use<T>(_ x: T) -> T { return x }

public func noinline(_ x: Int64) -> Int64 { return use(x) }
