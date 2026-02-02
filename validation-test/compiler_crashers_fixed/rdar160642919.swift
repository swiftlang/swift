// RUN: %target-swift-frontend -emit-ir %s -disable-availability-checking

public protocol IntoTuple {
    func asTuple<each E>() -> (repeat each E)
    func asTuples<each E>() -> (repeat each E)
}

extension InlineArray: IntoTuple {
    public func asTuple<each E>() -> (repeat each E)
    {
        var indices = self.indices.makeIterator()
        return (repeat self[indices.next()!] as! each E)
    }

    // Convert nested InlineArrays
    public func asTuples<each E>() -> (repeat each E)
        where Element: IntoTuple
    {
        var indices = self.indices.makeIterator()
        return (repeat self[indices.next()!].asTuple() as each E)
    }

    public func asTuples<each E>() -> (repeat each E)
    {
        return self.asTuple()
    }
}

let x: ((Float, Float), (Float, Float)) = [2 of [2 of _]](repeating: [1.0, 2.0]).asTuples()
print(x)

