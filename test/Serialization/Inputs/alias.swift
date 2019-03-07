public typealias MyInt64 = Int64
public typealias AnotherInt64 = (MyInt64)

public typealias TwoInts = (MyInt64, Int64)
public typealias ThreeNamedInts = (a : MyInt64, b : MyInt64, c : MyInt64)
public typealias None = ()

public typealias NullFunction = () -> ()
public typealias IntFunction = (MyInt64) -> MyInt64
public typealias TwoIntFunction = (TwoInts) -> MyInt64

public struct AliasWrapper {
  public typealias Boolean = Bool
}

public extension Int {
  public typealias EspeciallyMagicalInt = Int64
}

public typealias IntSlice = [Int]


public struct Base {
  public func foo() -> BaseAlias {
    return self
  }
}
public typealias BaseAlias = Base

public protocol ProtoWrapper {}
extension ProtoWrapper {
  public typealias Boolean = Bool
}

public struct Outer { public typealias G<T> = T }
public typealias GG = Outer.G

public typealias GInt = Outer.G<Int>

public struct UnboundStruct<T> {}
public typealias UnboundAlias<T: Comparable> = UnboundStruct<T>
