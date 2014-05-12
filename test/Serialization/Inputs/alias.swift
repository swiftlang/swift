typealias MyInt64 = Int64
typealias AnotherInt64 = (MyInt64)

typealias TwoInts = (MyInt64, Int64)
typealias ThreeNamedInts = (a : MyInt64, b : MyInt64, c : MyInt64)
typealias None = ()

typealias NullFunction = () -> ()
typealias IntFunction = (MyInt64) -> MyInt64
typealias TwoIntFunction = (TwoInts) -> MyInt64

struct AliasWrapper {
  typealias Boolean = Bool
}

extension Int {
  typealias EspeciallyMagicalInt = Int64
}

typealias IntSlice = Int[]


struct Base {
  func foo() -> BaseAlias {
    return self
  }
}
typealias BaseAlias = Base
