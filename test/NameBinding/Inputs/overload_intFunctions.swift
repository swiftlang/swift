public func something(obj: Int) -> Int { return obj }
public func something(a: Int, b: Int) -> (Int, Int) { return (a, b) }
public func something(a: Int, b: Int, c: Int) -> (Int, Int, Int) { return (a, b, c) }

public func ambiguousWithVar(_: Int) {}
public func scopedVar(_: Int) {}
public func localVar(_: Int) {}
public func scopedFunction(value: Int) -> Int { return value }

public func TypeNameWins(value: Int) -> Int { return value }
