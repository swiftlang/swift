public func something(_ obj: Int) -> Int { return obj }
public func something(_ a: Int, _ b: Int) -> (Int, Int) { return (a, b) }
public func something(_ a: Int, _ b: Int, _ c: Int) -> (Int, Int, Int) { return (a, b, c) }

public func ambiguousWithVar(_ _: Int) {}
public func scopedVar(_ _: Int) {}
public func localVar(_ _: Int) {}
public func scopedFunction(_ value: Int) -> Int { return value }

public func TypeNameWins(_ value: Int) -> Int { return value }
