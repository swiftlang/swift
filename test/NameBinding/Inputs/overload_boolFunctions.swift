public func something(_ value: Bool) -> Bool { return value }

public func ambiguousWithVar(_ _: Bool) {}

public func scopedFunction(_ value: Bool) -> Bool { return value }

public struct TypeNameWins {}
public struct localVar {}
