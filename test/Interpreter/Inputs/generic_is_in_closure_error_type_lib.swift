public func run(_ body: () throws -> Void, _ matches: (any Error) -> Bool) -> Bool {
  do { try body(); return false } catch { return matches(error) }
}

public func check<E: Error>(_ t: E.Type, _ body: () throws -> Void) -> Bool {
  run(body, { $0 is E })
}
