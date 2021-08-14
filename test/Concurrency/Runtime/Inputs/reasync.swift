@_transparent
public func and(_ lhs: Bool, _ rhs: @autoclosure () async -> Bool) reasync -> Bool {
  guard lhs else { return false }
  return await rhs()
}

@_transparent
public func andThrows(_ lhs: Bool, _ rhs: @autoclosure () async throws -> Bool) reasync rethrows -> Bool {
  guard lhs else { return false }
  return try await rhs()
}
