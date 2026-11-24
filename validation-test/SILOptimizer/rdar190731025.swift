// RUN: %target-swift-frontend -O -emit-ir %s -o /dev/null
// RUN: %target-swift-frontend -O -emit-sil -sil-verify-all %s -o /dev/null

// rdar://190731025

struct E: Error {}

@inline(never)
func get<U>(_ i: Int) throws -> U {
  if i < 0 { throw E() }
  return unsafeBitCast(i != 0, to: U.self)
}

@inline(never)
func build<each T>(_ n: Int) throws -> (repeat each T)? {
  try (repeat get(n) as each T)
}

public func test(_ n: Int) throws -> Bool? { try build(n) }
