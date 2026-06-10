// RUN: %target-swift-emit-silgen %s -verify

public func withThrowingClosure<Result>(
  _ body: () throws -> Result
) rethrows -> Result {
  return try body()
}

func witNestedThrowingClosure<E, Result>(
  _ body: () throws(E) -> Result
) throws(E) -> Result where E: Error {
  do {
    return try withThrowingClosure { () throws(E) in
      try body()
    }
  } catch let error as E {
    throw error
  } catch {
    fatalError()
  }
}
