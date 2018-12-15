// RUN: %target-typecheck-verify-swift

protocol P : class {}

class F<T> {
  func wait() throws -> T { fatalError() }
}

func bar(_ a: F<P>, _ b: F<P>) throws {
  _ = (try  a.wait()) === (try  b.wait()) // Ok
  _ = (try? a.wait()) === (try? b.wait()) // Ok
}
