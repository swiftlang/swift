// RUN: %target-swift-emit-silgen -enable-sil-ownership -verify %s
// RUN: %target-swift-emit-sil -enable-sil-ownership %s | %FileCheck --check-prefix=AFTER-MANDATORY-PASSES %s

// ensure escape analysis killed the box allocations used for delayed Self
// return buffers
// AFTER-MANDATORY-PASSES-NOT: alloc_box

extension Error {
  mutating func covariantReturn(_: Int) -> Self { return self }
  mutating func covariantOptionalReturn(_: Int) -> Self? { return self }
  mutating func covariantReturnOrThrow(_: Int) throws -> Self { return self }
  mutating func covariantClosureArgAndReturn(_: (Self) -> Int) -> Self { return self }
}

protocol MutatingWithCovariantReturn {
  mutating func covariantReturn(_: Int) -> Self
  mutating func covariantOptionalReturn(_: Int) -> Self?
  mutating func covariantReturnOrThrow(_: Int) throws -> Self
  mutating func covariantClosureArgAndReturn(_: (Self) -> Int) -> Self
}

protocol ClassConstrainedRefinement: MutatingWithCovariantReturn, AnyObject {}

func int() -> Int { return 0 }
func intThrows() throws -> Int { return 0 }

func foo(x: inout MutatingWithCovariantReturn, y: inout ClassConstrainedRefinement, z: inout Error) throws {
  _ = x.covariantReturn(int())
  _ = x.covariantReturn(try intThrows())
  _ = x.covariantReturn(try! intThrows())

  _ = x.covariantOptionalReturn(int())
  _ = x.covariantOptionalReturn(try intThrows())
  _ = x.covariantOptionalReturn(try! intThrows())

  _ = try x.covariantReturnOrThrow(int())
  _ = try x.covariantReturnOrThrow(try intThrows())
  _ = try x.covariantReturnOrThrow(try! intThrows())

  _ = x.covariantClosureArgAndReturn({ _ in 0 })


  _ = y.covariantReturn(int())
  _ = y.covariantReturn(try intThrows())
  _ = y.covariantReturn(try! intThrows())

  _ = y.covariantOptionalReturn(int())
  _ = y.covariantOptionalReturn(try intThrows())
  _ = y.covariantOptionalReturn(try! intThrows())

  _ = try y.covariantReturnOrThrow(int())
  _ = try y.covariantReturnOrThrow(try intThrows())
  _ = try y.covariantReturnOrThrow(try! intThrows())

  // FIXME: the dynamic self capture here has to happen after existential
  // opening as well.
  //_ = y.covariantClosureArgAndReturn({ _ in 0 })


  _ = z.covariantReturn(int())
  _ = z.covariantReturn(try intThrows())
  _ = z.covariantReturn(try! intThrows())

  _ = z.covariantOptionalReturn(int())
  _ = z.covariantOptionalReturn(try intThrows())
  _ = z.covariantOptionalReturn(try! intThrows())

  _ = try z.covariantReturnOrThrow(int())
  _ = try z.covariantReturnOrThrow(try intThrows())
  _ = try z.covariantReturnOrThrow(try! intThrows())

  _ = z.covariantClosureArgAndReturn({ _ in 0 })
}
