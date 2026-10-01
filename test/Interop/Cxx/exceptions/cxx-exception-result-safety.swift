// RUN: %target-swift-frontend -typecheck %s -cxx-interoperability-mode=default -strict-memory-safety -verify -verify-ignore-unrelated

@_spi(CxxExceptionBridging) import Cxx

func resultHelperIsUnsafe() throws {
  // The helper is @unsafe because its closure must initialize the result
  // storage on success, which the type system cannot check. This closure
  // performs no pointer operation, but the call itself is still diagnosed.
  let _: Int = try _withCxxExceptionResult(Int.self) { _, _, _ in } // expected-warning {{expression uses unsafe constructs but is not marked with 'unsafe'}} expected-note {{reference to unsafe global function '_withCxxExceptionResult'}}
}
