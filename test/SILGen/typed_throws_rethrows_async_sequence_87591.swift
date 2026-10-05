// RUN: %target-swift-emit-silgen-ossa -o /dev/null -enable-sil-opaque-values -module-name main -target %target-swift-6.0-abi-triple %s
// RUN: %target-swift-emit-sil -sil-verify-all -enable-sil-opaque-values -module-name main -target %target-swift-6.0-abi-triple %s -o /dev/null
// RUN: %target-swift-emit-sil -sil-verify-all -module-name main -target %target-swift-6.0-abi-triple %s -o /dev/null

// RUN: %target-swift-emit-silgen -module-name main -target %target-swift-6.0-abi-triple %s | %FileCheck %s

// REQUIRES: concurrency

// https://github.com/swiftlang/swift/issues/87591
//
// A `rethrows` function that can throw because of an AsyncSequence
// conformance is type-checked as throwing the sequence's `Failure` (SE-0421),
// but at the ABI level it still throws `any Error`. Calling one from a context
// that throws `Failure` must cast the error back rather than crash in SILGen.

struct MyErr: Error {}

struct Seq: AsyncSequence {
  typealias Element = Int
  typealias Failure = MyErr

  struct AsyncIterator: AsyncIteratorProtocol {
    mutating func next(isolation actor: isolated (any Actor)?) async throws(MyErr) -> Int? { nil }
    mutating func next() async throws -> Int? { nil }
  }

  func makeAsyncIterator() -> AsyncIterator { AsyncIterator() }
}

// A loadable concrete error type, passed back by value.
// CHECK-LABEL: sil hidden [ossa] @$s4main8concreteySiSgAA3SeqVYaAA5MyErrVYKF
// CHECK: unconditional_checked_cast_addr any Error in {{%.*}} to MyErr in {{%.*}}
// CHECK: end sil function '$s4main8concreteySiSgAA3SeqVYaAA5MyErrVYKF'
func concrete(_ s: Seq) async throws(MyErr) -> Int? {
  try await s.first(where: { _ in true })
}

// An address-only generic error type, passed back indirectly.
// CHECK-LABEL: sil hidden [ossa] @$s4main7genericy7ElementQzSgxYa7FailureQzYKSciRzlF
// CHECK: unconditional_checked_cast_addr any Error in {{%.*}} to S.Failure in {{%.*}}
// CHECK: end sil function '$s4main7genericy7ElementQzSgxYa7FailureQzYKSciRzlF'
func generic<S: AsyncSequence>(_ s: S) async throws(S.Failure) -> S.Element? {
  try await s.first(where: { _ in true })
}

// The original report: a do/catch infers `Self.Failure` as its thrown type.
// CHECK-LABEL: sil hidden {{.*}}[ossa] @$sSci4mainE10firstOrNil7ElementQzyYas013ExpressibleByD7LiteralADRQrlF
// CHECK: unconditional_checked_cast_addr any Error in {{%.*}} to Self.Failure in {{%.*}}
// CHECK: end sil function '$sSci4mainE10firstOrNil7ElementQzyYas013ExpressibleByD7LiteralADRQrlF'
extension AsyncSequence {
  func firstOrNil() async -> Element where Element: ExpressibleByNilLiteral {
    do {
      return try await first(where: { _ in true }) ?? nil
    } catch {
      return nil
    }
  }
}

// Other `rethrows` AsyncSequence operations take the same path.
// CHECK-LABEL: sil hidden [ossa] @$s4main11containsAnyySbxYa7FailureQzYKSciRzlF
// CHECK: unconditional_checked_cast_addr any Error in {{%.*}} to S.Failure in {{%.*}}
// CHECK: end sil function '$s4main11containsAnyySbxYa7FailureQzYKSciRzlF'
func containsAny<S: AsyncSequence>(_ s: S) async throws(S.Failure) -> Bool {
  try await s.contains(where: { _ in true })
}
