// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/unsafe_swift_decls.swiftmodule %S/Inputs/unsafe_swift_decls.swift

// RUN: %target-typecheck-verify-swift -strict-memory-safety -enable-experimental-feature StrictConcurrency -I %t

// REQUIRES: concurrency
// REQUIRES: swift_feature_StrictConcurrency

@preconcurrency import unsafe_swift_decls // expected-warning{{'@preconcurrency' import is not memory-safe because it can silently introduce data races}}

class C: @unchecked Sendable {
  var counter: Int = 0
}

nonisolated(unsafe) var globalCounter = 0

func acceptSendable<T: Sendable>(_: T) { }

typealias RequiresSendable<T> = T where T: Sendable

@available(SwiftStdlib 5.1, *)
func f() async {
  nonisolated(unsafe) var counter = 0
  Task.detached {
    // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}{{5-5=unsafe }}
    counter += 1 // expected-note{{reference to nonisolated(unsafe) var 'counter' is unsafe in concurrently-executing code}}
  }
  // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}{{3-3=unsafe }}
  counter += 1 // expected-note{{reference to nonisolated(unsafe) var 'counter' is unsafe in concurrently-executing code}}
  // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}{{3-3=unsafe }}
  print(counter) // expected-note{{reference to nonisolated(unsafe) var 'counter' is unsafe in concurrently-executing code}}
  // expected-warning@+1{{expression uses unsafe constructs but is not marked with 'unsafe'}}{{3-3=unsafe }}
  print(globalCounter) // expected-note{{reference to nonisolated(unsafe) var 'globalCounter' is unsafe in concurrently-executing code}}

  acceptSendable(C()) // okay
}

typealias WeirdC = RequiresSendable<C> // okay


@available(SwiftStdlib 5.9, *)
final class MyExecutor: SerialExecutor {
  func enqueue(_ job: consuming ExecutorJob) { fatalError("boom") }
  @unsafe func asUnownedSerialExecutor() -> UnownedSerialExecutor { fatalError("boom") }
}

// Ensure that this does not cause a reference cycle.
public struct TokenSyntax { }
public struct Syntax { }

open class SyntaxVisitor {
  open func visit(_ token: TokenSyntax) { }
}

open class SyntaxAnyVisitor: SyntaxVisitor {
  override open func visit(_ token: TokenSyntax) { }
}

@available(SwiftStdlib 5.1, *)
func testMemorySafetyWithForLoop() async {
  let (stream, continuation) = AsyncStream<Int>.makeStream()
  for await _ in stream {}
  _ = continuation
}
