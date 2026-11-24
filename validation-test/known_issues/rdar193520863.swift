// rdar://193520863
// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-sil -o /dev/null -verify -swift-version 6 %t/a.swift %t/b.swift

// The error depends on file order: it only occurs when a.swift precedes
// b.swift. In reverse order, compilation succeeds as expected.

// REQUIRES: objc_interop

//--- a.swift
import Foundation

func f(_: NSValue) {}

//--- b.swift
import Foundation

struct M<Value> {
  func withLock<Result>(
    _ body: (inout sending Value) -> sending Result
  ) -> sending Result {
    fatalError()
  }
}

func r(_ m: M<[NSNumber]>, _ n: NSNumber) {
  m.withLock { $0.append(n) }
  // expected-error@-1 {{'inout sending' parameter '$0' is accessible to code in the current isolation context at end of function}}
  // expected-note@-2 {{'$0' risks causing races in between code in the current isolation context and caller uses since caller assumes value is not actor isolated}}
}
