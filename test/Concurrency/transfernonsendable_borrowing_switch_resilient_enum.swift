// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module -enable-library-evolution -target %target-swift-5.1-abi-triple -module-name Lib -o %t/Lib.swiftmodule %t/Lib.swift
// RUN: %target-swift-frontend -emit-sil -swift-version 6 -target %target-swift-5.1-abi-triple -verify -I %t -module-name main %t/main.swift -o /dev/null
// RUN: %target-swift-frontend -emit-sil -swift-version 6 -target %target-swift-5.1-abi-triple -enable-sil-opaque-values -verify -I %t -module-name main %t/main.swift -o /dev/null

// REQUIRES: concurrency

// A borrowing switch over a resilient enum reads the payload through
// unchecked_borrow_enum_data_addr. The payload stays in the enum's region.

//--- Lib.swift
public struct Box<T>: ~Copyable {
  public var value: T
  public init(_ value: T) { self.value = value }
}

public enum NoncopyableState<T>: ~Copyable {
  case box(Box<T>)
  case count(Int)
  case other
}

public enum CopyableState<T> {
  case value(T)
  case count(Int)
  case other
}

//--- main.swift
import Lib

class NonSendable {}

@MainActor func sink(_ x: NonSendable) {}

func noncopyableCount<T>(_ s: borrowing NoncopyableState<T>) -> Int {
  switch s {
  case .count(let n): return n
  default: return 0
  }
}

func noncopyableSendPayload(_ s: borrowing NoncopyableState<NonSendable>) async {
  switch s {
  case .box(let box):
    await sink(box.value) // expected-error {{sending value of non-Sendable type 'NonSendable' risks causing data races}}
    // expected-note @-1 {{sending value of non-Sendable type 'NonSendable' to main actor-isolated global function 'sink' risks causing races in between code in the current isolation context and main actor-isolated uses}}
  default: break
  }
}

func copyableSendPayload(_ s: CopyableState<NonSendable>) async {
  switch s {
  case .value(let x):
    await sink(x) // expected-error {{sending 'x' risks causing data races}}
    // expected-note @-1 {{sending 'x' to main actor-isolated global function 'sink' risks causing data races between main actor-isolated code and code in the current isolation context}}
  default: break
  }
}
