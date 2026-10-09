// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module -enable-library-evolution -module-name Lib -o %t/Lib.swiftmodule %t/Lib.swift
// RUN: %target-swift-frontend -enable-sil-opaque-values -emit-sil -sil-verify-all -verify -I %t -module-name main %t/main.swift -o /dev/null

// Switches over noncopyable address-only enums pass AddressLowering and the
// noncopyable checker.

//--- Lib.swift
public struct ResilientIdle<T>: ~Copyable {
  public var value: T?
  public init(_ value: T?) { self.value = value }
}

public enum ResilientState<T>: ~Copyable {
  case idle(ResilientIdle<T>)
  case count(Int)
  case other
}

//--- main.swift
import Lib

struct Idle<T>: ~Copyable { var value: T? }
enum State<T>: ~Copyable { case idle(Idle<T>), other }

func borrowingBinding<T>(_ s: borrowing State<T>) -> T? {
  switch s {
  case .idle(let idle): return idle.value
  case .other: return nil
  }
}

func borrowingNoBinding<T>(_ s: borrowing State<T>) -> Bool {
  switch s {
  case .idle: return true
  case .other: return false
  }
}

struct StateMachine<T>: ~Copyable {
  var state: State<T>

  func get() -> T? {
    switch self.state {
    case .idle(let idle): return idle.value
    case .other: return nil
    }
  }
}

func consumingSwitch<T>(_ s: consuming State<T>) -> T? {
  switch consume s {
  case .idle(let idle): return idle.value
  case .other: return nil
  }
}

func consumingResult<S: ~Copyable, F: Error>(
  _ result: consuming Result<S, F>,
  success: (consuming S) -> Void, failure: (F) -> Void
) {
  switch consume result {
  case .success(let value): success(value)
  case .failure(let error): failure(error)
  }
}

struct NC<T>: ~Copyable { var value: T }
enum Inner<T>: ~Copyable { case a(NC<T>), b(Int) }
enum Outer<T>: ~Copyable { case inner(Inner<T>), none }

func borrowingOptional<T>(_ s: borrowing NC<T>?) -> T? {
  switch s {
  case .some(let nc): return nc.value
  case .none: return nil
  }
}

func borrowingNested<T>(_ o: borrowing Outer<T>) -> T? {
  switch o {
  case .inner(.a(let nc)): return nc.value
  case .inner(.b): return nil
  case .none: return nil
  }
}

func borrowingLoadablePayload<T>(_ o: borrowing Inner<T>) -> Int {
  switch o {
  case .a: return -1
  case .b(let n): return n
  }
}

final class Klass {}

struct Handle: ~Copyable { var klass: Klass }
enum Guarded<T>: ~Copyable { case pair(T, Klass), handle(Handle), other }

func borrowingNoncopyableLoadablePayload<T>(_ g: borrowing Guarded<T>) -> Klass? {
  switch g {
  case .handle(let h): return h.klass
  default: return nil
  }
}

func consumingGuardedPayloads<T>(_ g: consuming Guarded<T>, _ k: Klass) -> Bool {
  switch consume g {
  case .pair(_, let x) where x === k: return true
  default: return false
  }
}

func resilientBorrowingBinding<T>(_ s: borrowing ResilientState<T>) -> T? {
  switch s {
  case .idle(let idle): return idle.value
  case .count: return nil
  case .other: return nil
  @unknown default: return nil
  }
}

func resilientBorrowingNoBinding<T>(_ s: borrowing ResilientState<T>) -> Bool {
  switch s {
  case .idle: return true
  default: return false
  }
}

func resilientBorrowingCount<T>(_ s: borrowing ResilientState<T>) -> Int {
  switch s {
  case .idle: return -1
  case .count(let n): return n
  case .other: return 0
  @unknown default: return 0
  }
}

func resilientConsumingSwitch<T>(_ s: consuming ResilientState<T>) -> T? {
  switch consume s {
  case .idle(let idle): return idle.value
  case .count: return nil
  case .other: return nil
  @unknown default: return nil
  }
}
