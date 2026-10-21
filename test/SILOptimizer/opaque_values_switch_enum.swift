// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module -enable-library-evolution -module-name Lib -o %t/Lib.swiftmodule %t/Lib.swift
// RUN: %target-swift-frontend -enable-sil-opaque-values -emit-sil -sil-verify-all -verify -I %t -module-name main %t/main.swift -Xllvm -sil-print-types | %FileCheck %t/main.swift

// Switches over address-only enums pass AddressLowering. Noncopyable ones also
// pass the noncopyable checker, and copyable ones that match on a borrow read
// the enum in place.

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

public struct CopyableResilientIdle<T> {
  public var value: T?
  public init(_ value: T?) { self.value = value }
}

public enum CopyableResilientState<T> {
  case idle(CopyableResilientIdle<T>)
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

struct CopyableIdle<T> { var value: T? }
enum CopyableState<T> { case idle(CopyableIdle<T>), count(Int), other }

// CHECK-LABEL: sil hidden @$s4main16borrowingDefaultyxSgAA13CopyableStateOyxGlF :
// CHECK:       bb0({{%[^,]+}} : $*Optional<T>, [[STATE:%[^,]+]] : $*CopyableState<T>):
// CHECK-NOT:     copy_addr [[STATE]]
// CHECK:         switch_enum_addr [[STATE]] : $*CopyableState<T>, case #CopyableState.idle!enumelt: [[IDLE:bb[0-9]+]], default
// CHECK:       [[IDLE]]:
// CHECK-NEXT:    unchecked_inplace_enum_data_addr [[STATE]] : $*CopyableState<T>, #CopyableState.idle!enumelt
// CHECK-NOT:     destroy_addr [[STATE]]
// CHECK-LABEL: } // end sil function '$s4main16borrowingDefaultyxSgAA13CopyableStateOyxGlF'
func borrowingDefault<T>(_ s: CopyableState<T>) -> T? {
  switch s {
  case .idle(let idle): return idle.value
  default: return nil
  }
}

// CHECK-LABEL: sil hidden @$s4main14borrowingGuardySiAA13CopyableStateOyxGlF :
// CHECK:       bb0([[STATE:%[^,]+]] : $*CopyableState<T>):
// CHECK-NOT:     copy_addr [[STATE]]
// CHECK:         switch_enum_addr [[STATE]] : $*CopyableState<T>,
// CHECK-NOT:     destroy_addr [[STATE]]
// CHECK-LABEL: } // end sil function '$s4main14borrowingGuardySiAA13CopyableStateOyxGlF'
func borrowingGuard<T>(_ s: CopyableState<T>) -> Int {
  switch s {
  case .count(let n) where n > 0: return n
  case .idle: return -1
  default: return 0
  }
}

struct CopyableStateMachine<T> {
  var state: CopyableState<T>

  func get() -> T? {
    switch self.state {
    case .idle(let idle): return idle.value
    default: return nil
    }
  }
}

enum CopyableOuter<T> { case state(CopyableState<T>), none }

// CHECK-LABEL: sil hidden @$s4main15borrowingNestedyxSgAA13CopyableOuterOyxGlF :
// CHECK:       bb0({{%[^,]+}} : $*Optional<T>, [[OUTER:%[^,]+]] : $*CopyableOuter<T>):
// CHECK:         switch_enum_addr [[OUTER]] : $*CopyableOuter<T>, case #CopyableOuter.state!enumelt: [[STATE_BB:bb[0-9]+]],
// CHECK:       [[STATE_BB]]:
// CHECK-NEXT:    [[STATE:%[^,]+]] = unchecked_inplace_enum_data_addr [[OUTER]] : $*CopyableOuter<T>, #CopyableOuter.state!enumelt
// CHECK-NEXT:    switch_enum_addr [[STATE]] : $*CopyableState<T>,
// CHECK-LABEL: } // end sil function '$s4main15borrowingNestedyxSgAA13CopyableOuterOyxGlF'
func borrowingNested<T>(_ o: CopyableOuter<T>) -> T? {
  switch o {
  case .state(.idle(let idle)): return idle.value
  case .state: return nil
  case .none: return nil
  }
}

func borrowingOptional<T>(_ o: CopyableIdle<T>?) -> T? {
  switch o {
  case .some(let idle) where idle.value != nil: return idle.value
  default: return nil
  }
}

enum Payloads<T> { case opaque(T), pair(T, Klass), klass(Klass), other }

func borrowingTuplePayload<T>(_ p: Payloads<T>, _ k: Klass) -> Bool {
  switch p {
  case .pair(_, let x) where x === k: return true
  case .klass(let x) where x === k: return true
  default: return false
  }
}

func borrowingMultiplePatterns<T>(_ p: Payloads<T>) -> T? {
  switch p {
  case .opaque(let x), .pair(let x, _): return x
  default: return nil
  }
}

func borrowingDefaultBinding<T>(_ p: Payloads<T>) -> Payloads<T>? {
  switch p {
  case .other: return nil
  case let other: return other
  }
}

func borrowingFallthrough<T>(_ p: Payloads<T>) -> Int {
  var r = 0
  switch p {
  case .opaque: r += 1; fallthrough
  case .other: r += 10
  default: r += 100
  }
  return r
}

func borrowingCast<T>(_ p: Payloads<T>) -> Int {
  switch p {
  case .opaque(let x as Int): return x
  case .opaque(is String): return -2
  default: return -1
  }
}

func borrowingTuple<T>(_ a: Payloads<T>, _ b: Payloads<T>) -> Bool {
  switch (a, b) {
  case (.opaque, .opaque): return true
  case (.other, _): return true
  default: return false
  }
}

func resilientBorrowingDefault<T>(_ s: CopyableResilientState<T>) -> T? {
  switch s {
  case .idle(let idle): return idle.value
  default: return nil
  }
}

func resilientBorrowingGuard<T>(_ s: CopyableResilientState<T>) -> Int {
  switch s {
  case .count(let n) where n > 0: return n
  case .idle: return -1
  default: return 0
  }
}

func resilientBorrowingNoBinding<T>(_ s: CopyableResilientState<T>) -> Bool {
  switch s {
  case .idle: return true
  default: return false
  }
}
