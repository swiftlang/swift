
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -enable-library-evolution -emit-module-path=%t/resilient_struct.swiftmodule %S/../Inputs/resilient_struct.swift
// RUN: %target-swift-emit-silgen -I %t -enable-library-evolution %s | %FileCheck %s

import resilient_struct

public struct ResilientStruct {
  public init() {}
}

// CHECK-LABEL: sil [ossa] @$s18capture_resilience13hasClosureLetAA15ResilientStructVycyF : $@convention(thin) () -> @owned @callee_guaranteed () -> @out ResilientStruct
public func hasClosureLet() -> () -> ResilientStruct {
  let s = ResilientStruct()

  // CHECK-LABEL: sil private [ossa] @$s18capture_resilience13hasClosureLetAA15ResilientStructVycyFADycfU_ : $@convention(thin) (ResilientStruct) -> @out ResilientStruct
  return { s }
}

// CHECK-LABEL: sil [ossa] @$s18capture_resilience13hasClosureVarAA15ResilientStructVycyF : $@convention(thin) () -> @owned @callee_guaranteed () -> @out ResilientStruct
public func hasClosureVar() -> () -> ResilientStruct {
  var s = ResilientStruct()

  // CHECK-LABEL: sil private [ossa] @$s18capture_resilience13hasClosureVarAA15ResilientStructVycyFADycfU_ : $@convention(thin) (@guaranteed { var ResilientStruct }) -> @out ResilientStruct
  return { s }
}

// CHECK-LABEL: sil [serialized] [ossa] @$s18capture_resilience22hasInlinableClosureLetAA15ResilientStructVycyF : $@convention(thin) () -> @owned @callee_guaranteed () -> @out ResilientStruct
@inlinable public func hasInlinableClosureLet() -> () -> ResilientStruct {
  let s = ResilientStruct()

  // CHECK_LABEL: sil shared [serialized] [ossa] @$s18capture_resilience22hasInlinableClosureLetAA15ResilientStructVycyFADycfU_ : $@convention(thin) (@in_guaranteed ResilientStruct) -> @out ResilientStruct
  return { s }
}

// CHECK-LABEL: sil [serialized] [ossa] @$s18capture_resilience22hasInlinableClosureVarAA15ResilientStructVycyF : $@convention(thin) () -> @owned @callee_guaranteed () -> @out ResilientStruct
@inlinable public func hasInlinableClosureVar() -> () -> ResilientStruct {
  var s = ResilientStruct()

  // CHECK-LABEL: sil shared [serialized] [ossa] @$s18capture_resilience22hasInlinableClosureVarAA15ResilientStructVycyFADycfU_ : $@convention(thin) (@guaranteed { var ResilientStruct }) -> @out ResilientStruct
  return { s }
}

public func consume(_: () -> ResilientStruct) {}

// CHECK-LABEL: sil [ossa] @$s18capture_resilience21hasNoEscapeClosureLetyyF : $@convention(thin) () -> ()
public func hasNoEscapeClosureLet() {
  let s = ResilientStruct()

  // CHECK-LABEL: sil private [ossa] @$s18capture_resilience21hasNoEscapeClosureLetyyFAA15ResilientStructVyXEfU_ : $@convention(thin) (ResilientStruct) -> @out ResilientStruct
  consume { s }
}

// CHECK-LABEL: sil [ossa] @$s18capture_resilience21hasNoEscapeClosureVaryyF : $@convention(thin) () -> ()
public func hasNoEscapeClosureVar() {
  var s = ResilientStruct()

  // CHECK-LABEL: sil private [ossa] @$s18capture_resilience21hasNoEscapeClosureVaryyFAA15ResilientStructVyXEfU_ : $@convention(thin) (@inout_aliasable ResilientStruct) -> @out ResilientStruct
  consume { s }
}

// CHECK-LABEL: sil [serialized] [ossa] @$s18capture_resilience30hasInlinableNoEscapeClosureLetyyF : $@convention(thin) () -> ()
@inlinable public func hasInlinableNoEscapeClosureLet() {
  let s = ResilientStruct()

  // CHECK-LABEL: sil shared [serialized] [ossa] @$s18capture_resilience30hasInlinableNoEscapeClosureLetyyFAA15ResilientStructVyXEfU_ : $@convention(thin) (@in_guaranteed ResilientStruct) -> @out ResilientStruct
  consume { s }
}

// CHECK-LABEL: sil [serialized] [ossa] @$s18capture_resilience30hasInlinableNoEscapeClosureVaryyF : $@convention(thin) () -> ()
@inlinable public func hasInlinableNoEscapeClosureVar() {
  var s = ResilientStruct()

  // CHECK-LABEL: sil shared [serialized] [ossa] @$s18capture_resilience30hasInlinableNoEscapeClosureVaryyFAA15ResilientStructVyXEfU_ : $@convention(thin) (@inout_aliasable ResilientStruct) -> @out ResilientStruct
  consume { s }
}
