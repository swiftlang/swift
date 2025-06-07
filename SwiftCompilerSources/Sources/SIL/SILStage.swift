//===--- SILStage.swift ---------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

public enum SILStage: Int {
  /// "Raw" SIL, emitted by SILGen, but not yet run through guaranteed
  /// optimization and diagnostic passes.
  ///
  /// Raw SIL does not have fully-constructed SSA and may contain undiagnosed
  /// dataflow errors.
  case raw

  /// Canonical SIL, which has been run through at least the guaranteed
  /// optimization and diagnostic passes.
  ///
  /// Canonical SIL has stricter invariants than raw SIL. It must not contain
  /// dataflow errors, and some instructions must be canonicalized to simpler
  /// forms.
  case canonical

  /// Lowered SIL, which has been prepared for IRGen and will no longer
  /// be passed to canonical SIL transform passes.
  ///
  /// In lowered SIL, the SILType of all SILValues is its SIL storage
  /// type. Explicit storage is required for all address-only and resilient
  /// types.
  ///
  /// Generating the initial Raw SIL is typically referred to as lowering (from
  /// the AST). To disambiguate, refer to the process of generating the lowered
  /// stage of SIL as "address lowering".
  case lowered
}
