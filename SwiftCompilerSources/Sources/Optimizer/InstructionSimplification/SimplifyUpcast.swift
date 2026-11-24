//===--- SimplifyUpcast.swift ---------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SIL

extension UpcastInst : DebugReconstructionBlockSimplifiable {
  /// Folds undef, and combines a chain of upcasts:
  /// ```
  ///   %1 = upcast %0 : $Derived to $Base
  ///   %2 = upcast %1 : $Base to $Root   ->   %2 = upcast %0 : $Derived to $Root
  /// ```
  func simplifyForDebugReconstructionBlock(_ context: SimplifyContext) {
    if foldUndefOperands(context) {
      return
    }
    while let upcast = fromInstance as? UpcastInst {
      operand.set(to: upcast.fromInstance, context)
    }
  }
}
