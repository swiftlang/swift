//===--- Verifier.swift ---------------------------------------------------===//
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

/// Runs the mandatory verification (see `Function.verify`), including ownership verification,
/// on each OSSA function.
///
/// In contrast to the SILVerifier, this also runs in compilers built without assertions.
let verifierPass = FunctionPass(name: "verifier") {
  (function: Function, context: FunctionPassContext) in

  guard function.hasOwnership, context.options.enableMandatoryVerification(for: function) else {
    return
  }
  function.verify(VerificationContext(context))
}
