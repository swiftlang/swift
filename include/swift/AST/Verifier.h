//===--- Verifier.h - Swift AST Verifier ------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the interface for verifying AST invariants.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_VERIFIER_H
#define SWIFT_VERIFIER_H

namespace swift {
  class TranslationUnit;
  
/// VerificationKind - Defines the kind of verification we should do.
/// There are different invariants in place at different phases of
/// compilation.
enum class VerificationKind {
  /// Verify that the AST corresponds to the result of parsing the
  /// file.
  Parsed,

  /// Verify that the AST corresponds to the result of binding global
  /// names.
  BoundNames,

  /// Verify that the AST corresponds to the result of checking types.
  CheckedTypes
};

void verify(TranslationUnit *TUnit, VerificationKind K);
  
} // end namespace swift

#endif
