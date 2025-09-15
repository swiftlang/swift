//===--- TypeCheckEmbedded.h - Embedded -------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file provides type checking support for Embedded Swift.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SEMA_TYPECHECKEMBEDDED_H
#define SWIFT_SEMA_TYPECHECKEMBEDDED_H

#include <optional>

namespace swift {

class AbstractFunctionDecl;
class DeclContext;
struct DiagnosticBehavior;
class SourceLoc;
  
/// Whether we should diagnose language-level limitations of Embedded Swift
/// at the given source location, and how.
///
/// @param dc The declaration context in which the diagnostic would be emitted.
/// @param loc The source location at which the diagnostic would be emitted.
/// @param wasAlwaysEmbeddedError Whether this diagnostic was always an error
/// in Embedded Swift, which is used to avoid downgrading for
/// source-compatibility reasons.
/// @returns `std::nullopt` if no diagnostic should be emitted. Otherwise, a
/// behavior limit to place on the diagnostic when it is emitted.
std::optional<DiagnosticBehavior>
shouldDiagnoseEmbeddedLimitations(const DeclContext *dc, SourceLoc loc,
                                  bool wasAlwaysEmbeddedError = false);

/// Check embedded restrictions in the signature of the given function.
void checkEmbeddedRestrictionsInSignature(const AbstractFunctionDecl *func);

/// Diagnose a declaration of typed throws at the given location.
void diagnoseUntypedThrowsInEmbedded(const DeclContext *dc, SourceLoc throwsLoc);

}
#endif // SWIFT_SEMA_TYPECHECKEMBEDDED_H
