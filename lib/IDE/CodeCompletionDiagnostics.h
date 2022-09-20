//===--- CodeCompletionDiagnostics.h --------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IDE_CODECOMPLETIONDIAGNOSTICS_H
#define SWIFT_IDE_CODECOMPLETIONDIAGNOSTICS_H

#include "swift/IDE/CodeCompletionResult.h"

namespace swift {

class ValueDecl;

namespace ide {

/// Populate \p severity and \p Out with the context-free diagnostics for \p D.
/// See \c NotRecommendedReason for an explaination of context-free vs.
/// contextual diagnostics.
/// Returns \c true if it fails to generate the diagnostics.
bool getContextFreeCompletionDiagnostics(
    ContextFreeNotRecommendedReason reason, const ValueDecl *D,
    CodeCompletionDiagnosticSeverity &severity, llvm::raw_ostream &Out);

/// Populate \p severity and \p Out with the contextual for \p reason.
/// \p NameForDiagnostic is the name of the decl that produced this diagnostic.
/// \p Ctx is a context that's purely used to have a reference to a diagnostic
/// engine.
/// See \c NotRecommendedReason for an explaination of context-free vs.
/// contextual diagnostics.
/// Returns \c true if it fails to generate the diagnostics.
bool getContextualCompletionDiagnostics(
    ContextualNotRecommendedReason Reason, StringRef NameForDiagnostics,
    CodeCompletionDiagnosticSeverity &Severity, llvm::raw_ostream &Out,
    const ASTContext &Ctx);

} // namespace ide
} // namespace swift

#endif // SWIFT_IDE_CODECOMPLETIONDIAGNOSTICS_H
