//===--- DiagnosticHelpers.h ----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_MANDATORY_DIAGNOSTICHELPERS_H
#define SWIFT_SILOPTIMIZER_MANDATORY_DIAGNOSTICHELPERS_H

#include "swift/AST/ASTContext.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"

namespace swift {
namespace siloptimizer {

template <typename... T, typename... U>
static inline InFlightDiagnostic diagnoseError(ASTContext &context,
                                               SourceLoc loc, Diag<T...> diag,
                                               U &&...args) {
  return std::move(context.Diags.diagnose(loc, diag, std::forward<U>(args)...)
                       .warnUntilLanguageMode(LanguageMode::v6));
}

template <typename... T, typename... U>
static inline InFlightDiagnostic diagnoseError(ASTContext &context,
                                               SILLocation loc, Diag<T...> diag,
                                               U &&...args) {
  return siloptimizer::diagnoseError(context, loc.getSourceLoc(), diag,
                                     std::forward<U>(args)...);
}

template <typename... T, typename... U>
static inline InFlightDiagnostic diagnoseError(const Operand *op,
                                               Diag<T...> diag, U &&...args) {
  return siloptimizer::diagnoseError(
      op->getUser()->getFunction()->getASTContext(),
      op->getUser()->getLoc().getSourceLoc(), diag, std::forward<U>(args)...);
}

template <typename... T, typename... U>
static InFlightDiagnostic diagnoseError(const SILInstruction *inst,
                                        Diag<T...> diag, U &&...args) {
  return siloptimizer::diagnoseError(inst->getFunction()->getASTContext(),
                                     inst->getLoc().getSourceLoc(), diag,
                                     std::forward<U>(args)...);
}

template <typename... T, typename... U>
static InFlightDiagnostic diagnoseErrorAndHighlight(const SILInstruction *inst,
                                                    Diag<T...> diag,
                                                    U &&...args) {
  return std::move(
      siloptimizer::diagnoseError(inst->getFunction()->getASTContext(),
                                  inst->getLoc().getSourceLoc(), diag,
                                  std::forward<U>(args)...)
          .highlight(inst->getLoc().getSourceRange()));
}

template <typename... T, typename... U>
static inline InFlightDiagnostic
diagnoseNote(ASTContext &context, SourceLoc loc, Diag<T...> diag, U &&...args) {
  return context.Diags.diagnose(loc, diag, std::forward<U>(args)...);
}

template <typename... T, typename... U>
static inline InFlightDiagnostic diagnoseNote(ASTContext &context,
                                              SILLocation loc, Diag<T...> diag,
                                              U &&...args) {
  return siloptimizer::diagnoseNote(context, loc.getSourceLoc(), diag,
                                    std::forward<U>(args)...);
}

template <typename... T, typename... U>
static inline InFlightDiagnostic diagnoseNote(const Operand *op,
                                              Diag<T...> diag, U &&...args) {
  return siloptimizer::diagnoseNote(
      op->getUser()->getFunction()->getASTContext(),
      op->getUser()->getLoc().getSourceLoc(), diag, std::forward<U>(args)...);
}

template <typename... T, typename... U>
static inline InFlightDiagnostic diagnoseNote(const SILInstruction *inst,
                                              Diag<T...> diag, U &&...args) {
  return siloptimizer::diagnoseNote(inst->getFunction()->getASTContext(),
                                    inst->getLoc().getSourceLoc(), diag,
                                    std::forward<U>(args)...);
}

template <typename... T, typename... U>
static inline InFlightDiagnostic
diagnoseNoteAndHighlight(const SILInstruction *inst, Diag<T...> diag,
                         U &&...args) {
  return std::move(
      siloptimizer::diagnoseNote(inst->getFunction()->getASTContext(),
                                 inst->getLoc().getSourceLoc(), diag,
                                 std::forward<U>(args)...)
          .highlight(inst->getLoc().getSourceRange()));
}

} // namespace siloptimizer
} // namespace swift

#endif
