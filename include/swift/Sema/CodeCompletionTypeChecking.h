//===--- CodeCompletionTypeChecking.h - Type-check entry points -*- c++ -*-===//
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
/// \file
/// \brief Provides extra type-checking entry points for use during code
/// completion, which happens *without* type-checking an entire file at once.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SEMA_CODECOMPLETIONTYPECHECKING_H
#define SWIFT_SEMA_CODECOMPLETIONTYPECHECKING_H

#include "swift/Basic/SourceLoc.h"
#include <memory>

namespace swift {
  class AbstractFunctionDecl;
  class Decl;
  class Expr;
  class LazyResolver;
  class TranslationUnit;

  /// \brief Typecheck a declaration parsed during code completion.
  ///
  /// \returns true on success, false on error.
  bool typeCheckCompletionDecl(TranslationUnit *TU, Decl *D);

  /// \brief Typecheck an expression parsed during code completion.
  ///
  /// \returns true on success, false on error.
  bool typeCheckCompletionContextExpr(TranslationUnit *TU, Expr *&parsedExpr);

  /// Partially typecheck the specified function body.
  bool typeCheckAbstractFunctionBodyUntil(TranslationUnit *TU,
                                          AbstractFunctionDecl *AFD,
                                          SourceLoc EndTypeCheckLoc);

  /// A unique_ptr for LazyResolver that can perform additional cleanup.
  using OwnedResolver = std::unique_ptr<LazyResolver, void(*)(LazyResolver*)>;

  /// Creates a lazy type resolver for use in lookups.
  OwnedResolver createLazyResolver(TranslationUnit *TU);
}

#endif
