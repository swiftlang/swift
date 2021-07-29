//===--- RewriteContext.h - Term rewriting allocation arena -----*- C++ -*-===//
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

#ifndef SWIFT_REWRITECONTEXT_H
#define SWIFT_REWRITECONTEXT_H

#include "swift/AST/ASTContext.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Statistic.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/Support/Allocator.h"
#include "RewriteSystem.h"

namespace swift {

namespace rewriting {

/// A global object that can be shared by multiple rewrite systems.
///
/// It stores uniqued symbols and terms.
///
/// Out-of-line methods are documented in RewriteContext.cpp.
class RewriteContext final {
  friend class Symbol;
  friend class Term;

  /// Allocator for uniquing symbols and terms.
  llvm::BumpPtrAllocator Allocator;

  /// Folding set for uniquing symbols.
  llvm::FoldingSet<Symbol::Storage> Symbols;

  /// Folding set for uniquing terms.
  llvm::FoldingSet<Term::Storage> Terms;

  RewriteContext(const RewriteContext &) = delete;
  RewriteContext(RewriteContext &&) = delete;
  RewriteContext &operator=(const RewriteContext &) = delete;
  RewriteContext &operator=(RewriteContext &&) = delete;

  ASTContext &Context;

public:
  /// Statistical counters.
  UnifiedStatsReporter *Stats;

  RewriteContext(ASTContext &ctx) : Context(ctx), Stats(ctx.Stats) {}

  Term getTermForType(CanType paramType, const ProtocolDecl *proto);

  MutableTerm getMutableTermForType(CanType paramType,
                                    const ProtocolDecl *proto);

  ASTContext &getASTContext() { return Context; }

  Type getTypeForTerm(Term term,
                      TypeArrayView<GenericTypeParamType> genericParams,
                      const ProtocolGraph &protos) const;

  Type getTypeForTerm(const MutableTerm &term,
                      TypeArrayView<GenericTypeParamType> genericParams,
                      const ProtocolGraph &protos) const;

  Type getRelativeTypeForTerm(
                      const MutableTerm &term, const MutableTerm &prefix,
                      const ProtocolGraph &protos) const;
};

} // end namespace rewriting

} // end namespace swift

#endif
