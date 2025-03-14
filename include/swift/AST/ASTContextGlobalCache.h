//===--- ASTContextGlobalCache.h - AST Context Cache ------------*- C++ -*-===//
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
// This file defines the ASTContext::GlobalCache type. DO NOT include this
// header from any other header: it should only be included in those .cpp
// files that need to access the side tables. There are no include guards to
// force the issue.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTContext.h"

namespace swift {

/// A collection of side tables associated with the ASTContext itself, meant
// as
struct ASTContext::GlobalCache {
  /// Mapping from normal protocol conformances to the explicitly-specified
  /// global actor isolations, e.g., when the conformance was spelled
  /// `@MainActor P` or similar.
  llvm::DenseMap<const NormalProtocolConformance *, TypeExpr *>
      conformanceExplicitGlobalActorIsolation;
};

} // end namespace 
