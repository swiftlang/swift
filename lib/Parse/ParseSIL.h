//===--- ParseSIL.h - Internal interface to SIL parsing ---------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_PARSER_PARSESIL_H
#define SWIFT_PARSER_PARSESIL_H

#include "swift/AST/Identifier.h"
#include "swift/Basic/SourceManager.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseSet.h"
#include <memory>

namespace swift {
  class DiagnosticEngine;
  class SILDebugScope;
  class SILFunction;
  class SILModule;

  class SILParserTUState {
  public:
    explicit SILParserTUState(SILModule &M) : M(M) {}
    ~SILParserTUState();

    SILModule &M;

    /// This is all of the forward referenced functions with
    /// the location for where the reference is.
    llvm::DenseMap<Identifier,
                   std::pair<SILFunction*, SourceLoc>> ForwardRefFns;
    /// A list of all functions forward-declared by a sil_scope.
    llvm::DenseSet<SILFunction *> PotentialZombieFns;

    /// A map from textual .sil scope number to SILDebugScopes.
    llvm::DenseMap<unsigned, SILDebugScope *> ScopeSlots;

    /// Did we parse a sil_stage for this module?
    bool DidParseSILStage = false;
  };
} // end namespace swift

#endif // SWIFT_PARSER_PARSESIL_H

