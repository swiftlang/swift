//===--- SILParserState.h - SILParserState declaration -------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_SILPARSERSTATE_H
#define SWIFT_SIL_SILPARSERSTATE_H

#include "swift/AST/Identifier.h"
#include "swift/Basic/LLVM.h"
#include "swift/Parse/ParseSILSupport.h"
#include "swift/SIL/SILFunction.h"

#include "llvm/ADT/DenseMap.h"

//===----------------------------------------------------------------------===//
// SILParserState
//===----------------------------------------------------------------------===//

namespace swift {

class Parser;
class SILModule;

/// The global state of the SIL parser that the ordinary parser needs to
/// maintain while parsing a SIL file.  Local state, like the value map of
/// a SIL function, does not need to be kept here.
class SILParserState : public SILParserStateBase {
public:
  explicit SILParserState(SILModule &M) : M(M) {}
  ~SILParserState();

  SILModule &M;

  /// This is all of the forward referenced functions with
  /// the location for where the reference is.
  llvm::DenseMap<Identifier, Located<SILFunction *>> ForwardRefFns;
  /// A list of all functions forward-declared by a sil_scope.
  llvm::DenseSet<SILFunction *> PotentialZombieFns;

  /// A map from textual .sil scope number to SILDebugScopes.
  llvm::DenseMap<unsigned, SILDebugScope *> ScopeSlots;

  /// Did we parse a sil_stage for this module?
  bool DidParseSILStage = false;

  bool parseDeclSIL(Parser &P) override;
  bool parseDeclSILStage(Parser &P) override;
  bool parseSILVTable(Parser &P) override;
  bool parseSILMoveOnlyDeinit(Parser &P) override;
  bool parseSILGlobal(Parser &P) override;
  bool parseSILWitnessTable(Parser &P) override;
  bool parseSILDefaultWitnessTable(Parser &P) override;
  bool parseSILDefaultOverrideTable(Parser &P) override;
  bool parseSILDifferentiabilityWitness(Parser &P) override;
  bool parseSILCoverageMap(Parser &P) override;
  bool parseSILProperty(Parser &P) override;
  bool parseSILScope(Parser &P) override;

  /// Mark potential zombie functions as zombies.
  void markZombies();
};

} // end namespace swift

#endif // SWIFT_SIL_SILPARSERSTATE_H
