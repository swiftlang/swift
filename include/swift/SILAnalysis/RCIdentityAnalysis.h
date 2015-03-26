//===--- RCIdentityAnalysis.h ------------------------------*- C++ -*------===//
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
//  This is an analysis that determines the ref count identity (i.e. gc root) of
//  a pointer. Any values with the same ref count identity are able to be
//  retained and released interchangably.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILANALYSIS_RCIDENTITYANALYSIS_H
#define SWIFT_SILANALYSIS_RCIDENTITYANALYSIS_H

#include "swift/SIL/SILValue.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SILAnalysis/Analysis.h"
#include "swift/SILAnalysis/DominanceAnalysis.h"
#include "swift/SILPasses/PassManager.h"

namespace swift {

class DominanceAnalysis;

/// This class is a simple wrapper around an identity cache.
class RCIdentityAnalysis : public SILAnalysis {
  llvm::DenseMap<SILValue, SILValue> Cache;
  llvm::DenseSet<SILArgument *> VisitedArgs;
  DominanceAnalysis *DA;

  /// This number is arbitrary and conservative. At some point if compile time
  /// is not an issue, this value should be made more aggressive (i.e. greater).
  enum { MaxRecursionDepth = 16 };

public:
  RCIdentityAnalysis(SILModule *, SILPassManager *PM)
    : SILAnalysis(AnalysisKind::RCIdentity), Cache(), VisitedArgs(),
      DA(PM->getAnalysis<DominanceAnalysis>()) {}
  RCIdentityAnalysis(const RCIdentityAnalysis &) = delete;
  RCIdentityAnalysis &operator=(const RCIdentityAnalysis &) = delete;

  SILValue getRCIdentityRoot(SILValue V);

  /// Return all recursive users of V, looking through users which propagate
  /// RCIdentity. *NOTE* This ignores obvious ARC escapes where the a potential
  /// user of the RC is not managed by ARC. For instance
  /// unchecked_trivial_bit_cast.
  void getRCUsers(SILValue V, llvm::SmallVectorImpl<SILInstruction *> &Users);

  static bool classof(const SILAnalysis *S) {
    return S->getKind() == AnalysisKind::RCIdentity;
  }

  virtual void invalidate(SILAnalysis::PreserveKind K) {
      Cache.clear();
  }

  // TODO: Add function specific cache to save compile time.
  virtual void invalidate(SILFunction* F, SILAnalysis::PreserveKind K) {
    invalidate(K);
  }

private:
  SILValue getRCIdentityRootInner(SILValue V, unsigned RecursionDepth);
  SILValue stripRCIdentityPreservingOps(SILValue V, unsigned RecursionDepth);
  SILValue stripRCIdentityPreservingArgs(SILValue V, unsigned RecursionDepth);
  SILValue stripOneRCIdentityIncomingValue(SILArgument *Arg, SILValue V);
  bool findDominatingNonPayloadedEdge(SILBasicBlock *IncomingEdgeBB,
                                      SILValue RCIdentity);
};

} // end swift namespace

#endif
