//===--- RCIdentityAnalysis.h -----------------------------------*- C++ -*-===//
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
//
//  This is an analysis that determines the ref count identity (i.e. gc root) of
//  a pointer. Any values with the same ref count identity are able to be
//  retained and released interchangeably.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_ANALYSIS_RCIDENTITYANALYSIS_H
#define SWIFT_SILOPTIMIZER_ANALYSIS_RCIDENTITYANALYSIS_H

#include "swift/SIL/SILValue.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SILOptimizer/Analysis/Analysis.h"
#include "swift/SILOptimizer/Analysis/DominanceAnalysis.h"
#include "swift/SILOptimizer/PassManager/PassManager.h"

namespace swift {

/// Limit the size of the rc identity cache. We keep a cache per function.
constexpr unsigned MaxRCIdentityCacheSize = 64;

class DominanceAnalysis;

/// This class is a simple wrapper around an identity cache.
class RCIdentityFunctionInfo {
  llvm::DenseSet<SILArgument *> VisitedArgs;
  // RC identity cache.
  llvm::DenseMap<SILValue, SILValue> RCCache;
  DominanceAnalysis *DA;

  /// This number is arbitrary and conservative. At some point if compile time
  /// is not an issue, this value should be made more aggressive (i.e. greater).
  enum { MaxRecursionDepth = 16 };

public:
  RCIdentityFunctionInfo(DominanceAnalysis *D) : VisitedArgs(),
  DA(D) {}

  SILValue getRCIdentityRoot(SILValue V);

  /// Return all recursive users of V, looking through users which propagate
  /// RCIdentity.
  ///
  /// *NOTE* This ignores obvious ARC escapes where the a potential
  /// user of the RC is not managed by ARC. For instance
  /// unchecked_trivial_bit_cast.
  void getRCUses(SILValue V, llvm::SmallVectorImpl<Operand *> &Uses);

  /// A helper method that calls getRCUses and then maps each operand to the
  /// operands user and then uniques the list.
  ///
  /// *NOTE* The routine asserts that the passed in Users array is empty for
  /// simplicity. If needed this can be changed, but it is not necessary given
  /// current uses.
  void getRCUsers(SILValue V, llvm::SmallVectorImpl<SILInstruction *> &Users);

  void handleDeleteNotification(SILNode *node) {
    auto value = dyn_cast<ValueBase>(node);
    if (!value)
      return;

    // Check the cache. If we don't find it, there is nothing to do.
    auto Iter = RCCache.find(SILValue(value));
    if (Iter == RCCache.end())
      return;

    // Then erase Iter from the cache.
    RCCache.erase(Iter);
  }

private:
  SILValue getRCIdentityRootInner(SILValue V, unsigned RecursionDepth);
  SILValue stripRCIdentityPreservingOps(SILValue V, unsigned RecursionDepth);
  SILValue stripRCIdentityPreservingArgs(SILValue V, unsigned RecursionDepth);
  SILValue stripOneRCIdentityIncomingValue(SILArgument *Arg, SILValue V);
  bool findDominatingNonPayloadedEdge(SILBasicBlock *IncomingEdgeBB,
                                      SILValue RCIdentity);
};

class RCIdentityAnalysis : public FunctionAnalysisBase<RCIdentityFunctionInfo> {
  DominanceAnalysis *DA;

public:
  RCIdentityAnalysis(SILModule *)
      : FunctionAnalysisBase<RCIdentityFunctionInfo>(
            SILAnalysisKind::RCIdentity),
        DA(nullptr) {}

  RCIdentityAnalysis(const RCIdentityAnalysis &) = delete;
  RCIdentityAnalysis &operator=(const RCIdentityAnalysis &) = delete;

  virtual void handleDeleteNotification(SILNode *node) override {
    // If the parent function of this instruction was just turned into an
    // external declaration, bail. This happens during SILFunction destruction.
    SILFunction *F = node->getFunction();
    if (F->isExternalDeclaration()) {
      return;
    }
    get(F)->handleDeleteNotification(node);
  }

  virtual bool needsNotifications() override { return true; }

  static bool classof(const SILAnalysis *S) {
    return S->getKind() == SILAnalysisKind::RCIdentity;
  }

  virtual void initialize(SILPassManager *PM) override;
  
  virtual RCIdentityFunctionInfo *newFunctionAnalysis(SILFunction *F) override {
    return new RCIdentityFunctionInfo(DA);
  }

  virtual bool shouldInvalidate(SILAnalysis::InvalidationKind K) override {
    return true;
  }

 };

} // end swift namespace

#endif
