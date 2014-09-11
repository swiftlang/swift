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

namespace swift {

/// This class is a simple wrapper around an identity cache.
class RCIdentityAnalysis : public SILAnalysis {
  llvm::DenseMap<SILValue, SILValue> Cache;
  llvm::DenseSet<SILArgument *> VisitedArgs;

  /// This number is arbitrary and conservative. At some point if compile time
  /// is not an issue, this value should be made more aggressive (i.e. greater).
  enum { MaxRecursionDepth = 16 };

public:
  RCIdentityAnalysis(SILModule *) : SILAnalysis(AnalysisKind::RCIdentity) {}
  RCIdentityAnalysis(const RCIdentityAnalysis &) = delete;
  RCIdentityAnalysis &operator=(const RCIdentityAnalysis &) = delete;

  SILValue getRCIdentityRoot(SILValue V);

  static bool classof(const SILAnalysis *S) {
    return S->getKind() == AnalysisKind::RCIdentity;
  }

  virtual void invalidate(InvalidationKind K) {
    if (K >= InvalidationKind::Instructions) {
      Cache.clear();
    }
  }

  // TODO: Add function specific cache to save compile time.
  virtual void invalidate(SILFunction* F, InvalidationKind K) {
    invalidate(K);
  }

private:
  SILValue getRCIdentityRootInner(SILValue V, unsigned RecursionDepth);
  SILValue stripRCIdentityPreservingOps(SILValue V, unsigned RecursionDepth);
  SILValue stripRCIdentityPreservingArgs(SILValue V, unsigned RecursionDepth);

};

} // end swift namespace

#endif
