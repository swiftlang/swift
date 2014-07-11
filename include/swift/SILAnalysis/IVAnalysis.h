//===------------------- IVAnalysis.h - SIL IV Analysis -------*- C++ -*---===//
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

#ifndef SWIFT_SILANALYSIS_IVANALYSIS_H
#define SWIFT_SILANALYSIS_IVANALYSIS_H

#include "swift/SILAnalysis/Analysis.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILValue.h"
#include "swift/SILPasses/Utils/SCCVisitor.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/Debug.h"

namespace swift {

class IVInfo : public SCCVisitor<IVInfo> {
public:
  typedef llvm::SmallVectorImpl<ValueBase *> SCCType;
  friend class SCCVisitor;

public:
  IVInfo(SILFunction &F) : SCCVisitor(F) {
    run();
  }

  bool isInductionVariable(ValueBase *IV) {
    auto End = InductionVariableMap.end();
    auto Found = InductionVariableMap.find(IV);
    return Found != End;
  }

  SILArgument *getInductionVariableHeader(ValueBase *IV) {
    assert(isInductionVariable(IV) && "Expected induction variable!");

    return InductionVariableMap.find(IV)->second;
  }

private:
  // Map from an element of an induction sequence to the header.
  llvm::DenseMap<const ValueBase *, SILArgument *> InductionVariableMap;

  bool isInductionSequence(SCCType &);
  void visit(SCCType &SCC);
};

class IVAnalysis : public SILAnalysis {
public:
  IVAnalysis(SILModule *) : SILAnalysis(AnalysisKind::IVAnalysis) {}
  ~IVAnalysis();

  static bool classof(const SILAnalysis *S) {
    return S->getKind() == AnalysisKind::IVAnalysis;
  }

  IVInfo &getIVInfo(SILFunction *F) {
    if (!IVInfos.count(F))
      IVInfos[F] = new IVInfo(*F);
    return *IVInfos[F];
  }

  virtual void invalidate(InvalidationKind K) {
    if (K >= InvalidationKind::Instructions) {
      for (auto IVI : IVInfos)
        delete IVI.second;

      IVInfos.clear();
    }
  }

  virtual void invalidate(SILFunction *F, InvalidationKind K) {
    if (K >= InvalidationKind::Instructions) {
      if (IVInfos.count(F)) {
        delete IVInfos[F];
        IVInfos.erase(F);
      }
    }
  }

private:
  llvm::DenseMap<SILFunction *, IVInfo *> IVInfos;
};

}

#endif
