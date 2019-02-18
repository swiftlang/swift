//===--- IVAnalysis.h - SIL IV Analysis -------------------------*- C++ -*-===//
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

#ifndef SWIFT_SILOPTIMIZER_ANALYSIS_IVANALYSIS_H
#define SWIFT_SILOPTIMIZER_ANALYSIS_IVANALYSIS_H

#include "swift/SILOptimizer/Analysis/Analysis.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILValue.h"
#include "swift/SILOptimizer/Utils/SCCVisitor.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/Debug.h"

namespace swift {

class IVInfo : public SCCVisitor<IVInfo> {
public:
  typedef llvm::SmallVectorImpl<SILNode *> SCCType;
  friend SCCVisitor;

public:

  /// A descriptor for an induction variable comprised of a header argument
  /// (phi node) and an increment by an integer literal.
  class IVDesc {
  public:
    BuiltinInst *Inc;
    IntegerLiteralInst *IncVal;

    IVDesc() : Inc(nullptr), IncVal(nullptr) {}
    IVDesc(BuiltinInst *AI, IntegerLiteralInst *I) : Inc(AI), IncVal(I) {}

    operator bool() { return Inc != nullptr && IncVal != nullptr; }
    static IVDesc invalidIV() { return IVDesc(); }
  };

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

  IVDesc getInductionDesc(SILArgument *Arg) {
    llvm::DenseMap<const ValueBase *, IVDesc>::iterator CI =
        InductionInfoMap.find(Arg);
    if (CI == InductionInfoMap.end())
      return IVDesc::invalidIV();
    return CI->second;
  }

private:
  // Map from an element of an induction sequence to the header.
  llvm::DenseMap<const ValueBase *, SILArgument *> InductionVariableMap;

  // Map from an induction variable header to the induction descriptor.
  llvm::DenseMap<const ValueBase *, IVDesc> InductionInfoMap;

  SILArgument *isInductionSequence(SCCType &SCC);
  void visit(SCCType &SCC);
};

class IVAnalysis final : public FunctionAnalysisBase<IVInfo> {
public:
  IVAnalysis(SILModule *)
      : FunctionAnalysisBase<IVInfo>(SILAnalysisKind::InductionVariable) {}
  IVAnalysis(const IVAnalysis &) = delete;
  IVAnalysis &operator=(const IVAnalysis &) = delete;

  static bool classof(const SILAnalysis *S) {
    return S->getKind() == SILAnalysisKind::InductionVariable;
  }

  std::unique_ptr<IVInfo> newFunctionAnalysis(SILFunction *F) override {
    return llvm::make_unique<IVInfo>(*F);
  }

  /// For now we always invalidate.
  virtual bool shouldInvalidate(SILAnalysis::InvalidationKind K) override {
    return true;
  }
};

}

#endif
