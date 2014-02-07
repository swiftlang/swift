//===-- Devirtualizer.h ------ Devirtualize virtual calls ------*- C++ -*-===//
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
// This file declares the data structures exposed by the SILDevirtualizer Pass.
//
// TODO: Consider moving to SILAnalysis/SpecializedArgs.h once fully
// implemented.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILPASSES_DEVIRTUALIZER_H
#define SWIFT_SILPASSES_DEVIRTUALIZER_H

#include "swift/SILAnalysis/Analysis.h"

namespace swift {

/// Associate an argument with a resolved type.
struct SILResolvedArg {
  SILArgument *Arg = 0;
  CanType Ty;

  SILResolvedArg() = default;

  SILResolvedArg(SILArgument *Arg, CanType Ty): Arg(Arg), Ty(Ty) {}

  bool operator==(const SILResolvedArg &Other) const {
    return Arg == Other.Arg && Ty == Other.Ty;
  }
  bool operator!=(const SILResolvedArg &Other) const {
    return !operator==(Other);
  }
};
/// List of resolved arguments in function signature order.
typedef SmallVector<SILResolvedArg, 4> SILResolvedArgList;

/// RASuper and RASub are each a list of resolved arguments from a
/// function. These are not necessarilly the same function. Typically RASuper
/// contains resolved arguments for a specialization of the RASub function.
///
/// @return true if the resolved args in 'RASuper' cover the resolved args in
/// 'RASub'. A SILResolvedArgList must exist in RASuper at the same argument
/// position and same type as RASub.
bool isCoveringSpecialization(SILResolvedArgList &RASuper,
                              SILResolvedArgList &RASub);

/// Associate a function with a list of resolved argument types.
struct SILArgTypeSpecialization {
  SILFunction *Func = 0;
  SILResolvedArgList ResArgs;

  SILArgTypeSpecialization(SILFunction *F, SILResolvedArgList RA)
    : Func(F), ResArgs(std::move(RA)) {}

  SILArgTypeSpecialization(SILArgTypeSpecialization &&ATS)
    : Func(ATS.Func), ResArgs(std::move(ATS.ResArgs)) {}
};

/// Map original SILFunctions to specialized SILFunctions that have been
/// optimized under the assumption that only certain dynamic types may be
/// passed as arguments.
class SILSpecializedArgsAnalysis : public SILAnalysis
{
  // List each function specialization and its resolved argument types.
  std::vector<SILArgTypeSpecialization> Specializations;

  // Map a specialized function to its resolved arguments.
  llvm::DenseMap<SILFunction*, unsigned> SpecializedArgsMap;

  // Map an original function to a list of specializations ordered by decreasing
  // number of resolved arguments.
  llvm::DenseMap<SILFunction*, SmallVector<unsigned, 4> > SpecializedFuncMap;

public:
  SILSpecializedArgsAnalysis(): SILAnalysis(AnalysisKind::SpecializedArgs) {}
  virtual ~SILSpecializedArgsAnalysis() {}

  static bool classof(const SILAnalysis *S) {
    return S->getKind() == AnalysisKind::SpecializedArgs;
  }

  virtual void invalidate(InvalidationKind K);

  virtual void invalidate(SILFunction*, InvalidationKind K) { invalidate(K); }

  /// Get a specialization that at least covers the given ResArgs.
  /// TODO: returning a pointer to internal storage is unsafe.
  SILArgTypeSpecialization *getMinSpecialization(SILFunction *OrigF,
                                                 SILResolvedArgList &ResArgs);

  /// Get a specialization that is covered by the given ResArgs.
  /// TODO: returning a pointer to internal storage is unsafe.
  SILArgTypeSpecialization *getMaxSpecialization(SILFunction *OrigF,
                                                 SILResolvedArgList &ResArgs);

  /// Add a specialized function and its argument types to the result map.
  bool addSpecialization(SILFunction *OrigF, SILFunction *NewF,
                         SILResolvedArgList &ResArgs);

  SILResolvedArgList *getResolvedArgs(SILFunction *SpecialF);

  CanType lookupSpecializedArg(SILArgument *Arg);
};
} // end namespace swift
#endif
