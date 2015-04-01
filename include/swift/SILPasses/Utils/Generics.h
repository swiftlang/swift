//===-- Generics.h - Utilities for transforming generics --------*- C++ -*-===//
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
// This containts utilities for transforming generics.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_GENERICS_H
#define SWIFT_SIL_GENERICS_H

#include "swift/AST/Mangle.h"
#include "swift/SIL/Mangle.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/TypeSubstCloner.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"

using namespace swift;

class SpecializingCloner : public TypeSubstCloner<SpecializingCloner> {
public:
  SpecializingCloner(SILFunction *F,
                     TypeSubstitutionMap &InterfaceSubs,
                     TypeSubstitutionMap &ContextSubs,
                     StringRef NewName,
                     ArrayRef<Substitution> ApplySubs)
  : TypeSubstCloner(*initCloned(F, InterfaceSubs, NewName), *F, ContextSubs,
                    ApplySubs) {}
  /// Clone and remap the types in \p F according to the substitution
  /// list in \p Subs.
  static SILFunction *cloneFunction(SILFunction *F,
                                    TypeSubstitutionMap &InterfaceSubs,
                                    TypeSubstitutionMap &ContextSubs,
                                    StringRef NewName, ApplySite Caller) {
    // Clone and specialize the function.
    SpecializingCloner SC(F, InterfaceSubs, ContextSubs, NewName,
                          Caller.getSubstitutions());
    SC.populateCloned();
    SC.cleanUp(SC.getCloned());
    return SC.getCloned();
  }

private:
  static SILFunction *initCloned(SILFunction *Orig,
                                 TypeSubstitutionMap &InterfaceSubs,
                                 StringRef NewName);
  /// Clone the body of the function into the empty function that was created
  /// by initCloned.
  void populateCloned();
  SILFunction *getCloned() { return &getBuilder().getFunction(); }
};

struct GenericSpecializer {
  GenericSpecializer(SILModule *Mod) : M(Mod) {}

private:
  /// The SIL Module.
  SILModule *M;

  /// A worklist of functions to specialize.
  std::vector<SILFunction*> Worklist;

  bool specializeApplyInstGroup(llvm::SmallVectorImpl<ApplySite> &NewApplies);

public:
  /// Collect and specialize calls in a specific order specified by
  /// \p BotUpFuncList.
  bool specialize(const std::vector<SILFunction *> &BotUpFuncList);
};

#endif
