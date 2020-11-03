//===--- GenericCloner.h - Specializes generic functions  -------*- C++ -*-===//
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
// This contains the definition of a cloner class for creating specialized
// versions of generic functions by substituting concrete types.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_GENERICCLONER_H
#define SWIFT_SIL_GENERICCLONER_H

#include "swift/AST/Type.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/TypeSubstCloner.h"
#include "swift/SILOptimizer/Utils/BasicBlockOptUtils.h"
#include "swift/SILOptimizer/Utils/Generics.h"
#include "llvm/ADT/StringRef.h"
#include <functional>

namespace swift {

class GenericCloner
  : public TypeSubstCloner<GenericCloner, SILOptFunctionBuilder> {
  using SuperTy = TypeSubstCloner<GenericCloner, SILOptFunctionBuilder>;

  SILOptFunctionBuilder &FuncBuilder;
  IsSerialized_t Serialized;
  const ReabstractionInfo &ReInfo;
  CloneCollector::CallbackType Callback;
  llvm::SmallDenseMap<const SILDebugScope *, const SILDebugScope *, 8>
      RemappedScopeCache;

  llvm::SmallVector<AllocStackInst *, 8> AllocStacks;
  AllocStackInst *ReturnValueAddr = nullptr;

public:
  friend class SILCloner<GenericCloner>;

  GenericCloner(SILOptFunctionBuilder &FuncBuilder, SILFunction *F,
                const ReabstractionInfo &ReInfo, SubstitutionMap ParamSubs,
                StringRef NewName, CloneCollector::CallbackType Callback)
      : SuperTy(*createDeclaration(FuncBuilder, F, ReInfo, NewName), *F,
                ParamSubs),
        FuncBuilder(FuncBuilder), ReInfo(ReInfo), Callback(Callback) {
    assert(F->getDebugScope()->Parent != getCloned()->getDebugScope()->Parent);
  }
  /// Clone and remap the types in \p F according to the substitution
  /// list in \p Subs. Parameters are re-abstracted (changed from indirect to
  /// direct) according to \p ReInfo.
  static SILFunction *
  cloneFunction(SILOptFunctionBuilder &FuncBuilder,
                SILFunction *F,
                const ReabstractionInfo &ReInfo,
                SubstitutionMap ParamSubs,
                StringRef NewName,
                CloneCollector::CallbackType Callback =nullptr) {
    // Clone and specialize the function.
    GenericCloner SC(FuncBuilder, F, ReInfo, ParamSubs,
                     NewName, Callback);
    SC.populateCloned();
    return SC.getCloned();
  }

  void fixUp(SILFunction *calleeFunction);

  static SILFunction *createDeclaration(SILOptFunctionBuilder &FuncBuilder,
                                        SILFunction *Orig,
                                        const ReabstractionInfo &ReInfo,
                                        StringRef NewName);

protected:
  void visitTerminator(SILBasicBlock *BB);

  // FIXME: We intentionally call SILClonerWithScopes here to ensure
  //        the debug scopes are set correctly for cloned
  //        functions. TypeSubstCloner, SILClonerWithScopes, and
  //        SILCloner desperately need refactoring and/or combining so
  //        that the obviously right things are happening for cloning
  //        vs. inlining.
  void postProcess(SILInstruction *Orig, SILInstruction *Cloned) {
    // Call client-supplied callback function.
    if (Callback)
      Callback(Orig, Cloned);

    SILClonerWithScopes<GenericCloner>::postProcess(Orig, Cloned);
  }

private:
  /// Clone the body of the function into the empty function that was created
  /// by initCloned.
  void populateCloned();
  SILFunction *getCloned() { return &getBuilder().getFunction(); }

  const SILDebugScope *remapScope(const SILDebugScope *DS);

};

} // end namespace swift

#endif
