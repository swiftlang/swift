//===--- MandatoryInlining.cpp - Perform inlining of "force_inline" sites -===//
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

#define DEBUG_TYPE "mandatory-inlining"
#include "swift/Subsystems.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/Diagnostics.h"
#include "swift/SILPasses/Utils/SILInliner.h"
#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/ImmutableSet.h"
#include "llvm/Support/Debug.h"
using namespace swift;

typedef llvm::DenseSet<SILFunction*> DenseFunctionSet;
typedef llvm::ImmutableSet<SILFunction*> ImmutableFunctionSet;

STATISTIC(NumMandatoryInlineSitesInlined,
          "Number of function application sites inlined by the mandatory "
          "inlining pass");

template<typename...T, typename...U>
static void diagnose(ASTContext &Context, SourceLoc loc, Diag<T...> diag,
              U &&...args) {
  Context.Diags.diagnose(loc,
                         diag, std::forward<U>(args)...);
}

/// \brief Inlines all mandatory inlined functions into the body of a function,
/// first recursively inlining all mandatory apply instructions in those
/// functions into their bodies if necessary.
///
/// \param F the function to be processed
/// \param AI nullptr if this is being called from the top level; the relevant
///   ApplyInst requiring the recursive call when non-null
/// \param FullyInlinedSet the set of all functions already known to be fully
///   processed, to avoid processing them over again
/// \param SetFactory an instance of ImmutableFunctionSet::Factory
/// \param CurrentInliningSet the set of functions currently being inlined in
///   the current call stack of recursive calls
///
/// \returns true if successful, false if failed due to circular inlining.
static bool
runOnFunctionRecursively(SILFunction *F, ApplyInst* AI,
                         DenseFunctionSet &FullyInlinedSet,
                         ImmutableFunctionSet::Factory &SetFactory,
                         ImmutableFunctionSet CurrentInliningSet) {
  // Avoid reprocessing functions needlessly
  if (FullyInlinedSet.find(F) != FullyInlinedSet.end())
    return true;
  
  // Prevent attempt to circularly inline.
  if (CurrentInliningSet.contains(F)) {
    // This cannot happen on a top-level call, so AI should be non-null.
    assert(AI && "Cannot have circular inline without apply");
    SILLocation L = AI->getLoc();
    assert(L && "Must have location for forced inline apply");
    diagnose(F->getModule().getASTContext(), L.getStartSourceLoc(),
             diag::circular_force_inline);
    return false;
  }

  // Add to the current inlining set (immutably, so we only affect the set
  // during this call and recursive subcalls).
  CurrentInliningSet = SetFactory.add(CurrentInliningSet, F);

  SmallVector<ApplyInst*, 4> ApplySites;
  for (auto &BB : *F) {
    for (auto &I : BB) {
      ApplyInst *InnerAI;
      if ((InnerAI = dyn_cast<ApplyInst>(&I)) && InnerAI->isForceInline()) {
        // Figure out of this is something we have the body for
        // FIXME: once fragile SIL is serialized in modules, these can be
        // asserts, since forced inline functions should always have their
        // bodies available.
        SILValue Callee = InnerAI->getCallee();
        FunctionRefInst *FRI = dyn_cast<FunctionRefInst>(Callee.getDef());
        if (!FRI)
          continue;
        assert(Callee.getResultNumber() == 0);
        SILFunction *CalledFunc = FRI->getFunction();
        if (!CalledFunc || CalledFunc->empty())
          continue;

        // Then recursively process it first before trying to inline it.
        if (!runOnFunctionRecursively(CalledFunc, InnerAI, FullyInlinedSet,
                                      SetFactory, CurrentInliningSet)) {
          // If we failed due to circular inlining, then emit some notes to
          // trace back the failure if we have more information.
          // FIXME: possibly it could be worth recovering and attempting other
          // inlines within this same recursive call rather than simply
          // propogating the failure.
          if (AI) {
            SILLocation L = AI->getLoc();
            assert(L && "Must have location for forced inline apply");
            diagnose(F->getModule().getASTContext(), L.getStartSourceLoc(),
                     diag::note_while_inlining);
          }
          return false;
        }

        ApplySites.push_back(InnerAI);
      }
    }
  }

  // Do the inlining separately from the inspection loop to avoid iterator
  // invalidation issues.
  if (!ApplySites.empty()) {
    SILInliner Inliner(*F);
    for (auto *InnerAI : ApplySites) {
      Inliner.inlineFunction(InnerAI);
      ++NumMandatoryInlineSitesInlined;
    }
  }

  // Keep track of full inlined functions so we don't waste time recursively
  // reprocessing them.
  FullyInlinedSet.insert(F);
  return true;
}

//===----------------------------------------------------------------------===//
//                          Top Level Driver
//===----------------------------------------------------------------------===//

void swift::performSILMandatoryInlining(SILModule *M) {
  DenseFunctionSet FullyInlinedSet;
  ImmutableFunctionSet::Factory SetFactory;
  for (auto &F : *M)
    runOnFunctionRecursively(&F, nullptr, FullyInlinedSet, SetFactory,
                              SetFactory.getEmptySet());
}
