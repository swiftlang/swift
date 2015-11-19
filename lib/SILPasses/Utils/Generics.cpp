//===- Generics.cpp ---- Utilities for transforming generics ----*- C++ -*-===//
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

#define DEBUG_TYPE "generic-specializer"

#include "swift/Strings.h"
#include "swift/SILPasses/Utils/Generics.h"
#include "swift/SILPasses/Utils/GenericCloner.h"

using namespace swift;

// Create a new apply based on an old one, but with a different
// function being applied.
ApplySite swift::replaceWithSpecializedFunction(ApplySite AI,
                                                SILFunction *NewF) {
  SILLocation Loc = AI.getLoc();
  ArrayRef<Substitution> Subst;

  SmallVector<SILValue, 4> Arguments;
  for (auto &Op : AI.getArgumentOperands()) {
    Arguments.push_back(Op.get());
  }

  SILBuilderWithScope Builder(AI.getInstruction());
  FunctionRefInst *FRI = Builder.createFunctionRef(Loc, NewF);

  if (auto *TAI = dyn_cast<TryApplyInst>(AI))
    return Builder.createTryApply(Loc, FRI, TAI->getSubstCalleeSILType(),
                                  {}, Arguments, TAI->getNormalBB(),
                                  TAI->getErrorBB());

  if (auto *A = dyn_cast<ApplyInst>(AI))
    return Builder.createApply(Loc, FRI, Arguments, A->isNonThrowing());

  if (auto *PAI = dyn_cast<PartialApplyInst>(AI))
      return Builder.createPartialApply(Loc, FRI,
                                        PAI->getSubstCalleeSILType(),
                                        {},
                                        Arguments,
                                        PAI->getType());

  llvm_unreachable("unhandled kind of apply");
}


/// Try to convert definition into declaration.
static bool convertExtenralDefinitionIntoDeclaration(SILFunction *F) {
  // Bail if it is a declaration already.
  if (!F->isDefinition())
    return false;
  // Bail if there is no external implementation of this function.
  if (!F->isAvailableExternally())
    return false;
  // Bail if has a shared visibility, as there are no guarantees
  // that an implementation is available elsewhere.
  if (hasSharedVisibility(F->getLinkage()))
    return false;
  // Make this definition a declaration by removing the body of a function.
  F->convertToDeclaration();
  assert(F->isExternalDeclaration() &&
         "Function should be an external declaration");

  DEBUG(llvm::dbgs() << "  removed external function " << F->getName() << "\n");

  return true;
}

/// Check of a given name could be a name of a white-listed
/// specialization.
bool swift::isWhitelistedSpecialization(StringRef SpecName) {
  // The whitelist of classes and functions from the stdlib,
  // whose specializations we want to preserve.
  ArrayRef<StringRef> Whitelist = {
      "Array",
      "_ArrayBuffer",
      "_ContiguousArrayBuffer",
      "Range",
      "RangeGenerator",
      "_allocateUninitializedArray",
      "UTF8",
      "UTF16",
      "String",
      "_StringBuffer",
      "_toStringReadOnlyPrintable",
  };

  // TODO: Once there is an efficient API to check if
  // a given symbol is a specialization of a specific type,
  // use it instead. Doing demangling just for this check
  // is just wasteful.
  auto DemangledNameString =
     swift::Demangle::demangleSymbolAsString(SpecName);

  StringRef DemangledName = DemangledNameString;

  auto pos = DemangledName.find("generic ", 0);
  if (pos == StringRef::npos)
    return false;

  // Create "of Swift"
  llvm::SmallString<64> OfString;
  llvm::raw_svector_ostream buffer(OfString);
  buffer << "of ";
  buffer << STDLIB_NAME <<'.';

  StringRef OfStr = buffer.str();

  pos = DemangledName.find(OfStr, pos);

  if (pos == StringRef::npos)
    return false;

  pos += OfStr.size();

  for(auto Name: Whitelist) {
    auto pos1 = DemangledName.find(Name, pos);
    if (pos1 == pos && !isalpha(DemangledName[pos1+Name.size()])) {
      return true;
    }
  }

  return false;
}

/// Cache a specialization.
/// For now, it is performed only for specializations in the
/// standard library. But in the future, one could think of
/// maintaining a cache of optimized specializations.
///
/// Mark specializations as public, so that they can be used
/// by user applications. These specializations are supposed to be
/// used only by -Onone compiled code. They should be never inlined.
static bool cacheSpecialization(SILModule &M, SILFunction *F) {
  // Do not remove functions from the white-list. Keep them around.
  // Change their linkage to public, so that other applications can refer to it.

  if (M.getOptions().Optimization >= SILOptions::SILOptMode::Optimize &&
      F->getLinkage() != SILLinkage::Public &&
      F->getModule().getSwiftModule()->getName().str() == STDLIB_NAME) {
    if (F->getLinkage() != SILLinkage::Public &&
        isWhitelistedSpecialization(F->getName())) {

      DEBUG(
        auto DemangledNameString =
          swift::Demangle::demangleSymbolAsString(F->getName());
        StringRef DemangledName = DemangledNameString;
        llvm::dbgs() << "Keep specialization: " << DemangledName << " : "
                     << F->getName() << "\n");
      // Make it public, so that others can refer to it.
      // NOTE: This function may refer to non-public symbols, which may lead
      // to problems, if you ever try to inline this function. Therefore,
      // these specializations should only be used to refer to them,
      // but should never be inlined!
      // The general rule could be: Never inline specializations from stdlib!

      // NOTE: Making these specializations public at this point breaks
      // some optimizations. Therefore, just mark the function.
      // DeadFunctionElimination pass will check if the function is marked
      // and preserve it if required.
      F->setKeepAsPublic(true);
      return true;
    }
  }
  return false;
}

/// Try to look up an existing specialization in the specialization cache.
/// If it is found, it tries to link this specialization.
///
/// For now, it performs a lookup only in the standard library.
/// But in the future, one could think of maintaining a cache
/// of optimized specializations.
static SILFunction *lookupExistingSpecialization(SILModule &M,
                                                 StringRef FunctionName) {
  // Try to link existing specialization only in -Onone mode.
  // All other compilation modes perform specialization themselves.
  // TODO: Cache optimized specializations and perform lookup here?
  // TODO: Only check that this function exists, but don't read
  // its body. It can save some compile-time.
  if (isWhitelistedSpecialization(FunctionName) &&
      M.linkFunction(FunctionName, SILOptions::LinkingMode::LinkNormal))
    return M.lookUpFunction(FunctionName);

  return nullptr;
}

SILFunction *swift::getExistingSpecialization(SILModule &M,
                                              StringRef FunctionName) {
  auto *Specialization = lookupExistingSpecialization(M, FunctionName);
  if (!Specialization)
    return nullptr;
  if (hasPublicVisibility(Specialization->getLinkage())) {
    // The bodies of existing specializations cannot be used,
    // as they may refer to non-public symbols.
    if (Specialization->isDefinition())
      Specialization->convertToDeclaration();
    Specialization->setLinkage(SILLinkage::PublicExternal);
    // Ignore body for -Onone and -Odebug.
    assert((Specialization->isExternalDeclaration() ||
            convertExtenralDefinitionIntoDeclaration(Specialization)) &&
           "Could not remove body of the found specialization");
    if (!convertExtenralDefinitionIntoDeclaration(Specialization)) {
      DEBUG(
          llvm::dbgs() << "Could not remove body of specialization: "
                       << FunctionName << '\n');
    }

    DEBUG(
        llvm::dbgs() << "Found existing specialization for: "
                     << FunctionName << '\n';
        llvm::dbgs() << swift::Demangle::demangleSymbolAsString(
                            Specialization->getName()) << "\n\n");
  } else {
    // Forget about this function.
    DEBUG(llvm::dbgs() << "Cannot reuse the specialization: "
           << swift::Demangle::demangleSymbolAsString(Specialization->getName())
           <<"\n");
    return nullptr;
  }

  return Specialization;
}

ApplySite swift::trySpecializeApplyOfGeneric(ApplySite Apply,
                                             SILFunction *&NewFunction,
                                             CloneCollector &Collector) {
  NewFunction = nullptr;

  assert(Apply.hasSubstitutions() && "Expected an apply with substitutions!");

  auto *F = cast<FunctionRefInst>(Apply.getCallee())->getReferencedFunction();
  assert(F->isDefinition() && "Expected definition to specialize!");

  if (!F->shouldOptimize()) {
    DEBUG(llvm::dbgs() << "    Cannot specialize function " << F->getName()
                       << " marked to be excluded from optimizations.\n");
    return ApplySite();
  }

  DEBUG(llvm::dbgs() << "  ApplyInst: " << *Apply.getInstruction());

  // Create the substitution maps.
  TypeSubstitutionMap InterfaceSubs;
  TypeSubstitutionMap ContextSubs;

  if (F->getLoweredFunctionType()->getGenericSignature())
    InterfaceSubs = F->getLoweredFunctionType()->getGenericSignature()
      ->getSubstitutionMap(Apply.getSubstitutions());


  if (F->getContextGenericParams())
    ContextSubs = F->getContextGenericParams()
      ->getSubstitutionMap(Apply.getSubstitutions());

  // We do not support partial specialization.
  if (hasUnboundGenericTypes(InterfaceSubs)) {
    DEBUG(llvm::dbgs() << "    Can not specialize with interface subs.\n");
    return ApplySite();
  }
  if (hasDynamicSelfTypes(InterfaceSubs)) {
    DEBUG(llvm::dbgs() << "    Cannot specialize with dynamic self.\n");
    return ApplySite();
  }

  llvm::SmallString<64> ClonedName;
  {
    llvm::raw_svector_ostream buffer(ClonedName);
    ArrayRef<Substitution> Subs = Apply.getSubstitutions();
    Mangle::Mangler M(buffer);
    Mangle::GenericSpecializationMangler Mangler(M, F, Subs);
    Mangler.mangle();
  }
  DEBUG(llvm::dbgs() << "    Specialized function " << ClonedName << '\n');

  auto &M = Apply.getInstruction()->getModule();
  // If we already have this specialization, reuse it.
  auto NewF = M.lookUpFunction(ClonedName);

  if (NewF) {
#ifndef NDEBUG
    // Make sure that NewF's subst type matches the expected type.
    auto Subs = Apply.getSubstitutions();
    auto FTy =
      F->getLoweredFunctionType()->substGenericArgs(M,
                                                    M.getSwiftModule(),
                                                    Subs);
    assert(FTy == NewF->getLoweredFunctionType() &&
           "Previously specialized function does not match expected type.");
#endif
  } else {

    // Do not create any new specializations at Onone.
    if (M.getOptions().Optimization <= SILOptions::SILOptMode::None)
      return ApplySite();

    DEBUG(
      if (M.getOptions().Optimization <= SILOptions::SILOptMode::Debug) {
        llvm::dbgs() << "Creating a specialization: " << ClonedName << "\n"; });

    // Create a new function.
    NewF = GenericCloner::cloneFunction(F, InterfaceSubs, ContextSubs,
                                        ClonedName, Apply,
                                        Collector.getCallback());

    NewFunction = NewF;

    // Check if this specialization should be cached.
    cacheSpecialization(M, NewF);
  }
  return replaceWithSpecializedFunction(Apply, NewF);
}
