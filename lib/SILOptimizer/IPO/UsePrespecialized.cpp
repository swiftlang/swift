//===--- UsePrespecialized.cpp - use pre-specialized functions ------------===//
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

#define DEBUG_TYPE "use-prespecialized"
#include "swift/Basic/Assertions.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/Generics.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"
#include "swift/SILOptimizer/Utils/SpecializationMangler.h"
#include "llvm/Support/Debug.h"

using namespace swift;

namespace {



static void collectApplyInst(SILFunction &F,
                             llvm::SmallVectorImpl<ApplySite> &NewApplies) {
  // Scan all of the instructions in this function in search of ApplyInsts.
  for (auto &BB : F)
    for (auto &I : BB)
      if (ApplySite AI = ApplySite::isa(&I))
        NewApplies.push_back(AI);
}

/// A simple pass which replaces each apply of a generic function by an apply
/// of the corresponding pre-specialized function, if such a pre-specialization
/// exists.
class UsePrespecialized: public SILModuleTransform {
  ~UsePrespecialized() override { }

  void run() override {
    auto &M = *getModule();
    for (auto &F : M) {
      if (replaceByPrespecialized(F)) {
        invalidateAnalysis(&F, SILAnalysis::InvalidationKind::FunctionBody);
      }
    }
  }

  bool replaceByPrespecialized(SILFunction &F);
};

} // end anonymous namespace


// This needs to be in sync with the set of symbols in
// "stdlib/public/SwiftOnoneSupport/SwiftOnoneSupport.swift" or we miss
// opportunity.
static llvm::DenseSet<StringRef> KnownUnspecialized;

#if defined(__clang__)
_Pragma("clang diagnostic push")
_Pragma("clang diagnostic ignored \"-Wdollar-in-identifier-extension\"")
#endif

static const char *UnspecializedPrespecializedFuncs[] = {
 "$sSlsE20_failEarlyRangeCheck_6boundsy5IndexQz_SnyADGtF",
 "$sSlss16IndexingIteratorVyxG0B0RtzrlE04makeB0ACyF",
 "$sSKsE8reverseds18ReversedCollectionVyxGyF",
 "$sSMsSKRzrlE8_reverse6withinySny5IndexSlQzG_tF",
 "$sSMsSKRzrlE14_insertionSort6within2byySny5IndexSlQzG_Sb7ElementSTQz_AHtKXEtKF",
 "$sSMsSKRzrlE14_insertionSort6within9sortedEnd2byySny5IndexSlQzG_AFSb7ElementSTQz_AItKXEtKF",
 "$sSMsSkRzrlE4sort2byySb7ElementSTQz_ADtKXE_tKF",
 "$sSksSx5IndexRpzSnyABG7IndicesRtzSiAA_6StrideRTzrlE5index5afterA2B_tF",
 "$sSksSx5IndexRpzSnyABG7IndicesRtzSiAA_6StrideRTzrlE7indicesACvg",
 "$ss27_allocateUninitializedArrayySayxG_BptBwlF",
 "$sS2ayxGycfC",
 "$sSa10startIndexSivg",
 "$sSa12_getCapacitySiyF",
 "$sSa21_makeMutableAndUniqueyyF",
 "$sSa22_copyToContiguousArrays0cD0VyxGyF",
 "$sSa29_hoistableIsNativeTypeCheckedSbyF",
 "$sSa5countSivg",
 "$sSa8capacitySivg",
 "$sSa8endIndexSivg",
 "$sSa9formIndex6beforeySiz_tF",
 "$sSa034_makeUniqueAndReserveCapacityIfNotB0yyF",
 "$sSa15_checkSubscript_20wasNativeTypeCheckeds16_DependenceTokenVSi_SbtF",
 "$sSa11_getElement_20wasNativeTypeChecked22matchingSubscriptCheckxSi_Sbs16_DependenceTokenVtF",
 "$sSa12arrayLiteralSayxGxd_tcfC",
 "$sSa28_unsafeUninitializedCapacity16initializingWithSayxGSi_ySryxGz_SiztKXEtKcfC",
 "$sSa9removeAll15keepingCapacityySb_tF",
 "$sSa9removeAll15keepingCapacityySb_tFfA_",
 "$sSa19_uninitializedCountSayxGSi_tcfC",
 "$sSa36_reserveCapacityAssumingUniqueBuffer8oldCountySi_tF",
 "$sSa15reserveCapacityyySiF",
 "$sSa16_copyToNewBuffer8oldCountySi_tF",
 "$sSa9_getCountSiyF",
 "$sSa9formIndex5afterySiz_tF",
 "$sSayxSiciM",
 "$sSayxSicig",
 "$sSayxSicir",
 "$sSa37_appendElementAssumeUniqueAndCapacity_03newB0ySi_xntF",
 "$sSa6appendyyxnF",
 "$sSa9repeating5countSayxGx_SitcfC",
 "$sSa15replaceSubrange_4withySnySiG_qd__nt7ElementQyd__RszSlRd__lF",
 "$sSa42_withUnsafeMutableBufferPointerIfSupportedyqd__Sgqd__SryxGzKXEKlF",
 "$ss22_ContiguousArrayBufferV10startIndexSivg",
 "$ss22_ContiguousArrayBufferV19firstElementAddressSpyxGvg",
 "$ss22_ContiguousArrayBufferV7_buffer19shiftedToStartIndexAByxGAE_SitcfC",
 "$ss22_ContiguousArrayBufferV8endIndexSivg",
 "$ss22_ContiguousArrayBufferVAByxGycfC",
 "$ss22_ContiguousArrayBufferV13_copyContents8subRange12initializingSpyxGSnySiG_AFtF",
 "$ss22_ContiguousArrayBufferV18_initStorageHeader5count8capacityySi_SitF",
 "$ss22_ContiguousArrayBufferV5countSivg",
 "$ss12_ArrayBufferV013requestNativeB0s011_ContiguousaB0VyxGSgyF",
 "$ss12_ArrayBufferV10_nonNatives06_CocoaA7WrapperVvg",
 "$ss12_ArrayBufferV10startIndexSivg",
 "$ss12_ArrayBufferV19firstElementAddressSpyxGvg",
 "$ss12_ArrayBufferV20isUniquelyReferencedSbyF",
 "$ss12_ArrayBufferV5countSivs",
 "$ss12_ArrayBufferV7_natives011_ContiguousaB0VyxGvg",
 "$ss12_ArrayBufferV9_isNativeSbvg",
 "$ss12_ArrayBufferV8capacitySivg",
 "$ss12_ArrayBufferV8endIndexSivg",
 "$ss12_ArrayBufferV027requestUniqueMutableBackingB015minimumCapacitys011_ContiguousaB0VyxGSgSi_tF",
 "$ss12_ArrayBufferV19_getElementSlowPathyyXlSiF",
 "$ss12_ArrayBufferVyxSicig",
 "$ss12_ArrayBufferVyxSicir",
 "$ss12_ArrayBufferV10_typeCheckyySnySiGF",
 "$ss12_ArrayBufferV13_copyContents8subRange12initializingSpyxGSnySiG_AFtF",
 "$ss12_ArrayBufferV37_checkInoutAndNativeTypeCheckedBounds_03wasfgH0ySi_SbtF",
 "$ss12_ArrayBufferV7_buffer19shiftedToStartIndexAByxGs011_ContiguousaB0VyxG_SitcfC",
 "$sSn8containsySbxF",
 "$sSn15uncheckedBoundsSnyxGx5lower_x5uppert_tcfC",
 "$sSnsSxRzSZ6StrideRpzrlE10startIndexxvg",
 "$sSnsSxRzSZ6StrideRpzrlE8endIndexxvg",
 "$sSnsSxRzSZ6StrideRpzrlE5index5afterxx_tF",
 "$sSnsSxRzSZ6StrideRpzrlEyxxcir",
 "$sSN15uncheckedBoundsSNyxGx5lower_x5uppert_tcfC",
 "$sSNsSxRzSZ6StrideRpzrlE10startIndexSNsSxRzSZABRQrlE0C0Oyx_Gvg",
 "$sSNsSxRzSZ6StrideRpzrlE8endIndexSNsSxRzSZABRQrlE0C0Oyx_Gvg",
 "$sSNsSxRzSZ6StrideRpzrlEyxSNsSxRzSZABRQrlE5IndexOyx_Gcir",
 "$sSNsSxRzSZ6StrideRpzrlE5index5afterSNsSxRzSZABRQrlE5IndexOyx_GAG_tF",
 "$ss16IndexingIteratorV4next7ElementQzSgyF",
  nullptr
};

#if defined(__clang__)
_Pragma("clang diagnostic pop")
#endif

// Is this symbol a candidate to check that we have a specialization for.
static bool isKnownUnspecialized(StringRef unspecialized) {
  if (KnownUnspecialized.empty()) {
    const char **ptr = &UnspecializedPrespecializedFuncs[0];
    while (const char *name = *ptr++)
      KnownUnspecialized.insert(name);
  }
  return KnownUnspecialized.count(unspecialized) != 0;
}

// Analyze the function and replace each apply of
// a generic function by an apply of the corresponding
// pre-specialized function, if such a pre-specialization exists.
bool UsePrespecialized::replaceByPrespecialized(SILFunction &F) {
  bool Changed = false;
  auto &M = F.getModule();

  llvm::SmallVector<ApplySite, 16> NewApplies;
  collectApplyInst(F, NewApplies);

  for (auto &AI : NewApplies) {
    auto *ReferencedF = AI.getReferencedFunctionOrNull();
    if (!ReferencedF)
      continue;

    LLVM_DEBUG(llvm::dbgs() << "Trying to use specialized function for:\n";
               AI.getInstruction()->dumpInContext());

    // Check if it is a call of a generic function.
    // If this is the case, check if there is a specialization
    // available for it already and use this specialization
    // instead of the generic version.
    if (!AI.hasSubstitutions())
      continue;

    SubstitutionMap Subs = AI.getSubstitutionMap();

    // Bail if the replacement types depend on the callee's generic
    // environment.
    //
    // TODO: Remove this limitation once public partial specializations
    // are supported and can be provided by other modules.
    if (Subs.getRecursiveProperties().hasArchetype())
      continue;

    // No point in building and checking the specialization if we know that we
    // don't have a specialized version of the unspecialized symbol.
    if (!isKnownUnspecialized(ReferencedF->getName()))
      continue;

    ReabstractionInfo ReInfo(M.getSwiftModule(), M.isWholeModule(), AI,
                             ReferencedF, Subs, IsNotSerialized,
                             /*ConvertIndirectToDirect=*/ true,
                             /*dropUnusedArguments=*/ false);

    if (!ReInfo.canBeSpecialized())
      continue;

    auto SpecType = ReInfo.getSpecializedType();
    // Bail if any generic types parameters of the concrete type
    // are unbound.
    if (SpecType->hasArchetype())
      continue;

    // Create a name of the specialization. All external pre-specializations
    // are serialized without bodies. Thus use IsNotSerialized here.
    Mangle::GenericSpecializationMangler NewGenericMangler(M.getASTContext(), ReferencedF,
                                                           IsNotSerialized);
    std::string ClonedName = NewGenericMangler.mangleReabstracted(Subs,
       ReInfo.needAlternativeMangling());
      
    SILFunction *NewF = nullptr;
    // If we already have this specialization, reuse it.
    auto PrevF = M.lookUpFunction(ClonedName);
    if (PrevF) {
      LLVM_DEBUG(llvm::dbgs() << "Found a specialization: " << ClonedName
                              << "\n");
      NewF = PrevF;
    }

    if (!PrevF || !NewF) {
      // Check for the existence of this function in another module without
      // loading the function body.
      PrevF = lookupPrespecializedSymbol(M, ClonedName);
      LLVM_DEBUG(llvm::dbgs() << "Checked if there is a specialization in a "
                                 "different module: "
                              << PrevF << "\n");
      if (!PrevF)
        continue;
      assert(PrevF->isExternalDeclaration() &&
             "Prespecialized function should be an external declaration");
      NewF = PrevF;
    }

    if (!NewF)
      continue;

    // An existing specialization was found.
    LLVM_DEBUG(llvm::dbgs() << "Found a specialization of "
                            << ReferencedF->getName()
                            << " : " << NewF->getName() << "\n");

    auto NewAI = replaceWithSpecializedFunction(AI, NewF, ReInfo);
    switch (AI.getKind()) {
    case ApplySiteKind::ApplyInst:
      cast<ApplyInst>(AI)->replaceAllUsesWith(cast<ApplyInst>(NewAI));
      break;
    case ApplySiteKind::PartialApplyInst:
      cast<PartialApplyInst>(AI)->replaceAllUsesWith(
          cast<PartialApplyInst>(NewAI));
      break;
    case ApplySiteKind::TryApplyInst:
    case ApplySiteKind::BeginApplyInst:
      break;
    }
    recursivelyDeleteTriviallyDeadInstructions(AI.getInstruction(), true);
    Changed = true;
  }

  return Changed;
}


SILTransform *swift::createUsePrespecialized() {
  return new UsePrespecialized();
}
