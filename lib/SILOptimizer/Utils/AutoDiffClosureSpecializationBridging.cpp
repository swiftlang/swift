//===--- AutoDiffClosureSpecializationBridging.cpp ------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "autodiff-closure-specialization-bridging"

#include "swift/SILOptimizer/AutoDiffClosureSpecializationBridging.h"
#include "swift/AST/ParameterList.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/Differentiation/ADContext.h"
#include "swift/SILOptimizer/Differentiation/LinearMapInfo.h"
#include "llvm/ADT/DenseMapInfo.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallVector.h"

using namespace swift;

unsigned BridgedTypeHasher::operator()(const BridgedType &value) const {
  return llvm::DenseMapInfo<void *>::getHashValue(value.opaqueValue);
}

bool operator==(const BridgedType &lhs, const BridgedType &rhs) {
  return lhs.opaqueValue == rhs.opaqueValue;
}

ClosureAndIdxInPayload::ClosureAndIdxInPayload(BridgedInstruction closure,
                                               SwiftInt idxInPayload)
    : closure(closure), idxInPayload(idxInPayload) {}

static SILType getBranchingTraceEnumLoweredType(EnumDecl *ed,
                                                SILFunction &vjp) {
  return autodiff::getLoweredTypeImpl(
      ed->getDeclaredInterfaceType()->getCanonicalType(), &vjp,
      vjp.getModule().Types);
}

static Type getCapturedArgTypesTupleForClosure(const SILInstruction *closure,
                                               ASTContext &ctx) {
  llvm::SmallVector<TupleTypeElt, 8> capturedArgTypes;

  if (const auto *pai = dyn_cast<PartialApplyInst>(closure)) {
    capturedArgTypes.reserve(pai->getArguments().size());
    for (const SILValue &arg : pai->getArguments())
      capturedArgTypes.emplace_back(arg->getType().getASTType(), Identifier{});
  } else {
    assert(isa<ThinToThickFunctionInst>(closure));
  }

  return TupleType::get(capturedArgTypes, ctx);
}

// NOTE: this is adopted from
// lib/SILOptimizer/Differentiation/PullbackCloner.cpp.
/// Remap any archetypes into the current function's context.
static SILType remapType(SILType ty, SILFunction &f) {
  if (ty.hasArchetype())
    ty = ty.mapTypeOutOfContext();
  auto remappedType = ty.getASTType()->getReducedType(
      f.getLoweredFunctionType()->getSubstGenericSignature());
  auto remappedSILType =
      SILType::getPrimitiveType(remappedType, ty.getCategory());
  if (f.getGenericEnvironment())
    return f.mapTypeIntoContext(remappedSILType);
  return remappedSILType;
}

static llvm::SmallVector<Type, 8> getBranchTracingEnumPreds(Type bteType) {
  llvm::SmallVector<Type, 8> btePreds;
  EnumDecl *ed = bteType->getEnumOrBoundGenericEnum();
  for (EnumCaseDecl *ecd : ed->getAllCases()) {
    assert(ecd->getElements().size() == 1);
    EnumElementDecl *oldEED = ecd->getElements().front();

    assert(oldEED->getParameterList()->size() == 1);
    ParamDecl &oldParamDecl = *oldEED->getParameterList()->front();

    auto *tt = cast<TupleType>(oldParamDecl.getInterfaceType().getPointer());

    if (tt->getNumElements() > 0 && !tt->getElement(0).getName().empty()) {
      assert(tt->getElement(0).getName().is("predecessor"));
      btePreds.emplace_back(tt->getElement(0).getType());
    }
  }
  return btePreds;
}

static void iterateOverBranchTracingEnumPreds(
    llvm::DenseMap<Type, llvm::SmallVector<Type, 8>> &bteToPredsDict,
    const Type &currentBTEType) {
  assert(currentBTEType->isCanonical());
  llvm::SmallVector<Type, 8> currentBTEPreds =
      getBranchTracingEnumPreds(currentBTEType);
  bteToPredsDict[currentBTEType] = currentBTEPreds;
  for (const Type &currentBTEPred : currentBTEPreds) {
    if (!bteToPredsDict.contains(currentBTEPred)) {
      iterateOverBranchTracingEnumPreds(bteToPredsDict, currentBTEPred);
    }
  }
}

static llvm::SmallVector<Type, 8>
getBranchTracingEnumQueue(BridgedType topBTEType) {
  llvm::DenseMap<Type, llvm::SmallVector<Type, 8>> bteToPredsDict;
  iterateOverBranchTracingEnumPreds(bteToPredsDict,
                                    topBTEType.unbridged().getASTType());

  llvm::SmallVector<Type, 8> bteQueue;
  std::size_t totalEnums = bteToPredsDict.size();
  bteQueue.reserve(totalEnums);
  for (std::size_t i = 0; i < totalEnums; ++i) {
    for (const auto &[bteType, btePreds] : bteToPredsDict) {
      if (!btePreds.empty())
        continue;
      TypeBase *bteTypePointer = bteType.getPointer();
      assert(llvm::find_if(bteQueue, [bteTypePointer](const Type &type) {
               return bteTypePointer == type.getPointer();
             }) == bteQueue.end());
      bteQueue.emplace_back(bteType);
      break;
    }
    assert(bteQueue.size() == i + 1);
    bteToPredsDict.erase(bteQueue.back());
    for (auto &[bteType, btePreds] : bteToPredsDict) {
      llvm::erase_if(btePreds, [&bteQueue](const Type &type) {
        return bteQueue.back().getPointer() == type.getPointer();
      });
    }
  }
  assert(bteQueue.size() == totalEnums);

  return bteQueue;
}

struct EnumTypeAndCaseIdx {
  BridgedType enumType;
  SwiftInt caseIdx;
};

static bool operator==(const EnumTypeAndCaseIdx &lhs,
                       const EnumTypeAndCaseIdx &rhs) {
  return lhs.enumType == rhs.enumType && lhs.caseIdx == rhs.caseIdx;
}

struct EnumTypeAndCaseIdxHasher {
  unsigned operator()(const EnumTypeAndCaseIdx &value) const {
    return llvm::DenseMapInfo<std::pair<void *, SwiftInt>>::getHashValue(
        std::pair<void *, SwiftInt>(value.enumType.opaqueValue, value.caseIdx));
  }
};

using BTECaseToClosureListDict =
    std::unordered_map<EnumTypeAndCaseIdx,
                       llvm::SmallVector<ClosureAndIdxInPayload, 8>,
                       EnumTypeAndCaseIdxHasher>;

// NOTE: Branch tracing enum creation logic was adopted from
// LinearMapInfo::createBranchingTraceDecl.
static BridgedType autodiffSpecializeBranchTracingEnum(
    BridgedType bteType, SILFunction &topVJP,
    const BTECaseToClosureListDict &bteCaseToClosureListDict,
    const SpecializedBranchTracingEnumDict &specBTEDict) {
  EnumDecl *oldED = bteType.unbridged().getEnumOrBoundGenericEnum();
  assert(oldED && "Expected valid enum type");
  // TODO: switch to contains() after transition to C++20
  assert(specBTEDict.find(bteType) == specBTEDict.end());

  SILModule &silModule = topVJP.getModule();
  ASTContext &astContext = oldED->getASTContext();

  std::string newEDNameStr = oldED->getNameStr().str() + "_spec";
  llvm::SmallVector<ParameterList *, 8> newPLs;

  for (EnumCaseDecl *oldECD : oldED->getAllCases()) {
    assert(oldECD->getElements().size() == 1);
    EnumElementDecl *oldEED = oldECD->getElements().front();
    unsigned caseIdx = silModule.getCaseIndex(oldEED);

    static llvm::SmallVector<ClosureAndIdxInPayload, 8> emptyClosureAndIdxList;
    auto closureAndIdxListIt =
        bteCaseToClosureListDict.find(EnumTypeAndCaseIdx{bteType, caseIdx});
    const llvm::SmallVector<ClosureAndIdxInPayload, 8> &closureAndIdxList =
        (closureAndIdxListIt == bteCaseToClosureListDict.end()
             ? emptyClosureAndIdxList
             : closureAndIdxListIt->second);

    assert(oldEED->getParameterList()->size() == 1);
    ParamDecl &oldParamDecl = *oldEED->getParameterList()->front();

    const auto *oldPayloadTupleType =
        cast<TupleType>(oldParamDecl.getInterfaceType().getPointer());
    llvm::SmallVector<TupleTypeElt, 8> newPayloadTupleElementTypes;
    newPayloadTupleElementTypes.reserve(oldPayloadTupleType->getNumElements());

    std::string newECDNameSuffix;

    for (unsigned idxInPayloadTuple = 0;
         idxInPayloadTuple < oldPayloadTupleType->getNumElements();
         ++idxInPayloadTuple) {
      Identifier label =
          oldPayloadTupleType->getElement(idxInPayloadTuple).getName();

      auto closureAndIdxInPayloadIt = llvm::find_if(
          closureAndIdxList,
          [idxInPayloadTuple](
              const ClosureAndIdxInPayload &closureAndIdxInPayload) {
            return closureAndIdxInPayload.idxInPayload == idxInPayloadTuple;
          });

      Type newPayloadTupleEltType;
      if (closureAndIdxInPayloadIt != closureAndIdxList.end()) {
        newECDNameSuffix += '_' + std::to_string(idxInPayloadTuple);
        newPayloadTupleEltType = getCapturedArgTypesTupleForClosure(
            closureAndIdxInPayloadIt->closure.unbridged(), astContext);
      } else {
        newPayloadTupleEltType =
            oldPayloadTupleType->getElementType(idxInPayloadTuple);
        if (idxInPayloadTuple == 0) {
          if (EnumDecl *predED =
                  newPayloadTupleEltType->getEnumOrBoundGenericEnum()) {
            assert(label.str() == "predecessor");
            SILType predBTEType = remapType(
                getBranchingTraceEnumLoweredType(predED, topVJP), topVJP);
            newPayloadTupleEltType =
                specBTEDict.at(predBTEType).unbridged().getASTType();
          }
        }
      }

      newPayloadTupleElementTypes.emplace_back(newPayloadTupleEltType, label);
    }

    Type newTupleType = TupleType::get(newPayloadTupleElementTypes, astContext)
                            ->mapTypeOutOfContext();
    auto *newParamDecl = ParamDecl::cloneWithoutType(astContext, &oldParamDecl);
    newParamDecl->setInterfaceType(newTupleType);
    newPLs.emplace_back(ParameterList::create(astContext, {newParamDecl}));

    if (!newECDNameSuffix.empty())
      newEDNameStr += '_' + oldEED->getNameStr().str() + newECDNameSuffix;
  }

  CanGenericSignature genericSig = nullptr;
  if (auto *derivativeFnGenEnv = topVJP.getGenericEnvironment())
    genericSig =
        derivativeFnGenEnv->getGenericSignature().getCanonicalSignature();
  GenericParamList *genericParams = nullptr;
  if (genericSig)
    genericParams = autodiff::cloneGenericParameters(
        astContext, oldED->getDeclContext(), genericSig);

  Identifier newEDName = astContext.getIdentifier(newEDNameStr);

  auto *newED = new (astContext) EnumDecl(
      /*EnumLoc*/ SourceLoc(), /*Name*/ newEDName, /*NameLoc*/ SourceLoc(),
      /*Inherited*/ {}, /*GenericParams*/ genericParams,
      /*DC*/ oldED->getDeclContext());
  newED->setImplicit();
  if (genericSig)
    newED->setGenericSignature(genericSig);

  for (auto [idx, oldECD] : llvm::enumerate(oldED->getAllCases())) {
    EnumElementDecl *oldEED = oldECD->getElements().front();
    auto *newPL = newPLs[idx];
    auto *newEED = new (astContext) EnumElementDecl(
        /*IdentifierLoc*/ SourceLoc(),
        DeclName(astContext.getIdentifier(oldEED->getNameStr())), newPL,
        SourceLoc(), /*RawValueExpr*/ nullptr, newED);
    newEED->setImplicit();
    newED->addMember(newEED);
  }

  // TODO: Copying access level from oldED causes crashes for some reason.
  //       Use Public unconditionally as for now.
  // TODO: Do we need to copy FrozenAttr and UsableFromInlineAttr from oldED?
  newED->setAccess(AccessLevel::Public);

  auto &file = autodiff::getSourceFile(&topVJP).getOrCreateSynthesizedFile();
  file.addTopLevelDecl(newED);
  file.getParentModule()->clearLookupCache();

  SILType newEnumType =
      remapType(getBranchingTraceEnumLoweredType(newED, topVJP), topVJP);

  return newEnumType;
}

SpecializedBranchTracingEnumDict autodiffSpecializeBranchTracingEnums(
    BridgedFunction topVJP, BridgedType topBTE,
    const VectorOfBranchTracingEnumAndClosureInfo
        &vectorOfBranchTracingEnumAndClosureInfo) {
  SILFunction &silTopVJP = *topVJP.getFunction();

  BTECaseToClosureListDict bteCaseToClosureListDict;

  for (const BranchTracingEnumAndClosureInfo &elem :
       vectorOfBranchTracingEnumAndClosureInfo) {
    bteCaseToClosureListDict[EnumTypeAndCaseIdx{elem.enumType,
                                                elem.enumCaseIdx}]
        .emplace_back(elem.closure, elem.idxInPayload);
  }

  llvm::SmallVector<Type, 8> bteQueue = getBranchTracingEnumQueue(topBTE);
  SpecializedBranchTracingEnumDict dict;

  for (const Type &t : bteQueue) {
    EnumDecl *ed = t->getEnumOrBoundGenericEnum();

    SILType silType =
        remapType(getBranchingTraceEnumLoweredType(ed, silTopVJP), silTopVJP);

    dict[BridgedType(silType)] = autodiffSpecializeBranchTracingEnum(
        BridgedType(silType), silTopVJP, bteCaseToClosureListDict, dict);
  }

  return dict;
}

BridgedArgument specializeBranchTracingEnumBBArgInVJP(
    BridgedArgument arg, const SpecializedBranchTracingEnumDict &specBTEDict) {
  ValueOwnershipKind oldOwnership = arg.getArgument()->getOwnershipKind();

  SILArgument *oldArg = arg.getArgument();
  SILBasicBlock *bb = oldArg->getParentBlock();
  assert(!bb->isEntry());
  unsigned index = oldArg->getIndex();
  // TODO: switch to contains() after transition to C++20
  assert(specBTEDict.find(oldArg->getType()) != specBTEDict.end());
  SILType type = specBTEDict.at(BridgedType(oldArg->getType())).unbridged();
  SILPhiArgument *newArg = bb->insertPhiArgument(index, type, oldOwnership);
  return {newArg};
}

BridgedArgument specializePayloadTupleBBArgInPullback(BridgedArgument arg,
                                                      BridgedType enumType,
                                                      SwiftInt caseIdx) {
  SILArgument *oldArg = arg.getArgument();
  unsigned argIdx = oldArg->getIndex();
  SILBasicBlock *bb = oldArg->getParentBlock();
  assert(!bb->isEntry());
  SILModule &silModule = bb->getModule();

  SILType newEnumType = enumType.unbridged();
  EnumDecl *newED = newEnumType.getEnumOrBoundGenericEnum();
  assert(newED != nullptr);

  CanType newPayloadTupleTy;
  for (EnumElementDecl *newEED : newED->getAllElements()) {
    unsigned currentCaseIdx = silModule.getCaseIndex(newEED);
    if (currentCaseIdx != caseIdx)
      continue;

    newPayloadTupleTy = newEED->getPayloadInterfaceType()->getCanonicalType();
  }

  assert(newPayloadTupleTy != CanType());

  SILType newPayloadTupleSILTy =
      remapType(autodiff::getLoweredTypeImpl(
                    newPayloadTupleTy, bb->getFunction(), silModule.Types),
                *bb->getFunction());

  ValueOwnershipKind oldOwnership = bb->getArgument(argIdx)->getOwnershipKind();

  SILPhiArgument *newArg =
      bb->insertPhiArgument(argIdx, newPayloadTupleSILTy, oldOwnership);

  return {newArg};
}

BridgedOwnedString getSpecializedBranchTracingEnumDictAsString(
    const SpecializedBranchTracingEnumDict &specBTEDict) {
  llvm::SmallVector<BridgedType, 8> keys;
  keys.reserve(specBTEDict.size());
  for (const auto &[key, _] : specBTEDict)
    keys.emplace_back(key);
  llvm::sort(keys, [](const BridgedType &lhs, const BridgedType &rhs) {
    return lhs.unbridged().getAsString() < rhs.unbridged().getAsString();
  });

  std::string str;
  llvm::raw_string_ostream out(str);
  for (const auto &[idx, key] : llvm::enumerate(keys)) {
    out << "non-specialized BTE " << idx << ": ";
    key.unbridged().getEnumOrBoundGenericEnum()->print(out);
    out << '\n';
    out << "specialized BTE " << idx << ": ";
    specBTEDict.at(key).unbridged().getEnumOrBoundGenericEnum()->print(out);
    out << '\n';
  }

  return BridgedOwnedString(/*stringToCopy=*/StringRef(str));
}
