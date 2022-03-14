//===--- CaptureInfo.cpp - Data Structure for Capture Lists ---------------===//
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

#include "swift/AST/CaptureInfo.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/GenericEnvironment.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

CaptureInfo::CaptureInfo(ASTContext &ctx, ArrayRef<CapturedValue> captures,
                         ArrayRef<GenericEnvironment *> openedExistentials,
                         DynamicSelfType *dynamicSelf,
                         OpaqueValueExpr *opaqueValue,
                         bool genericParamCaptures) {
  static_assert(IsTriviallyDestructible<CapturedValue>::value,
                "Capture info is alloc'd on the ASTContext and not destroyed");
  static_assert(IsTriviallyDestructible<CaptureInfo::CaptureInfoStorage>::value,
                "Capture info is alloc'd on the ASTContext and not destroyed");

  OptionSet<Flags> flags;
  if (genericParamCaptures)
    flags |= Flags::HasGenericParamCaptures;

  if (captures.empty() && openedExistentials.empty() &&
      !dynamicSelf && !opaqueValue) {
    *this = CaptureInfo::empty();
    StorageAndFlags.setInt(flags);
    return;
  }

  size_t storageToAlloc =
      CaptureInfoStorage::totalSizeToAlloc<CapturedValue, GenericEnvironment *>(
        captures.size(), openedExistentials.size());
  void *storageBuf = ctx.Allocate(storageToAlloc, alignof(CaptureInfoStorage));
  auto *storage = new (storageBuf) CaptureInfoStorage(captures.size(),
                                                      openedExistentials.size(),
                                                      dynamicSelf,
                                                      opaqueValue);
  StorageAndFlags.setPointerAndInt(storage, flags);
  std::uninitialized_copy(captures.begin(), captures.end(),
                          storage->getTrailingObjects<CapturedValue>());
  std::uninitialized_copy(openedExistentials.begin(), openedExistentials.end(),
                          storage->getTrailingObjects<GenericEnvironment *>());
}

CaptureInfo CaptureInfo::empty() {
  static const CaptureInfoStorage empty{0, 0, /*dynamicSelf*/nullptr,
                                        /*opaqueValue*/nullptr};
  CaptureInfo result;
  result.StorageAndFlags.setPointer(&empty);
  return result;
}

bool CaptureInfo::hasLocalCaptures() const {
  for (auto capture : getCaptures())
    if (capture.getDecl()->isLocalCapture())
      return true;
  return false;
}


void CaptureInfo::
getLocalCaptures(SmallVectorImpl<CapturedValue> &Result) const {
  if (!hasLocalCaptures()) return;

  Result.reserve(getCaptures().size());

  // Filter out global variables.
  for (auto capture : getCaptures()) {
    if (!capture.getDecl()->isLocalCapture())
      continue;

    Result.push_back(capture);
  }
}

VarDecl *CaptureInfo::getIsolatedParamCapture() const {
  if (!hasLocalCaptures())
    return nullptr;

  for (const auto &capture : getCaptures()) {
    if (!capture.getDecl()->isLocalCapture())
      continue;

    if (capture.isDynamicSelfMetadata())
      continue;

    // If we captured an isolated parameter, return it.
    if (auto param = dyn_cast_or_null<ParamDecl>(capture.getDecl())) {
      // If we have captured an isolated parameter, return it.
      if (param->isIsolated())
        return param;
    }

    // If we captured 'self', check whether it is (still) isolated.
    if (auto var = dyn_cast_or_null<VarDecl>(capture.getDecl())) {
      if (var->isSelfParamCapture() && var->isSelfParamCaptureIsolated())
        return var;
    }
  }

  return nullptr;
}

GenericSignature CaptureInfo::getEffectiveGenericSignature(
    DeclContext *dc,
    llvm::SmallDenseMap<GenericTypeParamType *, Type> *openedExistentialMap
) const {
  // If this isn't a local function/closure, there is never an adjustment.
  if (!dc->getParent()->isLocalContext())
    return dc->getGenericSignatureOfContext();

  // If there are no opened existentials, it's either the context's generic
  // signature or nothing.
  if (getOpenedExistentials().empty()) {
    // If the generic parameters don't need to be captured, there is no generic
    // signature at all.
    if (!hasGenericParamCaptures())
      return nullptr;

    return dc->getGenericSignatureOfContext();
  }

  // When there are opened existentials, create new generic parameters
  // for each of the parameters in the opened existentials.

  // Determine the depth at which the synthesized generic parameters should
  // be.
  unsigned depth = hasGenericParamCaptures() ? dc->getGenericContextDepth() + 1
                                             : 0;
  SmallVector<GenericTypeParamType *, 2> openedGenericParams;
  SmallVector<Requirement, 2> openedRequirements;
  ASTContext &ctx = dc->getASTContext();
  for (auto opened : getOpenedExistentials()) {
    unsigned firstDestIndex = openedGenericParams.size();

    // Clone the generic parameters from this opened existential into
    // the new signature.
    auto sourceParams =
        opened->getGenericSignature().getInnermostGenericParams();
    unsigned destIndex = firstDestIndex;
    llvm::SmallDenseMap<GenericTypeParamType *, Type> genericParamMap;
    for (auto genericParam : sourceParams) {
      auto newGenericParam = GenericTypeParamType::get(
          genericParam->isTypeSequence(), depth, destIndex++, ctx);
      openedGenericParams.push_back(newGenericParam);
      genericParamMap[genericParam] = Type(newGenericParam);

      // Record the mapping from the new generic parameter to the corresponding
      // opened existential archetype.
      if (openedExistentialMap) {
        (*openedExistentialMap)[newGenericParam] =
            opened->mapTypeIntoContext(genericParam);
      }
    }

    // Clone the generic requirements, substituting in the new generic
    // parameters for the old.
    for (const auto &req : opened->getGenericSignature().getRequirements()) {
      auto newReq = req.subst(
        [&](SubstitutableType *dependentType) {
          auto genericParam = dyn_cast<GenericTypeParamType>(dependentType);
          if (!genericParam)
            return Type(dependentType);

          auto remapped = genericParamMap.find(genericParam);
          if (remapped == genericParamMap.end())
            return Type(dependentType);

          return remapped->second;
        },
        MakeAbstractConformanceForGenericType());

      if (newReq)
        openedRequirements.push_back(*newReq);
    }
  }

  // If this captures generic parameters, bring in the base signature.
  GenericSignature baseSignature;
  if (hasGenericParamCaptures())
    baseSignature = dc->getGenericSignatureOfContext();

  return buildGenericSignature(
      ctx, baseSignature, openedGenericParams, openedRequirements);
}

SubstitutionMap CaptureInfo::getEffectiveSubstitutionMap(
    DeclContext *dc, SubstitutionMap dcSubs) {
  // If this isn't a local function/closure, there is never an adjustment.
  if (!dc->getParent()->isLocalContext())
    return dcSubs;

  // If there are no opened existentials, it's either the context
  // substitutions or an empty substitution map.
  if (getOpenedExistentials().empty()) {
    // If the generic parameters don't need to be captured, there are no
    // substitutions to perform.
    if (!hasGenericParamCaptures())
      return SubstitutionMap();

    return dcSubs;
  }

  // Produce substitutions mapping the newly-introduced generic parameters
  // to their corresponding opened existential types.

  // Retrieve the effective generic signature, which includes the additional
  // generic parameters.
  llvm::SmallDenseMap<GenericTypeParamType *, Type> openedExistentialMap;
  auto genericSig = getEffectiveGenericSignature(dc, &openedExistentialMap);

  return SubstitutionMap::get(
      genericSig,
      [&](SubstitutableType *dependentType) {
        if (auto genericParam = dyn_cast<GenericTypeParamType>(dependentType)) {
          auto opened = openedExistentialMap.find(genericParam);
          if (opened != openedExistentialMap.end())
            return opened->second;
        }

        return QuerySubstitutionMap{dcSubs}(dependentType);
      },
      LookUpConformanceInSubstitutionMap(dcSubs));
}

LLVM_ATTRIBUTE_USED void CaptureInfo::dump() const {
  print(llvm::errs());
  llvm::errs() << '\n';
}

void CaptureInfo::print(raw_ostream &OS) const {
  OS << "captures=(";

  if (hasGenericParamCaptures())
    OS << "<generic> ";
  if (hasDynamicSelfCapture())
    OS << "<dynamic_self> ";
  if (hasOpaqueValueCapture())
    OS << "<opaque_value> ";

  interleave(getCaptures(),
             [&](const CapturedValue &capture) {
               OS << capture.getDecl()->getBaseName();

               if (capture.isDirect())
                 OS << "<direct>";
               if (capture.isNoEscape())
                 OS << "<noescape>";
             },
             [&] { OS << ", "; });

  interleave(getOpenedExistentials(),
             [&](GenericEnvironment *env) {
               OS << env->getOpenedExistentialType() << " @ "
                  << env->getOpenedExistentialUUID();
             },
             [&] { OS << ", "; });

  OS << ')';
}

