//===--- LinearMapInfo.cpp ------------------------------------*- C++ -*---===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// SWIFT_ENABLE_TENSORFLOW
//
// Linear map struct and branching trace enum information for differentation.
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "differentiation"

#include "swift/SILOptimizer/Utils/Differentiation/LinearMapInfo.h"
#include "swift/AST/DeclContext.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/SourceFile.h"
#include "swift/SIL/LoopInfo.h"
#include "swift/SILOptimizer/Analysis/LoopAnalysis.h"
#include "swift/SILOptimizer/Utils/Differentiation/ADContext.h"

namespace swift {
namespace autodiff {

//===----------------------------------------------------------------------===//
// Local helpers
//===----------------------------------------------------------------------===//

/// Clone the generic parameters of the given generic signature and return a new
/// `GenericParamList`.
static GenericParamList *cloneGenericParameters(ASTContext &ctx,
                                                DeclContext *dc,
                                                CanGenericSignature sig) {
  SmallVector<GenericTypeParamDecl *, 2> clonedParams;
  for (auto paramType : sig->getGenericParams()) {
    auto clonedParam = new (ctx)
        GenericTypeParamDecl(dc, paramType->getName(), SourceLoc(),
                             paramType->getDepth(), paramType->getIndex());
    clonedParam->setDeclContext(dc);
    clonedParam->setImplicit(true);
    clonedParams.push_back(clonedParam);
  }
  return GenericParamList::create(ctx, SourceLoc(), clonedParams, SourceLoc());
}

//===----------------------------------------------------------------------===//
// LinearMapInfo methods
//===----------------------------------------------------------------------===//

LinearMapInfo::LinearMapInfo(ADContext &context, AutoDiffLinearMapKind kind,
                             SILFunction *original, SILFunction *derivative,
                             SILAutoDiffIndices indices,
                             const DifferentiableActivityInfo &activityInfo)
    : kind(kind), original(original), derivative(derivative),
      activityInfo(activityInfo), indices(indices),
      typeConverter(context.getTypeConverter()) {
  generateDifferentiationDataStructures(context, indices, derivative);
}

SILType LinearMapInfo::remapTypeInDerivative(SILType ty) {
  if (ty.hasArchetype())
    return derivative->mapTypeIntoContext(ty.mapTypeOutOfContext());
  return derivative->mapTypeIntoContext(ty);
}

VarDecl *LinearMapInfo::addVarDecl(NominalTypeDecl *nominal, StringRef name,
                                   Type type) {
  auto &astCtx = nominal->getASTContext();
  auto id = astCtx.getIdentifier(name);
  auto *varDecl = new (astCtx) VarDecl(
      /*IsStatic*/ false, VarDecl::Introducer::Var, /*IsCaptureList*/ false,
      SourceLoc(), id, nominal);
  varDecl->setAccess(nominal->getEffectiveAccess());
  if (type->hasArchetype())
    varDecl->setInterfaceType(type->mapTypeOutOfContext());
  else
    varDecl->setInterfaceType(type);
  nominal->addMember(varDecl);
  return varDecl;
}

SourceFile &LinearMapInfo::getDeclarationFileUnit() {
  if (original->hasLocation())
    if (auto *declContext = original->getLocation().getAsDeclContext())
      if (auto *parentSourceFile = declContext->getParentSourceFile())
        return *parentSourceFile;
  for (auto *file : original->getModule().getSwiftModule()->getFiles())
    if (auto *src = dyn_cast<SourceFile>(file))
      return *src;
  llvm_unreachable("No files?");
}

void LinearMapInfo::computeAccessLevel(NominalTypeDecl *nominal,
                                       SILLinkage originalLinkage) {
  auto &astCtx = nominal->getASTContext();
  switch (originalLinkage) {
  case swift::SILLinkage::Public:
  case swift::SILLinkage::PublicNonABI:
    nominal->setAccess(AccessLevel::Internal);
    nominal->getAttrs().add(new (astCtx)
                                UsableFromInlineAttr(/*Implicit*/ true));
    break;
  case swift::SILLinkage::Hidden:
  case swift::SILLinkage::Shared:
    nominal->setAccess(AccessLevel::Internal);
    break;
  case swift::SILLinkage::Private:
    nominal->setAccess(AccessLevel::FilePrivate);
    break;
  default:
    // When the original function has external linkage, we create an internal
    // struct for use by our own module. This is necessary for cross-cell
    // differentiation in Jupyter.
    // TODO: Add a test in the compiler that exercises a similar situation as
    // cross-cell differentiation in Jupyter.
    nominal->setAccess(AccessLevel::Internal);
  }
}

EnumDecl *LinearMapInfo::createBranchingTraceDecl(
    SILBasicBlock *originalBB, SILAutoDiffIndices indices,
    CanGenericSignature genericSig, SILLoopInfo *loopInfo) {
  assert(originalBB->getParent() == original);
  auto &astCtx = original->getASTContext();
  auto *moduleDecl = original->getModule().getSwiftModule();
  auto &file = getDeclarationFileUnit();
  // Create a branching trace enum.
  std::string enumName;
  switch (kind) {
  case AutoDiffLinearMapKind::Differential:
    enumName = "_AD__" + original->getName().str() + "_bb" +
               std::to_string(originalBB->getDebugID()) + "__Succ__" +
               indices.mangle();
    break;
  case AutoDiffLinearMapKind::Pullback:
    enumName = "_AD__" + original->getName().str() + "_bb" +
               std::to_string(originalBB->getDebugID()) + "__Pred__" +
               indices.mangle();
    break;
  }
  auto enumId = astCtx.getIdentifier(enumName);
  auto loc = original->getLocation().getSourceLoc();
  GenericParamList *genericParams = nullptr;
  if (genericSig)
    genericParams = cloneGenericParameters(astCtx, &file, genericSig);
  auto *branchingTraceDecl = new (astCtx) EnumDecl(
      /*EnumLoc*/ SourceLoc(), /*Name*/ enumId, /*NameLoc*/ loc,
      /*Inherited*/ {}, /*GenericParams*/ genericParams, /*DC*/ &file);
  // Note: must mark enum as implicit to satisfy assertion in
  // `Parser::parseDeclListDelayed`.
  branchingTraceDecl->setImplicit();
  if (genericSig)
    branchingTraceDecl->setGenericSignature(genericSig);
  computeAccessLevel(branchingTraceDecl, original->getEffectiveSymbolLinkage());
  branchingTraceDecl->getInterfaceType();
  assert(branchingTraceDecl->hasInterfaceType());
  file.addVisibleDecl(branchingTraceDecl);
  // Add basic block enum cases.
  for (auto *predBB : originalBB->getPredecessorBlocks()) {
    auto bbId = "bb" + std::to_string(predBB->getDebugID());
    auto *linearMapStruct = getLinearMapStruct(predBB);
    assert(linearMapStruct);
    auto linearMapStructTy =
        linearMapStruct->getDeclaredInterfaceType()->getCanonicalType();
    // Create dummy declaration representing enum case parameter.
    auto *decl = new (astCtx)
        ParamDecl(loc, loc, Identifier(), loc, Identifier(), moduleDecl);
    decl->setSpecifier(ParamDecl::Specifier::Default);
    if (linearMapStructTy->hasArchetype())
      decl->setInterfaceType(linearMapStructTy->mapTypeOutOfContext());
    else
      decl->setInterfaceType(linearMapStructTy);
    // Create enum element and enum case declarations.
    auto *paramList = ParameterList::create(astCtx, {decl});
    auto *enumEltDecl = new (astCtx) EnumElementDecl(
        /*IdentifierLoc*/ loc, DeclName(astCtx.getIdentifier(bbId)), paramList,
        loc, /*RawValueExpr*/ nullptr, branchingTraceDecl);
    enumEltDecl->setImplicit();
    enumEltDecl->getInterfaceType();
    auto *enumCaseDecl = EnumCaseDecl::create(
        /*CaseLoc*/ loc, {enumEltDecl}, branchingTraceDecl);
    enumCaseDecl->setImplicit();
    branchingTraceDecl->addMember(enumEltDecl);
    branchingTraceDecl->addMember(enumCaseDecl);
    // Record enum element declaration.
    branchingTraceEnumCases.insert({{predBB, originalBB}, enumEltDecl});
  }
  // If original block is in a loop, mark branching trace enum as indirect.
  if (loopInfo->getLoopFor(originalBB))
    branchingTraceDecl->getAttrs().add(new (astCtx)
                                           IndirectAttr(/*Implicit*/ true));
  return branchingTraceDecl;
}

StructDecl *
LinearMapInfo::createLinearMapStruct(SILBasicBlock *originalBB,
                                     SILAutoDiffIndices indices,
                                     CanGenericSignature genericSig) {
  assert(originalBB->getParent() == original);
  auto *original = originalBB->getParent();
  auto &astCtx = original->getASTContext();
  auto &file = getDeclarationFileUnit();
  std::string structName;
  switch (kind) {
  case swift::AutoDiffLinearMapKind::Differential:
    structName = "_AD__" + original->getName().str() + "_bb" +
                 std::to_string(originalBB->getDebugID()) + "__DF__" +
                 indices.mangle();
    break;
  case swift::AutoDiffLinearMapKind::Pullback:
    structName = "_AD__" + original->getName().str() + "_bb" +
                 std::to_string(originalBB->getDebugID()) + "__PB__" +
                 indices.mangle();
    break;
  }
  auto structId = astCtx.getIdentifier(structName);
  GenericParamList *genericParams = nullptr;
  if (genericSig)
    genericParams = cloneGenericParameters(astCtx, &file, genericSig);
  auto *linearMapStruct = new (astCtx) StructDecl(
      /*StructLoc*/ SourceLoc(), /*Name*/ structId, /*NameLoc*/ SourceLoc(),
      /*Inherited*/ {}, /*GenericParams*/ genericParams, /*DC*/ &file);
  // Note: must mark struct as implicit to satisfy assertion in
  // `Parser::parseDeclListDelayed`.
  linearMapStruct->setImplicit();
  if (genericSig)
    linearMapStruct->setGenericSignature(genericSig);
  computeAccessLevel(linearMapStruct, original->getEffectiveSymbolLinkage());
  linearMapStruct->getInterfaceType();
  assert(linearMapStruct->hasInterfaceType());
  file.addVisibleDecl(linearMapStruct);
  return linearMapStruct;
}

VarDecl *LinearMapInfo::addLinearMapDecl(ApplyInst *ai, SILType linearMapType) {
  // IRGen requires decls to have AST types (not `SILFunctionType`), so we
  // convert the `SILFunctionType` of the linear map to a `FunctionType` with
  // the same parameters and results.
  auto silFnTy = linearMapType.castTo<SILFunctionType>();
  SmallVector<AnyFunctionType::Param, 8> params;
  for (auto &param : silFnTy->getParameters()) {
    ParameterTypeFlags flags;
    if (param.isIndirectMutating())
      flags = flags.withInOut(true);
    params.push_back(
        AnyFunctionType::Param(param.getInterfaceType(), Identifier(), flags));
  }
  AnyFunctionType *astFnTy;
  if (auto genSig = silFnTy->getSubstGenericSignature())
    astFnTy = GenericFunctionType::get(
        genSig, params, silFnTy->getAllResultsInterfaceType().getASTType());
  else
    astFnTy = FunctionType::get(
        params, silFnTy->getAllResultsInterfaceType().getASTType());

  auto *origBB = ai->getParent();
  auto *linMapStruct = getLinearMapStruct(origBB);
  std::string linearMapName;
  switch (kind) {
  case AutoDiffLinearMapKind::Differential:
    linearMapName = "differential_" + llvm::itostr(linearMapFieldMap.size());
    break;
  case AutoDiffLinearMapKind::Pullback:
    linearMapName = "pullback_" + llvm::itostr(linearMapFieldMap.size());
    break;
  }
  auto *linearMapDecl = addVarDecl(linMapStruct, linearMapName, astFnTy);
  linearMapFieldMap.insert({ai, linearMapDecl});
  return linearMapDecl;
}

void LinearMapInfo::addLinearMapToStruct(ADContext &context, ApplyInst *ai,
                                         SILAutoDiffIndices indices) {
  SmallVector<SILValue, 4> allResults;
  SmallVector<unsigned, 8> activeParamIndices;
  SmallVector<unsigned, 8> activeResultIndices;
  collectMinimalIndicesForFunctionCall(ai, indices, activityInfo, allResults,
                                       activeParamIndices, activeResultIndices);

  // Check if there are any active results or arguments. If not, skip
  // this instruction.
  auto hasActiveResults = llvm::any_of(allResults, [&](SILValue res) {
    return activityInfo.isActive(res, indices);
  });
  bool hasActiveInoutArgument = false;
  bool hasActiveArguments = false;
  auto numIndirectResults = ai->getNumIndirectResults();
  for (auto argIdx : range(ai->getSubstCalleeConv().getNumParameters())) {
    auto arg = ai->getArgumentsWithoutIndirectResults()[argIdx];
    if (activityInfo.isActive(arg, indices)) {
      hasActiveArguments = true;
      auto paramInfo = ai->getSubstCalleeConv().getParamInfoForSILArg(
          numIndirectResults + argIdx);
      if (paramInfo.isIndirectMutating())
        hasActiveInoutArgument = true;
    }
  }
  if (!hasActiveArguments)
    return;
  if (!hasActiveResults && !hasActiveInoutArgument)
    return;

  // Compute differentiation result index.
  auto source = activeResultIndices.front();
  // Compute differentiation parameters.
  // - If the callee has `@differentiable` function type, use differentiation
  //   parameters from the function type.
  // - Otherwise, use the active parameters.
  IndexSubset *parameters;
  auto origFnSubstTy = ai->getSubstCalleeType();
  auto remappedOrigFnSubstTy =
      remapTypeInDerivative(SILType::getPrimitiveObjectType(origFnSubstTy))
          .castTo<SILFunctionType>()
          ->getUnsubstitutedType(original->getModule());
  if (remappedOrigFnSubstTy->isDifferentiable()) {
    parameters = remappedOrigFnSubstTy->getDifferentiabilityParameterIndices();
  } else {
    parameters = IndexSubset::get(
        original->getASTContext(),
        ai->getArgumentsWithoutIndirectResults().size(), activeParamIndices);
  }
  // Create autodiff indices for the `apply` instruction.
  SILAutoDiffIndices applyIndices(source, parameters);

  // Check for non-differentiable original function type.
  auto checkNondifferentiableOriginalFunctionType =
      [&](CanSILFunctionType origFnTy) {
        // Check non-differentiable arguments.
        for (unsigned paramIndex : range(origFnTy->getNumParameters())) {
          auto remappedParamType = origFnTy->getParameters()[paramIndex]
                                       .getSILStorageInterfaceType();
          if (applyIndices.isWrtParameter(paramIndex) &&
              !remappedParamType.isDifferentiable(derivative->getModule()))
            return true;
        }
        // Check non-differentiable results.
        SILType remappedResultType;
        if (applyIndices.source >= origFnTy->getNumResults()) {
          auto inoutArgIdx = applyIndices.source - origFnTy->getNumResults();
          auto inoutArg =
              *std::next(ai->getInoutArguments().begin(), inoutArgIdx);
          remappedResultType = inoutArg->getType();
        } else {
          remappedResultType = origFnTy->getResults()[applyIndices.source]
                                   .getSILStorageInterfaceType();
        }
        if (!remappedResultType.isDifferentiable(derivative->getModule()))
          return true;
        return false;
      };
  if (checkNondifferentiableOriginalFunctionType(remappedOrigFnSubstTy))
    return;

  AutoDiffDerivativeFunctionKind derivativeFnKind(kind);
  auto derivativeFnType =
      remappedOrigFnSubstTy
          ->getAutoDiffDerivativeFunctionType(
              parameters, source, derivativeFnKind, context.getTypeConverter(),
              LookUpConformanceInModule(
                  derivative->getModule().getSwiftModule()))
          ->getUnsubstitutedType(original->getModule());

  auto derivativeFnResultTypes = derivativeFnType->getAllResultsInterfaceType();
  auto linearMapSILType = derivativeFnResultTypes;
  if (auto tupleType = linearMapSILType.getAs<TupleType>()) {
    linearMapSILType = SILType::getPrimitiveObjectType(
        tupleType.getElementType(tupleType->getElements().size() - 1));
  }
  if (auto fnTy = linearMapSILType.getAs<SILFunctionType>()) {
    linearMapSILType = SILType::getPrimitiveObjectType(
        fnTy->getUnsubstitutedType(original->getModule()));
  }
  addLinearMapDecl(ai, linearMapSILType);
}

void LinearMapInfo::generateDifferentiationDataStructures(
    ADContext &context, SILAutoDiffIndices indices, SILFunction *derivativeFn) {
  auto &astCtx = original->getASTContext();
  auto *loopAnalysis = context.getPassManager().getAnalysis<SILLoopAnalysis>();
  auto *loopInfo = loopAnalysis->get(original);

  // Get the derivative function generic signature.
  CanGenericSignature derivativeFnGenSig = nullptr;
  if (auto *derivativeFnGenEnv = derivativeFn->getGenericEnvironment())
    derivativeFnGenSig =
        derivativeFnGenEnv->getGenericSignature()->getCanonicalSignature();

  // Create linear map struct for each original block.
  for (auto &origBB : *original) {
    auto *linearMapStruct =
        createLinearMapStruct(&origBB, indices, derivativeFnGenSig);
    linearMapStructs.insert({&origBB, linearMapStruct});
  }

  // Create branching trace enum for each original block and add it as a field
  // in the corresponding struct.
  StringRef traceEnumFieldName;
  switch (kind) {
  case AutoDiffLinearMapKind::Differential:
    traceEnumFieldName = "successor";
    break;
  case AutoDiffLinearMapKind::Pullback:
    traceEnumFieldName = "predecessor";
    break;
  }
  for (auto &origBB : *original) {
    auto *traceEnum = createBranchingTraceDecl(&origBB, indices,
                                               derivativeFnGenSig, loopInfo);
    branchingTraceDecls.insert({&origBB, traceEnum});
    if (origBB.isEntry())
      continue;
    // Add branching trace enum field to corresponding linear map struct.
    auto *linearMapStruct = getLinearMapStruct(&origBB);
    auto *traceEnumField = addVarDecl(
        linearMapStruct, astCtx.getIdentifier(traceEnumFieldName).str(),
        traceEnum->getDeclaredInterfaceType());
    linearMapStructEnumFields.insert({linearMapStruct, traceEnumField});
  }

  // Add linear map fields to the linear map structs.
  for (auto &origBB : *original) {
    for (auto &inst : origBB) {
      if (auto *ai = dyn_cast<ApplyInst>(&inst)) {
        // Add linear map field to struct for active `apply` instructions.
        // Skip array literal intrinsic applications since array literal
        // initialization is linear and handled separately.
        if (!shouldDifferentiateApplySite(ai) || isArrayLiteralIntrinsic(ai))
          continue;
        LLVM_DEBUG(getADDebugStream()
                   << "Adding linear map struct field for " << *ai);
        addLinearMapToStruct(context, ai, indices);
      }
    }
  }

  // Print generated linear map structs and branching trace enums.
  // These declarations do not show up with `-emit-sil` because they are
  // implicit. Instead, use `-Xllvm -debug-only=differentiation` to test
  // declarations with FileCheck.
  LLVM_DEBUG({
    auto &s = getADDebugStream();
    PrintOptions printOptions;
    printOptions.TypeDefinitions = true;
    printOptions.ExplodePatternBindingDecls = true;
    printOptions.SkipImplicit = false;
    s << "Generated linear map structs and branching trace enums for @"
      << original->getName() << ":\n";
    for (auto &origBB : *original) {
      auto *linearMapStruct = getLinearMapStruct(&origBB);
      linearMapStruct->print(s, printOptions);
      s << '\n';
    }
    for (auto &origBB : *original) {
      auto *traceEnum = getBranchingTraceDecl(&origBB);
      traceEnum->print(s, printOptions);
      s << '\n';
    }
  });
}

/// Returns a flag that indicates whether the `apply` instruction should be
/// differentiated, given the differentiation indices of the instruction's
/// parent function. Whether the `apply` should be differentiated is determined
/// sequentially from the following conditions:
/// 1. The instruction has an active `inout` argument.
/// 2. The instruction is a call to the array literal initialization intrinsic
///    ("array.uninitialized_intrinsic"), where the result is active and where
///    there is a `store` of an active value into the array's buffer.
/// 3. The instruction has both an active result (direct or indirect) and an
///    active argument.
bool LinearMapInfo::shouldDifferentiateApplySite(FullApplySite applySite) {
  // Function applications with an inout argument should be differentiated.
  for (auto inoutArg : applySite.getInoutArguments())
    if (activityInfo.isActive(inoutArg, indices))
      return true;

  bool hasActiveDirectResults = false;
  forEachApplyDirectResult(applySite, [&](SILValue directResult) {
    hasActiveDirectResults |= activityInfo.isActive(directResult, indices);
  });
  bool hasActiveIndirectResults =
      llvm::any_of(applySite.getIndirectSILResults(), [&](SILValue result) {
        return activityInfo.isActive(result, indices);
      });
  bool hasActiveResults = hasActiveDirectResults || hasActiveIndirectResults;

  // TODO: Pattern match to make sure there is at least one `store` to the
  // array's active buffer.
  if (isArrayLiteralIntrinsic(applySite) && hasActiveResults)
    return true;

  auto arguments = applySite.getArgumentsWithoutIndirectResults();
  bool hasActiveArguments = llvm::any_of(arguments, [&](SILValue arg) {
    return activityInfo.isActive(arg, indices);
  });
  return hasActiveResults && hasActiveArguments;
}

/// Returns a flag indicating whether the instruction should be differentiated,
/// given the differentiation indices of the instruction's parent function.
/// Whether the instruction should be differentiated is determined sequentially
/// from any of the following conditions:
/// 1. The instruction is a full apply site and `shouldDifferentiateApplyInst`
///    returns true.
/// 2. The instruction has a source operand and a destination operand, both
///    being active.
/// 3. The instruction is an allocation instruction and has an active result.
/// 4. The instruction performs reference counting, lifetime ending, access
///    ending, or destroying on an active operand.
/// 5. The instruction creates an SSA copy of an active operand.
bool LinearMapInfo::shouldDifferentiateInstruction(SILInstruction *inst) {
  // A full apply site with an active argument and an active result (direct or
  // indirect) should be differentiated.
  if (FullApplySite::isa(inst))
    return shouldDifferentiateApplySite(FullApplySite(inst));
  // Anything with an active result and an active operand should be
  // differentiated.
  auto hasActiveOperands =
      llvm::any_of(inst->getAllOperands(), [&](Operand &op) {
        return activityInfo.isActive(op.get(), indices);
      });
  auto hasActiveResults = llvm::any_of(inst->getResults(), [&](SILValue val) {
    return activityInfo.isActive(val, indices);
  });
  if (hasActiveOperands && hasActiveResults)
    return true;
    // `store`-like instructions do not have an SSA result, but have two
    // operands that represent the source and the destination. We treat them as
    // the input and the output, respectively.
    // For `store`-like instructions whose destination is an element address
    // from an `array.uninitialized_intrinsic` application, return true if the
    // intrinsic application (representing the semantic destination) is active.
#define CHECK_INST_TYPE_ACTIVE_DEST(INST)                                      \
  if (auto *castInst = dyn_cast<INST##Inst>(inst))                             \
    return activityInfo.isActive(castInst->getDest(), indices);
  CHECK_INST_TYPE_ACTIVE_DEST(Store)
  CHECK_INST_TYPE_ACTIVE_DEST(StoreBorrow)
  CHECK_INST_TYPE_ACTIVE_DEST(CopyAddr)
  CHECK_INST_TYPE_ACTIVE_DEST(UnconditionalCheckedCastAddr)
#undef CHECK_INST_TYPE_ACTIVE_DEST
  // Should differentiate any allocation instruction that has an active result.
  if ((isa<AllocationInst>(inst) && hasActiveResults))
    return true;
  if (hasActiveOperands) {
    // Should differentiate any instruction that performs reference counting,
    // lifetime ending, access ending, or destroying on an active operand.
    if (isa<RefCountingInst>(inst) || isa<EndAccessInst>(inst) ||
        isa<EndBorrowInst>(inst) || isa<DeallocationInst>(inst) ||
        isa<DestroyValueInst>(inst) || isa<DestroyAddrInst>(inst))
      return true;
    // Should differentiate any instruction that creates an SSA copy of an
    // active operand.
    if (isa<CopyValueInst>(inst))
      return true;
  }
  return false;
}

} // end namespace autodiff
} // end namespace swift
