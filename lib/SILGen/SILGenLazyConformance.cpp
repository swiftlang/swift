//===--- SILGenLazyConformance.cpp ----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file forces emission of lazily-generated ClangImporter-synthesized
//  conformances.
//
//===----------------------------------------------------------------------===//

#include "SILGen.h"
#include "swift/AST/ConformanceLookup.h"
#include "swift/AST/Decl.h"
#include "swift/AST/PackConformance.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/AST/ProtocolConformanceRef.h"
#include "swift/Basic/Assertions.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/SIL/FormalLinkage.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILVisitor.h"

using namespace swift;
using namespace swift::Lowering;

void SILGenModule::useConformance(SILInstruction *inst,
                                  ProtocolConformanceRef conformanceRef) {
  // If the conformance is invalid, crash deterministically even in noassert
  // builds.
  if (conformanceRef.isInvalid()) {
    // When lazy type checking is enabled, a conformance may only be diagnosed
    // as invalid during SILGen. Ignore it instead of asserting.
    auto &ctx = getASTContext();
    if (ctx.TypeCheckerOpts.EnableLazyTypecheck && ctx.hadError())
      return;

    ABORT([&](auto &out) {
      out << "Found an invalid conformance in SILGen. This may indicate a problem in ";
      out << "Sema or the ClangImporter.\n";

      if (inst != nullptr) {
        out << "\n";
        out << "Emitting lazy conformances for instruction:\n";
        inst->print(out);
        out << "\n";
        out << "In function:\n";
        inst->getFunction()->print(out);
        out << "\n";
      }
    });
  }

  // We don't need to emit dependent conformances.
  if (conformanceRef.isAbstract())
    return;

  // Recursively visit pack conformances.
  if (conformanceRef.isPack()) {
    auto *packConformance = conformanceRef.getPack();

    for (auto patternConformanceRef : packConformance->getPatternConformances())
      useConformance(inst, patternConformanceRef);

    return;
  }

  auto conformance = conformanceRef.getConcrete();

  // Always look through inherited conformances.
  if (auto *inherited = dyn_cast<InheritedProtocolConformance>(conformance))
    conformance = inherited->getInheritedConformance();

  // Emit any conformances implied by conditional requirements.
  if (auto *specialized = dyn_cast<SpecializedProtocolConformance>(conformance)) {
    useConformancesFromSubstitutions(inst, specialized->getSubstitutionMap());
    conformance = specialized->getGenericConformance();
  }

  // Get the normal conformance. If we don't have one, this is a self
  // conformance, which we can ignore.
  auto normal = dyn_cast<NormalProtocolConformance>(conformance);
  if (normal == nullptr)
    return;

  // If this conformance was not synthesized, we're not going to be emitting
  // it lazily either, so we can avoid doing anything below.
  if (!normal->isSynthesized())
    return;

  // If we already emitted this witness table, we don't need to track the fact
  // we need it.
  if (emittedWitnessTables.count(normal))
    return;

  // Check if we already forced this witness table but haven't emitted it yet.
  if (!forcedConformances.insert(normal).second)
    return;

  pendingConformances.push_back(normal);
}

void SILGenModule::useConformancesFromSubstitutions(
                                                SILInstruction *inst,
                                                const SubstitutionMap subs) {
  for (auto conf : subs.getConformances())
    useConformance(inst, conf);
}

void SILGenModule::useConformancesFromType(SILInstruction *inst, CanType type) {
  if (!usedConformancesFromTypes.insert(type.getPointer()).second)
    return;

  type.visit([&](Type t) {
    auto *decl = t->getAnyNominal();
    if (!decl)
      return;

    if (isa<ProtocolDecl>(decl))
      return;

    // If this is an imported noncopyable type with a deinitializer, record it.
    if (decl->hasClangNode() && !decl->canBeCopyable() &&
        decl->getValueTypeDestructor() &&
        importedNontrivialNoncopyableTypes.insert(decl).second) {
      visitImportedNontrivialNoncopyableType(decl);
    }

    auto genericSig = decl->getGenericSignature();
    if (!genericSig)
      return;

    auto subMap = t->getContextSubstitutionMap();
    useConformancesFromSubstitutions(inst, subMap);
    return;
  });
}

void SILGenModule::useConformancesFromObjectiveCType(
    SILInstruction *inst, CanType type) {
  if (!usedConformancesFromObjectiveCTypes.insert(type.getPointer()).second)
    return;

  auto &ctx = getASTContext();
  auto objectiveCBridgeable = ctx.getProtocol(
      KnownProtocolKind::ObjectiveCBridgeable);
  auto bridgedStoredNSError = ctx.getProtocol(
      KnownProtocolKind::BridgedStoredNSError);
  if (!objectiveCBridgeable && !bridgedStoredNSError)
    return;

  type.visit([&](Type t) {
    auto *decl = t->getAnyNominal();
    if (!decl)
      return;

    if (!isa<ClangModuleUnit>(decl->getModuleScopeContext()))
      return;

    if (objectiveCBridgeable) {
      if (auto subConformance = lookupConformance(t, objectiveCBridgeable))
        useConformance(inst, subConformance);
    }

    if (bridgedStoredNSError) {
      if (auto subConformance = lookupConformance(t, bridgedStoredNSError))
        useConformance(inst, subConformance);
    }
  });
}

/// A visitor class that tries to guess which SIL instructions can cause
/// IRGen to emit references to witness tables. This is used to emit
/// ClangImporter-synthesized conformances lazily.
///
/// In the long run, we'll instead have IRGen directly ask SILGen to
/// generate a witness table when needed, so that we don't have to do
/// any "guessing" here.
class LazyConformanceEmitter : public SILInstructionVisitor<LazyConformanceEmitter> {
  SILGenModule &SGM;

public:
  LazyConformanceEmitter(SILGenModule &SGM) : SGM(SGM) {}

  void visitAllocExistentialBoxInst(AllocExistentialBoxInst *AEBI) {
    SGM.useConformancesFromType(AEBI, AEBI->getFormalConcreteType());
    SGM.useConformancesFromObjectiveCType(AEBI, AEBI->getFormalConcreteType());
    for (auto conformance : AEBI->getConformances())
      SGM.useConformance(AEBI, conformance);
  }

  void visitAllocGlobalInst(AllocGlobalInst *AGI) {
    SGM.useConformancesFromType(AGI,
        AGI->getReferencedGlobal()->getLoweredType().getASTType());
  }

  void visitAllocRefInst(AllocRefInst *ARI) {
    SGM.useConformancesFromType(ARI, ARI->getType().getASTType());
  }

  void visitAllocStackInst(AllocStackInst *ASI) {
    SGM.useConformancesFromType(ASI, ASI->getType().getASTType());
  }

  void visitApplyInst(ApplyInst *AI) {
    SGM.useConformancesFromObjectiveCType(AI, AI->getSubstCalleeType());
    SGM.useConformancesFromSubstitutions(AI, AI->getSubstitutionMap());
  }

  void visitBeginApplyInst(BeginApplyInst *BAI) {
    SGM.useConformancesFromObjectiveCType(BAI, BAI->getSubstCalleeType());
    SGM.useConformancesFromSubstitutions(BAI, BAI->getSubstitutionMap());
  }

  void visitBuiltinInst(BuiltinInst *BI) {
    SGM.useConformancesFromSubstitutions(BI, BI->getSubstitutions());
  }

  void visitCheckedCastBranchInst(CheckedCastBranchInst *CCBI) {
    SGM.useConformancesFromType(CCBI, CCBI->getSourceFormalType());
    SGM.useConformancesFromType(CCBI, CCBI->getTargetFormalType());
    SGM.useConformancesFromObjectiveCType(CCBI, CCBI->getSourceFormalType());
    SGM.useConformancesFromObjectiveCType(CCBI, CCBI->getTargetFormalType());
  }

  void visitCheckedCastAddrBranchInst(CheckedCastAddrBranchInst *CCABI) {
    SGM.useConformancesFromType(CCABI, CCABI->getSourceFormalType());
    SGM.useConformancesFromType(CCABI, CCABI->getTargetFormalType());
    SGM.useConformancesFromObjectiveCType(CCABI, CCABI->getSourceFormalType());
    SGM.useConformancesFromObjectiveCType(CCABI, CCABI->getTargetFormalType());
  }

  void visitCopyAddrInst(CopyAddrInst *CAI) {
    SGM.useConformancesFromType(CAI, CAI->getSrc()->getType().getASTType());
    SGM.useConformancesFromType(CAI, CAI->getDest()->getType().getASTType());
  }

  void visitMarkUnresolvedMoveAddrInst(MarkUnresolvedMoveAddrInst *MAI) {
    SGM.useConformancesFromType(MAI, MAI->getSrc()->getType().getASTType());
    SGM.useConformancesFromType(MAI, MAI->getDest()->getType().getASTType());
  }

  void visitCopyValueInst(CopyValueInst *CVI) {
    SGM.useConformancesFromType(CVI, CVI->getOperand()->getType().getASTType());
  }

  void visitDestroyAddrInst(DestroyAddrInst *DAI) {
    SGM.useConformancesFromType(DAI, DAI->getOperand()->getType().getASTType());
  }

  void visitDestroyValueInst(DestroyValueInst *DVI) {
    SGM.useConformancesFromType(DVI, DVI->getOperand()->getType().getASTType());
  }

  void visitGlobalAddrInst(GlobalAddrInst *GAI) {
    SGM.useConformancesFromType(GAI,
        GAI->getReferencedGlobal()->getLoweredType().getASTType());
  }

  void visitGlobalValueInst(GlobalValueInst *GVI) {
    SGM.useConformancesFromType(GVI,
        GVI->getReferencedGlobal()->getLoweredType().getASTType());
  }

  void visitKeyPathInst(KeyPathInst *KPI) {
    SGM.useConformancesFromSubstitutions(KPI, KPI->getSubstitutions());
  }

  void visitInitEnumDataAddrInst(InitEnumDataAddrInst *IEDAI) {
    SGM.useConformancesFromType(IEDAI,
        IEDAI->getOperand()->getType().getASTType());
  }

  void visitInjectEnumAddrInst(InjectEnumAddrInst *IEAI) {
    SGM.useConformancesFromType(IEAI, IEAI->getOperand()->getType().getASTType());
  }

  void visitInitExistentialAddrInst(InitExistentialAddrInst *IEAI) {
    SGM.useConformancesFromType(IEAI, IEAI->getFormalConcreteType());
    SGM.useConformancesFromObjectiveCType(IEAI, IEAI->getFormalConcreteType());
    for (auto conformance : IEAI->getConformances())
      SGM.useConformance(IEAI, conformance);
  }

  void visitInitExistentialMetatypeInst(InitExistentialMetatypeInst *IEMI) {
    SGM.useConformancesFromType(IEMI, IEMI->getOperand()->getType().getASTType());
    for (auto conformance : IEMI->getConformances())
      SGM.useConformance(IEMI, conformance);
  }

  void visitInitExistentialRefInst(InitExistentialRefInst *IERI) {
    SGM.useConformancesFromType(IERI, IERI->getFormalConcreteType());
    SGM.useConformancesFromObjectiveCType(IERI, IERI->getFormalConcreteType());
    for (auto conformance : IERI->getConformances())
      SGM.useConformance(IERI, conformance);
  }

  void visitInitExistentialValueInst(InitExistentialValueInst *IEVI) {
    SGM.useConformancesFromType(IEVI, IEVI->getFormalConcreteType());
    SGM.useConformancesFromObjectiveCType(IEVI, IEVI->getFormalConcreteType());
    for (auto conformance : IEVI->getConformances())
      SGM.useConformance(IEVI, conformance);
  }

  void visitMetatypeInst(MetatypeInst *MI) {
    SGM.useConformancesFromType(MI, MI->getType().getASTType());
  }

  void visitPartialApplyInst(PartialApplyInst *PAI) {
    SGM.useConformancesFromObjectiveCType(PAI, PAI->getSubstCalleeType());
    SGM.useConformancesFromSubstitutions(PAI, PAI->getSubstitutionMap());
  }

  void visitSelectEnumAddrInst(SelectEnumAddrInst *SEAI) {
    SGM.useConformancesFromType(SEAI,
        SEAI->getEnumOperand()->getType().getASTType());
  }

  void visitStructElementAddrInst(StructElementAddrInst *SEAI) {
    SGM.useConformancesFromType(SEAI, SEAI->getOperand()->getType().getASTType());
  }

  void visitTryApplyInst(TryApplyInst *TAI) {
    SGM.useConformancesFromObjectiveCType(TAI, TAI->getSubstCalleeType());
    SGM.useConformancesFromSubstitutions(TAI, TAI->getSubstitutionMap());
  }

  void visitTupleElementAddrInst(TupleElementAddrInst *TEAI) {
    SGM.useConformancesFromType(TEAI, TEAI->getOperand()->getType().getASTType());
  }

  void visitUnconditionalCheckedCastInst(UnconditionalCheckedCastInst *UCCI) {
    SGM.useConformancesFromType(UCCI, UCCI->getSourceFormalType());
    SGM.useConformancesFromType(UCCI, UCCI->getTargetFormalType());
    SGM.useConformancesFromObjectiveCType(UCCI, UCCI->getSourceFormalType());
    SGM.useConformancesFromObjectiveCType(UCCI, UCCI->getTargetFormalType());
  }

  void visitUnconditionalCheckedCastAddrInst(UnconditionalCheckedCastAddrInst *UCCAI) {
    SGM.useConformancesFromType(UCCAI, UCCAI->getSourceFormalType());
    SGM.useConformancesFromType(UCCAI, UCCAI->getTargetFormalType());
    SGM.useConformancesFromObjectiveCType(UCCAI, UCCAI->getSourceFormalType());
    SGM.useConformancesFromObjectiveCType(UCCAI, UCCAI->getTargetFormalType());
  }

  void visitUncheckedTakeEnumDataAddrInst(UncheckedTakeEnumDataAddrInst *UTEDAI) {
    SGM.useConformancesFromType(UTEDAI, UTEDAI->getOperand()->getType().getASTType());
  }

  void visitWitnessMethodInst(WitnessMethodInst *WMI) {
    SGM.useConformance(WMI, WMI->getConformance());
  }

  void visitSILInstruction(SILInstruction *I) {}
};

void SILGenModule::emitLazyConformancesForFunction(SILFunction *F) {
  LazyConformanceEmitter emitter(*this);

  for (auto &BB : *F)
    for (auto &I : BB)
      emitter.visit(&I);
}

void SILGenModule::emitLazyConformancesForType(NominalTypeDecl *NTD) {
  auto genericSig = NTD->getGenericSignature();

  for (auto reqt : genericSig.getRequirements()) {
    if (reqt.getKind() != RequirementKind::Layout)
      useConformancesFromType(nullptr, reqt.getSecondType()->getCanonicalType());
  }

  if (auto *ED = dyn_cast<EnumDecl>(NTD)) {
    for (auto *EED : ED->getAllElements()) {
      if (EED->hasAssociatedValues()) {
        useConformancesFromType(nullptr, EED->getPayloadInterfaceType()
                                            ->getReducedType(genericSig));
      }
    }
  }

  if (isa<StructDecl>(NTD) || isa<ClassDecl>(NTD)) {
    for (auto *VD : NTD->getStoredProperties()) {
      useConformancesFromType(nullptr, VD->getValueInterfaceType()
                                          ->getReducedType(genericSig));
    }
  }

  if (auto *CD = dyn_cast<ClassDecl>(NTD))
    if (auto superclass = CD->getSuperclass())
      useConformancesFromType(nullptr, superclass->getReducedType(genericSig));

  if (auto *PD = dyn_cast<ProtocolDecl>(NTD)) {
    for (auto reqt : PD->getRequirementSignature().getRequirements()) {
      if (reqt.getKind() != RequirementKind::Layout)
        useConformancesFromType(nullptr, reqt.getSecondType()->getCanonicalType());
    }
  }
}
