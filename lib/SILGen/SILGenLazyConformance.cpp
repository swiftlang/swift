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

#include "SILGen.h"
#include "swift/AST/Decl.h"
#include "swift/AST/ProtocolConformance.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILVisitor.h"

using namespace swift;
using namespace swift::Lowering;

void SILGenModule::useConformance(ProtocolConformanceRef conformanceRef) {
  // We don't need to emit dependent conformances.
  if (conformanceRef.isAbstract())
    return;

  auto conformance = conformanceRef.getConcrete();
  auto normal = dyn_cast<NormalProtocolConformance>(
      conformance->getRootConformance());
  if (normal == nullptr)
    return;

  // If we already emitted this witness table, we don't need to track the fact
  // we need it.
  if (emittedWitnessTables.count(normal))
    return;

  // If we delayed emitting this witness table, force it.
  auto foundDelayed = delayedConformances.find(normal);
  if (foundDelayed != delayedConformances.end()) {
    forcedConformances.push_back(*foundDelayed);
    delayedConformances.erase(foundDelayed);
    return;
  }

  // Otherwise, just remember the fact we used this conformance.
  usedConformances.insert(normal);
}

void SILGenModule::useConformancesFromSubstitutions(
                                                const SubstitutionMap subs) {
  for (auto conf : subs.getConformances())
    useConformance(conf);
}

void SILGenModule::useConformancesFromType(CanType type) {
  type.findIf([&](Type t) -> bool {
    auto *decl = t->getAnyNominal();
    if (!decl)
      return false;

    if (isa<ProtocolDecl>(decl))
      return false;

    auto *genericSig = decl->getGenericSignature();
    if (!genericSig)
      return false;

    auto subMap = t->getContextSubstitutionMap(SwiftModule, decl);
    useConformancesFromSubstitutions(subMap);
    return false;
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
    SGM.useConformancesFromType(AEBI->getFormalConcreteType());
    for (auto conformance : AEBI->getConformances())
      SGM.useConformance(conformance);
  }

  void visitAllocGlobalInst(AllocGlobalInst *AGI) {
    SGM.useConformancesFromType(
        AGI->getReferencedGlobal()->getLoweredType().getASTType());
  }

  void visitAllocRefInst(AllocRefInst *ARI) {
    SGM.useConformancesFromType(ARI->getType().getASTType());
  }

  void visitAllocStackInst(AllocStackInst *ASI) {
    SGM.useConformancesFromType(ASI->getType().getASTType());
  }

  void visitAllocValueBufferInst(AllocValueBufferInst *AVBI) {
    SGM.useConformancesFromType(AVBI->getType().getASTType());
  }

  void visitApplyInst(ApplyInst *AI) {
    SGM.useConformancesFromSubstitutions(AI->getSubstitutionMap());
  }

  void visitBeginApplyInst(BeginApplyInst *BAI) {
    SGM.useConformancesFromSubstitutions(BAI->getSubstitutionMap());
  }

  void visitBuiltinInst(BuiltinInst *BI) {
    SGM.useConformancesFromSubstitutions(BI->getSubstitutions());
  }

  void visitCheckedCastBranchInst(CheckedCastBranchInst *CCBI) {
    SGM.useConformancesFromType(CCBI->getSourceType());
    SGM.useConformancesFromType(CCBI->getTargetType());
  }

  void visitCheckedCastAddrBranchInst(CheckedCastAddrBranchInst *CCABI) {
    SGM.useConformancesFromType(CCABI->getSourceType());
    SGM.useConformancesFromType(CCABI->getTargetType());
  }

  void visitCheckedCastValueBranchInst(CheckedCastValueBranchInst *CCVBI) {
    SGM.useConformancesFromType(CCVBI->getSourceType());
    SGM.useConformancesFromType(CCVBI->getTargetType());
  }

  void visitCopyAddrInst(CopyAddrInst *CAI) {
    SGM.useConformancesFromType(CAI->getSrc()->getType().getASTType());
    SGM.useConformancesFromType(CAI->getDest()->getType().getASTType());
  }

  void visitCopyValueInst(CopyValueInst *CVI) {
    SGM.useConformancesFromType(CVI->getOperand()->getType().getASTType());
  }

  void visitDestroyAddrInst(DestroyAddrInst *DAI) {
    SGM.useConformancesFromType(DAI->getOperand()->getType().getASTType());
  }

  void visitDestroyValueInst(DestroyValueInst *DVI) {
    SGM.useConformancesFromType(DVI->getOperand()->getType().getASTType());
  }

  void visitGlobalAddrInst(GlobalAddrInst *GAI) {
    SGM.useConformancesFromType(
        GAI->getReferencedGlobal()->getLoweredType().getASTType());
  }

  void visitGlobalValueInst(GlobalValueInst *GVI) {
    SGM.useConformancesFromType(
        GVI->getReferencedGlobal()->getLoweredType().getASTType());
  }

  void visitKeyPathInst(KeyPathInst *KPI) {
    SGM.useConformancesFromSubstitutions(KPI->getSubstitutions());
  }

  void visitInitEnumDataAddrInst(InitEnumDataAddrInst *IEDAI) {
    SGM.useConformancesFromType(
        IEDAI->getOperand()->getType().getASTType());
  }

  void visitInjectEnumAddrInst(InjectEnumAddrInst *IEAI) {
    SGM.useConformancesFromType(IEAI->getOperand()->getType().getASTType());
  }

  void visitInitExistentialAddrInst(InitExistentialAddrInst *IEAI) {
    SGM.useConformancesFromType(IEAI->getFormalConcreteType());
    for (auto conformance : IEAI->getConformances())
      SGM.useConformance(conformance);
  }

  void visitInitExistentialMetatypeInst(InitExistentialMetatypeInst *IEMI) {
    SGM.useConformancesFromType(IEMI->getOperand()->getType().getASTType());
    for (auto conformance : IEMI->getConformances())
      SGM.useConformance(conformance);
  }

  void visitInitExistentialRefInst(InitExistentialRefInst *IERI) {
    SGM.useConformancesFromType(IERI->getFormalConcreteType());
    for (auto conformance : IERI->getConformances())
      SGM.useConformance(conformance);
  }

  void visitInitExistentialValueInst(InitExistentialValueInst *IEVI) {
    SGM.useConformancesFromType(IEVI->getFormalConcreteType());
    for (auto conformance : IEVI->getConformances())
      SGM.useConformance(conformance);
  }

  void visitMetatypeInst(MetatypeInst *MI) {
    SGM.useConformancesFromType(MI->getType().getASTType());
  }

  void visitPartialApplyInst(PartialApplyInst *PAI) {
    SGM.useConformancesFromSubstitutions(PAI->getSubstitutionMap());
  }

  void visitSelectEnumAddrInst(SelectEnumAddrInst *SEAI) {
    SGM.useConformancesFromType(
        SEAI->getEnumOperand()->getType().getASTType());
  }

  void visitStructElementAddrInst(StructElementAddrInst *SEAI) {
    SGM.useConformancesFromType(SEAI->getOperand()->getType().getASTType());
  }

  void visitTryApplyInst(TryApplyInst *TAI) {
    SGM.useConformancesFromSubstitutions(TAI->getSubstitutionMap());
  }

  void visitTupleElementAddrInst(TupleElementAddrInst *TEAI) {
    SGM.useConformancesFromType(TEAI->getOperand()->getType().getASTType());
  }

  void visitUnconditionalCheckedCastInst(UnconditionalCheckedCastInst *UCCI) {
    SGM.useConformancesFromType(UCCI->getSourceType());
    SGM.useConformancesFromType(UCCI->getTargetType());
  }

  void visitUnconditionalCheckedCastAddrInst(UnconditionalCheckedCastAddrInst *UCCAI) {
    SGM.useConformancesFromType(UCCAI->getSourceType());
    SGM.useConformancesFromType(UCCAI->getTargetType());
  }

  void visitUncheckedTakeEnumDataAddrInst(UncheckedTakeEnumDataAddrInst *UTEDAI) {}

  void visitWitnessMethodInst(WitnessMethodInst *WMI) {
    SGM.useConformance(WMI->getConformance());
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
  auto *genericSig = NTD->getGenericSignature();

  if (genericSig) {
    for (auto reqt : genericSig->getRequirements()) {
      if (reqt.getKind() != RequirementKind::Layout)
        useConformancesFromType(reqt.getSecondType()->getCanonicalType());
    }
  }

  if (auto *ED = dyn_cast<EnumDecl>(NTD)) {
    for (auto *EED : ED->getAllElements()) {
      if (EED->hasAssociatedValues()) {
        useConformancesFromType(EED->getArgumentInterfaceType()
                                   ->getCanonicalType(genericSig));
      }
    }
  }

  if (isa<StructDecl>(NTD) || isa<ClassDecl>(NTD)) {
    for (auto *VD : NTD->getStoredProperties()) {
      useConformancesFromType(VD->getValueInterfaceType()
                                ->getCanonicalType(genericSig));
    }
  }

  if (auto *CD = dyn_cast<ClassDecl>(NTD))
    if (auto superclass = CD->getSuperclass())
      useConformancesFromType(superclass->getCanonicalType(genericSig));

  if (auto *PD = dyn_cast<ProtocolDecl>(NTD)) {
    for (auto reqt : PD->getRequirementSignature()) {
      if (reqt.getKind() != RequirementKind::Layout)
        useConformancesFromType(reqt.getSecondType()->getCanonicalType());
    }
  }
}