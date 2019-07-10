//===--- Linker.h -----------------------------------------------*- C++ -*-===//
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

#ifndef SWIFT_SIL_LINKER_H
#define SWIFT_SIL_LINKER_H

#include "swift/SIL/SILDebugScope.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SIL/SILModule.h"
#include <functional>

namespace swift {

/// Visitor that knows how to link in dependencies of SILInstructions.
class SILLinkerVisitor : public SILInstructionVisitor<SILLinkerVisitor, void> {
  using LinkingMode = SILModule::LinkingMode;

  /// The SILModule that we are loading from.
  SILModule &Mod;

  /// Break cycles visiting recursive protocol conformances.
  llvm::DenseSet<ProtocolConformance *> VisitedConformances;

  /// Worklist of SILFunctions we are processing.
  llvm::SmallVector<SILFunction *, 128> Worklist;

  /// A list of callees of the current instruction being visited. cleared after
  /// every instruction is visited.
  llvm::SmallVector<SILFunction *, 4> FunctionDeserializationWorklist;

  /// The current linking mode.
  LinkingMode Mode;

  /// Whether any functions were deserialized.
  bool Changed;

public:
  SILLinkerVisitor(SILModule &M, SILModule::LinkingMode LinkingMode)
      : Mod(M), Worklist(), FunctionDeserializationWorklist(),
        Mode(LinkingMode), Changed(false) {}

  /// Process F, recursively deserializing any thing F may reference.
  /// Returns true if any deserialization was performed.
  bool processFunction(SILFunction *F);

  /// Deserialize the VTable mapped to C if it exists and all SIL the VTable
  /// transitively references.
  ///
  /// This method assumes that the caller made sure that no vtable existed in
  /// Mod.
  SILVTable *processClassDecl(const ClassDecl *C);

  /// We do not want to visit callee functions if we just have a value base.
  void visitSILInstruction(SILInstruction *I) { }

  void visitApplyInst(ApplyInst *AI);
  void visitTryApplyInst(TryApplyInst *TAI);
  void visitPartialApplyInst(PartialApplyInst *PAI);
  void visitFunctionRefInst(FunctionRefInst *FRI);
  void visitDynamicFunctionRefInst(DynamicFunctionRefInst *FRI);
  void visitPreviousDynamicFunctionRefInst(PreviousDynamicFunctionRefInst *FRI);
  void visitProtocolConformance(ProtocolConformanceRef C,
                                const Optional<SILDeclRef> &Member);
  void visitApplySubstitutions(SubstitutionMap subs);
  void visitWitnessMethodInst(WitnessMethodInst *WMI) {
    visitProtocolConformance(WMI->getConformance(), WMI->getMember());
  }
  void visitInitExistentialAddrInst(InitExistentialAddrInst *IEI);
  void visitInitExistentialRefInst(InitExistentialRefInst *IERI);
  void visitAllocRefInst(AllocRefInst *ARI);
  void visitMetatypeInst(MetatypeInst *MI);

private:
  /// Cause a function to be deserialized, and visit all other functions
  /// referenced from this function according to the linking mode.
  void addFunctionToWorklist(SILFunction *F);

  /// Consider a function for deserialization if the current linking mode
  /// requires it.
  void maybeAddFunctionToWorklist(SILFunction *F);

  /// Is the current mode link all? Link all implies we should try and link
  /// everything, not just transparent/shared functions.
  bool isLinkAll() const { return Mode == LinkingMode::LinkAll; }

  void linkInVTable(ClassDecl *D);

  // Main loop of the visitor. Called by one of the other *visit* methods.
  void process();
};

} // end namespace swift

#endif
