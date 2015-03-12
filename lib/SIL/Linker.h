//===--- Linker.h --------------------------------------------*- C++ -*----===//
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

#ifndef SWIFT_SIL_LINKER_H
#define SWIFT_SIL_LINKER_H

#include "swift/SIL/SILDebugScope.h"
#include "swift/SIL/SILExternalSource.h"
#include "swift/SIL/SILVisitor.h"
#include "swift/SIL/SILModule.h"
#include "swift/Serialization/SerializedSILLoader.h"

namespace swift {

/// Visitor that knows how to link in dependencies of SILInstructions.
class SILLinkerVisitor : public SILInstructionVisitor<SILLinkerVisitor, bool> {
  using LinkingMode = SILModule::LinkingMode;

  /// The SILModule that we are loading from.
  SILModule &Mod;

  /// The SILLoader that this visitor is using to link.
  SerializedSILLoader *Loader;

  /// The external SIL source to use when linking this module.
  SILExternalSource *ExternalSource = nullptr;

  /// Worklist of SILFunctions we are processing.
  llvm::SmallVector<SILFunction *, 128> Worklist;

  /// A list of callees of the current instruction being visited. cleared after
  /// every instruction is visited.
  llvm::SmallVector<SILFunction *, 4> FunctionDeserializationWorklist;

  /// The current linking mode.
  LinkingMode Mode;

public:
  SILLinkerVisitor(SILModule &M, SerializedSILLoader *L,
                   SILModule::LinkingMode LinkingMode,
                   SILExternalSource *E = nullptr)
      : Mod(M), Loader(L), ExternalSource(E), Worklist(),
        FunctionDeserializationWorklist(), Mode(LinkingMode) {}

  /// Process F, recursively deserializing any thing F may reference.
  bool processFunction(SILFunction *F);

  /// Deserialize the VTable mapped to C if it exists and all SIL the VTable
  /// transitively references.
  ///
  /// This method assumes that the caller made sure that no vtable existed in
  /// Mod.
  SILVTable *processClassDecl(const ClassDecl *C);

  /// We do not want to visit callee functions if we just have a value base.
  bool visitValueBase(ValueBase *V) { return false; }

  bool visitApplyInst(ApplyInst *AI);
  bool visitPartialApplyInst(PartialApplyInst *PAI);
  bool visitFunctionRefInst(FunctionRefInst *FRI);
  bool visitProtocolConformance(ProtocolConformance *C,
                                const Optional<SILDeclRef> &Member);
  bool visitWitnessMethodInst(WitnessMethodInst *WMI) {
    return visitProtocolConformance(WMI->getConformance(), WMI->getMember());
  }
  bool visitInitExistentialAddrInst(InitExistentialAddrInst *IEI);
  bool visitInitExistentialRefInst(InitExistentialRefInst *IERI);
  bool visitAllocRefInst(AllocRefInst *ARI);
  bool visitMetatypeInst(MetatypeInst *MI);

private:
  /// Add a function to our function worklist for processing.
  void addFunctionToWorklist(SILFunction *F) {
    FunctionDeserializationWorklist.push_back(F);
  }

  /// Is the current mode link all? Link all implies we should try and link
  /// everything, not just transparent/shared functions.
  bool isLinkAll() const { return Mode == LinkingMode::LinkAll; }

  bool linkInVTable(ClassDecl *D);

  // Main loop of the visitor. Called by one of the other *visit* methods.
  bool process();
};

} // end namespace swift

#endif
