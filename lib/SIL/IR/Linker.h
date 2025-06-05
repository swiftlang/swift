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

/// Visits the call graph and makes sure that all required functions (according
/// to the linking `Mode`) are de-serialized and the `isSerialized` flag is set
/// correctly.
///
/// If the Mode is LinkNormal, just de-serialize the bare minimum of what's
/// required for a non-optimized compilation. That are all referenced shared
/// functions, because shared functions must have a body.
///
/// If the Mode is LinkAll, de-serialize the maximum amount of functions - for
/// optimized compilation.
///
/// Also make sure that shared functions which are referenced from "IsSerialized"
/// functions also have the "IsSerialized" flag set.
/// Usually this already is enforced by SILGen and all passes which create new
/// functions.
/// Only in case a de-serialized function references a shared function which
/// already exists in the module as "IsNotSerialized", it's required to explicitly
/// set the "IsSerialized" flag.
///
/// Example:
///
/// Before de-serialization:
/// \code
///   sil @foo {
///     function_ref @publicFuncInOtherModule
///     function_ref @specializedFunction
///   }
///   sil shared @specializedFunction { // IsNotSerialized
///     function_ref @otherSpecializedFunction
///   }
///   sil shared @otherSpecializedFunction { // IsNotSerialized
///   }
/// \endcode
///
/// After de-serialization:
/// \code
///   sil @foo {
///     function_ref @publicFuncInOtherModule
///     function_ref @specializedFunction
///   }
///   sil public_external [serialized] @publicFuncInOtherModule {
///     function_ref @specializedFunction
///   }
///   // Need to be changed to "IsSerialized"
///   sil shared [serialized] @specializedFunction {
///     function_ref @otherSpecializedFunction
///   }
///   sil shared [serialized] @otherSpecializedFunction {
///   }
/// \endcode
///
class SILLinkerVisitor : public SILInstructionVisitor<SILLinkerVisitor, void> {
  using LinkingMode = SILModule::LinkingMode;

  /// The SILModule that we are loading from.
  SILModule &Mod;

  /// Break cycles visiting recursive protocol conformances.
  llvm::DenseSet<ProtocolConformance *> VisitedConformances;

  /// Worklist of SILFunctions we are processing.
  llvm::SmallVector<SILFunction *, 128> Worklist;

  llvm::SmallVector<SILFunction *, 32> toVerify;

  /// The current linking mode.
  LinkingMode Mode;

  /// Whether any functions were deserialized.
  bool Changed;

  bool hasError = false;

public:
  SILLinkerVisitor(SILModule &M, SILModule::LinkingMode LinkingMode)
      : Mod(M), Worklist(), Mode(LinkingMode), Changed(false) {}

  /// Process F, recursively deserializing any thing F may reference.
  /// Returns true if any deserialization was performed.
  bool processFunction(SILFunction *F);

  /// Process the witnesstable of \p conformanceRef.
  /// Returns true if any deserialization was performed.
  bool processConformance(ProtocolConformanceRef conformanceRef);

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
                                bool referencedFromInitExistential);
  void visitApplySubstitutions(SubstitutionMap subs);
  void visitWitnessMethodInst(WitnessMethodInst *WMI) {
    visitProtocolConformance(WMI->getConformance(), false);
  }
  void visitInitExistentialAddrInst(InitExistentialAddrInst *IEI);
  void visitInitExistentialRefInst(InitExistentialRefInst *IERI);
  void visitBuiltinInst(BuiltinInst *bi);
  void visitAllocRefInst(AllocRefInst *ARI);
  void visitAllocRefDynamicInst(AllocRefDynamicInst *ARI);
  void visitMetatypeInst(MetatypeInst *MI);
  void visitGlobalAddrInst(GlobalAddrInst *i);

private:
  /// Cause a function to be deserialized, and visit all other functions
  /// referenced from this function according to the linking mode.
  void deserializeAndPushToWorklist(SILFunction *F);

  /// Consider a function for deserialization if the current linking mode
  /// requires it.
  ///
  /// If `callerSerializedKind` is IsSerialized, then all shared
  /// functions which are referenced from `F` are set to be serialized.
  void maybeAddFunctionToWorklist(SILFunction *F,
                                  SerializedKind_t callerSerializedKind,
                                  SILFunction *caller = nullptr);

  /// Is the current mode link all? Link all implies we should try and link
  /// everything, not just transparent/shared functions.
  bool isLinkAll() const {
    return Mode == LinkingMode::LinkAll || Mod.getOptions().EmbeddedSwift;
  }

  void linkInVTable(ClassDecl *D);

  // Main loop of the visitor. Called by one of the other *visit* methods.
  void process();
};

} // end namespace swift

#endif
