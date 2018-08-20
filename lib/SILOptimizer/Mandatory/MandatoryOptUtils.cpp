//===--- MandatoryOptUtils.cpp --------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "sil-mandatory-utils"
#include "MandatoryOptUtils.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsSIL.h"
#include "swift/AST/Expr.h"
#include "swift/ClangImporter/ClangModule.h"
#include "swift/SIL/InstructionUtils.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/CFG.h"
#include "swift/SILOptimizer/Utils/Local.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallBitVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/Debug.h"

STATISTIC(NumAssignRewritten, "Number of assigns rewritten");

using namespace swift;

/// Emit the sequence that an assign instruction lowers to once we know
/// if it is an initialization or an assignment.  If it is an assignment,
/// a live-in value can be provided to optimize out the reload.
void swift::lowerAssignInstruction(SILBuilderWithScope &B, AssignInst *Inst,
                                   PartialInitializationKind isInitialization) {
  LLVM_DEBUG(llvm::dbgs() << "  *** Lowering [isInit="
                          << unsigned(isInitialization)
                          << "]: " << *Inst << "\n");

  ++NumAssignRewritten;

  SILValue Src = Inst->getSrc();
  SILLocation Loc = Inst->getLoc();

  if (isInitialization == PartialInitializationKind::IsInitialization ||
      Inst->getDest()->getType().isTrivial(Inst->getModule())) {

    // If this is an initialization, or the storage type is trivial, we
    // can just replace the assignment with a store.
    assert(isInitialization != PartialInitializationKind::IsReinitialization);
    B.createTrivialStoreOr(Loc, Src, Inst->getDest(),
                           StoreOwnershipQualifier::Init);
    Inst->eraseFromParent();
    return;
  }

  if (isInitialization == PartialInitializationKind::IsReinitialization) {
    // We have a case where a convenience initializer on a class
    // delegates to a factory initializer from a protocol extension.
    // Factory initializers give us a whole new instance, so the existing
    // instance, which has not been initialized and never will be, must be
    // freed using dealloc_partial_ref.
    SILValue Pointer =
        B.createLoad(Loc, Inst->getDest(), LoadOwnershipQualifier::Take);
    B.createStore(Loc, Src, Inst->getDest(), StoreOwnershipQualifier::Init);

    auto MetatypeTy = CanMetatypeType::get(
        Inst->getDest()->getType().getASTType(), MetatypeRepresentation::Thick);
    auto SILMetatypeTy = SILType::getPrimitiveObjectType(MetatypeTy);
    SILValue Metatype = B.createValueMetatype(Loc, SILMetatypeTy, Pointer);

    B.createDeallocPartialRef(Loc, Pointer, Metatype);
    Inst->eraseFromParent();
    return;
  }

  assert(isInitialization == PartialInitializationKind::IsNotInitialization);
  // Otherwise, we need to replace the assignment with the full
  // load/store/release dance. Note that the new value is already
  // considered to be retained (by the semantics of the storage type),
  // and we're transferring that ownership count into the destination.

  // This is basically TypeLowering::emitStoreOfCopy, except that if we have
  // a known incoming value, we can avoid the load.
  SILValue IncomingVal =
      B.createLoad(Loc, Inst->getDest(), LoadOwnershipQualifier::Take);
  B.createStore(Inst->getLoc(), Src, Inst->getDest(),
                StoreOwnershipQualifier::Init);

  B.emitDestroyValueOperation(Loc, IncomingVal);
  Inst->eraseFromParent();
}
