//===--- SerializeSILPass.cpp ---------------------------------------------===//
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

#define DEBUG_TYPE "serialize-sil"
#include "swift/Strings.h"
#include "swift/SIL/ApplySite.h"
#include "swift/SIL/SILCloner.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SILOptimizer/PassManager/Passes.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/BasicBlockOptUtils.h"

using namespace swift;

namespace {
/// In place map opaque archetypes to their underlying type in a function.
/// This needs to happen when a function changes from serializable to not
/// serializable.
class MapOpaqueArchetypes : public SILCloner<MapOpaqueArchetypes> {
  using SuperTy = SILCloner<MapOpaqueArchetypes>;

  SILBasicBlock *origEntryBlock;
  SILBasicBlock *clonedEntryBlock;
public:
  friend class SILCloner<MapOpaqueArchetypes>;
  friend class SILCloner<MapOpaqueArchetypes>;
  friend class SILInstructionVisitor<MapOpaqueArchetypes>;

  MapOpaqueArchetypes(SILFunction &fun) : SuperTy(fun) {
    origEntryBlock = fun.getEntryBlock();
    clonedEntryBlock = fun.createBasicBlock();
  }

  SILType remapType(SILType Ty) {
    if (!Ty.getASTType()->hasOpaqueArchetype() ||
        !getBuilder()
             .getTypeExpansionContext()
             .shouldLookThroughOpaqueTypeArchetypes())
      return Ty;

    return getBuilder().getTypeLowering(Ty).getLoweredType().getCategoryType(
        Ty.getCategory());
  }

  CanType remapASTType(CanType ty) {
    if (!ty->hasOpaqueArchetype() ||
        !getBuilder()
             .getTypeExpansionContext()
             .shouldLookThroughOpaqueTypeArchetypes())
      return ty;
    // Remap types containing opaque result types in the current context.
    return getBuilder()
        .getTypeLowering(SILType::getPrimitiveObjectType(ty))
        .getLoweredType()
        .getASTType();
  }

  ProtocolConformanceRef remapConformance(Type ty,
                                          ProtocolConformanceRef conf) {
    auto context = getBuilder().getTypeExpansionContext();
    auto conformance = conf;
    if (ty->hasOpaqueArchetype() &&
        context.shouldLookThroughOpaqueTypeArchetypes()) {
      conformance =
          substOpaqueTypesWithUnderlyingTypes(conformance, ty, context);
    }
    return conformance;
  }

  void replace();
};
} // namespace

void MapOpaqueArchetypes::replace() {
  // Map the function arguments.
  SmallVector<SILValue, 4> entryArgs;
  entryArgs.reserve(origEntryBlock->getArguments().size());
  for (auto &origArg : origEntryBlock->getArguments()) {
    SILType mappedType = remapType(origArg->getType());
    auto *NewArg = clonedEntryBlock->createFunctionArgument(
        mappedType, origArg->getDecl(), true);
    entryArgs.push_back(NewArg);
  }

  getBuilder().setInsertionPoint(clonedEntryBlock);
  auto &fn = getBuilder().getFunction();
  cloneFunctionBody(&fn, clonedEntryBlock, entryArgs,
                    true /*replaceOriginalFunctionInPlace*/);
  // Insert the new entry block at the beginning.
  fn.getBlocks().splice(fn.getBlocks().begin(), fn.getBlocks(),
                        clonedEntryBlock);
  removeUnreachableBlocks(fn);
}

static bool opaqueArchetypeWouldChange(TypeExpansionContext context,
                                       CanType ty) {
  if (!ty->hasOpaqueArchetype())
    return false;

  return ty.findIf([=](Type type) -> bool {
    if (auto opaqueTy = type->getAs<OpaqueTypeArchetypeType>()) {
      auto opaque = opaqueTy->getDecl();
      auto module = context.getContext()->getParentModule();
      OpaqueSubstitutionKind subKind =
          ReplaceOpaqueTypesWithUnderlyingTypes::shouldPerformSubstitution(
              opaque, module, context.getResilienceExpansion());
      return subKind != OpaqueSubstitutionKind::DontSubstitute;
    }
    return false;
  });
}

static bool hasOpaqueArchetypeOperand(TypeExpansionContext context,
                                      SILInstruction &inst) {
  // Check the operands for opaque types.
  for (auto &opd : inst.getAllOperands())
    if (opaqueArchetypeWouldChange(context, opd.get()->getType().getASTType()))
      return true;
  return false;
}

static bool hasOpaqueArchetypeResult(TypeExpansionContext context,
                                     SILInstruction &inst) {
  // Check the results for opaque types.
  for (const auto &res : inst.getResults())
    if (opaqueArchetypeWouldChange(context, res->getType().getASTType()))
      return true;
  return false;
}

static bool hasOpaqueArchetype(TypeExpansionContext context,
                               SILInstruction &inst) {
  // Check operands and results.
  if (hasOpaqueArchetypeOperand(context, inst))
    return true;
  if (hasOpaqueArchetypeResult(context, inst))
    return true;

  // Check substitution maps.
  switch (inst.getKind()) {
  case SILInstructionKind::AllocStackInst:
  case SILInstructionKind::AllocRefInst:
  case SILInstructionKind::AllocRefDynamicInst:
  case SILInstructionKind::AllocValueBufferInst:
  case SILInstructionKind::AllocBoxInst:
  case SILInstructionKind::AllocExistentialBoxInst:
  case SILInstructionKind::IndexAddrInst:
  case SILInstructionKind::TailAddrInst:
  case SILInstructionKind::IndexRawPointerInst:
  case SILInstructionKind::FunctionRefInst:
  case SILInstructionKind::DynamicFunctionRefInst:
  case SILInstructionKind::PreviousDynamicFunctionRefInst:
  case SILInstructionKind::GlobalAddrInst:
  case SILInstructionKind::GlobalValueInst:
  case SILInstructionKind::IntegerLiteralInst:
  case SILInstructionKind::FloatLiteralInst:
  case SILInstructionKind::StringLiteralInst:
  case SILInstructionKind::ClassMethodInst:
  case SILInstructionKind::SuperMethodInst:
  case SILInstructionKind::ObjCMethodInst:
  case SILInstructionKind::ObjCSuperMethodInst:
  case SILInstructionKind::WitnessMethodInst:
  case SILInstructionKind::UpcastInst:
  case SILInstructionKind::AddressToPointerInst:
  case SILInstructionKind::PointerToAddressInst:
  case SILInstructionKind::UncheckedRefCastInst:
  case SILInstructionKind::UncheckedAddrCastInst:
  case SILInstructionKind::UncheckedTrivialBitCastInst:
  case SILInstructionKind::UncheckedBitwiseCastInst:
  case SILInstructionKind::RefToRawPointerInst:
  case SILInstructionKind::RawPointerToRefInst:
#define LOADABLE_REF_STORAGE(Name, ...)                                        \
  case SILInstructionKind::RefTo##Name##Inst:                                  \
  case SILInstructionKind::Name##ToRefInst:
#include "swift/AST/ReferenceStorage.def"
#undef LOADABLE_REF_STORAGE_HELPER
  case SILInstructionKind::ConvertFunctionInst:
  case SILInstructionKind::ConvertEscapeToNoEscapeInst:
  case SILInstructionKind::ThinFunctionToPointerInst:
  case SILInstructionKind::PointerToThinFunctionInst:
  case SILInstructionKind::RefToBridgeObjectInst:
  case SILInstructionKind::BridgeObjectToRefInst:
  case SILInstructionKind::BridgeObjectToWordInst:
  case SILInstructionKind::ThinToThickFunctionInst:
  case SILInstructionKind::ThickToObjCMetatypeInst:
  case SILInstructionKind::ObjCToThickMetatypeInst:
  case SILInstructionKind::ObjCMetatypeToObjectInst:
  case SILInstructionKind::ObjCExistentialMetatypeToObjectInst:
  case SILInstructionKind::UnconditionalCheckedCastValueInst:
  case SILInstructionKind::UnconditionalCheckedCastInst:
  case SILInstructionKind::ClassifyBridgeObjectInst:
  case SILInstructionKind::ValueToBridgeObjectInst:
  case SILInstructionKind::MarkDependenceInst:
  case SILInstructionKind::CopyBlockInst:
  case SILInstructionKind::CopyBlockWithoutEscapingInst:
  case SILInstructionKind::CopyValueInst:
#define UNCHECKED_REF_STORAGE(Name, ...)                                       \
  case SILInstructionKind::StrongCopy##Name##ValueInst:
#define ALWAYS_OR_SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name, ...)            \
  case SILInstructionKind::StrongCopy##Name##ValueInst:
#include "swift/AST/ReferenceStorage.def"
#undef UNCHECKED_REF_STORAGE
#undef ALWAYS_OR_SOMETIMES_LOADABLE_CHECKED_REF_STORAGE
  case SILInstructionKind::UncheckedOwnershipConversionInst:
  case SILInstructionKind::IsUniqueInst:
  case SILInstructionKind::IsEscapingClosureInst:
  case SILInstructionKind::LoadInst:
  case SILInstructionKind::LoadBorrowInst:
  case SILInstructionKind::BeginBorrowInst:
  case SILInstructionKind::StoreBorrowInst:
  case SILInstructionKind::BeginAccessInst:
#define NEVER_OR_SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name, name, ...)       \
  case SILInstructionKind::Load##Name##Inst:
#include "swift/AST/ReferenceStorage.def"
#undef NEVER_OR_SOMETIMES_LOADABLE_CHECKED_REF_STORAGE
  case SILInstructionKind::MarkUninitializedInst:
  case SILInstructionKind::ProjectValueBufferInst:
  case SILInstructionKind::ProjectBoxInst:
  case SILInstructionKind::ProjectExistentialBoxInst:
  case SILInstructionKind::BuiltinInst:
  case SILInstructionKind::MetatypeInst:
  case SILInstructionKind::ValueMetatypeInst:
  case SILInstructionKind::ExistentialMetatypeInst:
  case SILInstructionKind::ObjCProtocolInst:
  case SILInstructionKind::ObjectInst:
  case SILInstructionKind::TupleInst:
  case SILInstructionKind::TupleExtractInst:
  case SILInstructionKind::TupleElementAddrInst:
  case SILInstructionKind::StructInst:
  case SILInstructionKind::StructExtractInst:
  case SILInstructionKind::StructElementAddrInst:
  case SILInstructionKind::RefElementAddrInst:
  case SILInstructionKind::RefTailAddrInst:
  case SILInstructionKind::EnumInst:
  case SILInstructionKind::UncheckedEnumDataInst:
  case SILInstructionKind::InitEnumDataAddrInst:
  case SILInstructionKind::UncheckedTakeEnumDataAddrInst:
  case SILInstructionKind::SelectEnumInst:
  case SILInstructionKind::SelectEnumAddrInst:
  case SILInstructionKind::SelectValueInst:
  case SILInstructionKind::InitExistentialAddrInst:
  case SILInstructionKind::InitExistentialValueInst:
  case SILInstructionKind::OpenExistentialAddrInst:
  case SILInstructionKind::InitExistentialRefInst:
  case SILInstructionKind::OpenExistentialRefInst:
  case SILInstructionKind::InitExistentialMetatypeInst:
  case SILInstructionKind::OpenExistentialMetatypeInst:
  case SILInstructionKind::OpenExistentialBoxInst:
  case SILInstructionKind::OpenExistentialValueInst:
  case SILInstructionKind::OpenExistentialBoxValueInst:
  case SILInstructionKind::ProjectBlockStorageInst:
  case SILInstructionKind::InitBlockStorageHeaderInst:
  case SILInstructionKind::KeyPathInst:
  case SILInstructionKind::UnreachableInst:
  case SILInstructionKind::ReturnInst:
  case SILInstructionKind::ThrowInst:
  case SILInstructionKind::YieldInst:
  case SILInstructionKind::UnwindInst:
  case SILInstructionKind::BranchInst:
  case SILInstructionKind::CondBranchInst:
  case SILInstructionKind::SwitchValueInst:
  case SILInstructionKind::SwitchEnumInst:
  case SILInstructionKind::SwitchEnumAddrInst:
  case SILInstructionKind::DynamicMethodBranchInst:
  case SILInstructionKind::CheckedCastBranchInst:
  case SILInstructionKind::CheckedCastAddrBranchInst:
  case SILInstructionKind::CheckedCastValueBranchInst:
  case SILInstructionKind::DeallocStackInst:
  case SILInstructionKind::DeallocRefInst:
  case SILInstructionKind::DeallocPartialRefInst:
  case SILInstructionKind::DeallocValueBufferInst:
  case SILInstructionKind::DeallocBoxInst:
  case SILInstructionKind::DeallocExistentialBoxInst:
  case SILInstructionKind::StrongRetainInst:
  case SILInstructionKind::StrongReleaseInst:
  case SILInstructionKind::UnmanagedRetainValueInst:
  case SILInstructionKind::UnmanagedReleaseValueInst:
  case SILInstructionKind::UnmanagedAutoreleaseValueInst:
#define ALWAYS_OR_SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name, ...)            \
  case SILInstructionKind::StrongRetain##Name##Inst:                           \
  case SILInstructionKind::Name##RetainInst:                                   \
  case SILInstructionKind::Name##ReleaseInst:
#include "swift/AST/ReferenceStorage.def"
#undef ALWAYS_OR_SOMETIMES_LOADABLE_CHECKED_REF_STORAGE
  case SILInstructionKind::RetainValueInst:
  case SILInstructionKind::RetainValueAddrInst:
  case SILInstructionKind::ReleaseValueInst:
  case SILInstructionKind::ReleaseValueAddrInst:
  case SILInstructionKind::SetDeallocatingInst:
  case SILInstructionKind::AutoreleaseValueInst:
  case SILInstructionKind::BindMemoryInst:
  case SILInstructionKind::FixLifetimeInst:
  case SILInstructionKind::DestroyValueInst:
  case SILInstructionKind::EndBorrowInst:
  case SILInstructionKind::EndAccessInst:
  case SILInstructionKind::BeginUnpairedAccessInst:
  case SILInstructionKind::EndUnpairedAccessInst:
  case SILInstructionKind::StoreInst:
  case SILInstructionKind::AssignInst:
  case SILInstructionKind::AssignByWrapperInst:
  case SILInstructionKind::MarkFunctionEscapeInst:
  case SILInstructionKind::DebugValueInst:
  case SILInstructionKind::DebugValueAddrInst:
#define NEVER_OR_SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name, ...)             \
  case SILInstructionKind::Store##Name##Inst:
#include "swift/AST/ReferenceStorage.def"
  case SILInstructionKind::CopyAddrInst:
  case SILInstructionKind::DestroyAddrInst:
  case SILInstructionKind::EndLifetimeInst:
  case SILInstructionKind::InjectEnumAddrInst:
  case SILInstructionKind::DeinitExistentialAddrInst:
  case SILInstructionKind::DeinitExistentialValueInst:
  case SILInstructionKind::UnconditionalCheckedCastAddrInst:
  case SILInstructionKind::UncheckedRefCastAddrInst:
  case SILInstructionKind::AllocGlobalInst:
  case SILInstructionKind::EndApplyInst:
  case SILInstructionKind::AbortApplyInst:
  case SILInstructionKind::CondFailInst:
  case SILInstructionKind::DestructureStructInst:
  case SILInstructionKind::DestructureTupleInst:
    // Handle by operand and result check.
    break;

  case SILInstructionKind::ApplyInst:
  case SILInstructionKind::PartialApplyInst:
  case SILInstructionKind::TryApplyInst:
  case SILInstructionKind::BeginApplyInst:
    // Check substitution map.
    auto apply = ApplySite(&inst);
    auto subs = apply.getSubstitutionMap();
    for (auto ty: subs.getReplacementTypes()) {
       if (opaqueArchetypeWouldChange(context, ty->getCanonicalType()))
         return true;
    }
    break;
  }

  return false;
}

static bool hasOpaqueArchetypeArgument(TypeExpansionContext context, SILBasicBlock &BB) {
  for (auto *arg : BB.getArguments()) {
    if (opaqueArchetypeWouldChange(context, arg->getType().getASTType()))
      return true;
  }
  return false;
}

static bool hasAnyOpaqueArchetype(SILFunction &F) {
  bool foundOpaqueArchetype = false;
  auto context = F.getTypeExpansionContext();
  for (auto &BB : F) {
    // Check basic block argument types.
    if (hasOpaqueArchetypeArgument(context, BB)) {
      foundOpaqueArchetype = true;
      break;
    }

    // Check instruction results and operands.
    for (auto &inst : BB) {
      if (hasOpaqueArchetype(context, inst)) {
        foundOpaqueArchetype = true;
        break;
      }
    }

    if (foundOpaqueArchetype)
      break;
  }

  return foundOpaqueArchetype;
}

void updateOpaqueArchetypes(SILFunction &F) {
  // Only map if there are opaque archetypes that could change.
  if (!hasAnyOpaqueArchetype(F))
    return;

  MapOpaqueArchetypes(F).replace();
}

/// A utility pass to serialize a SILModule at any place inside the optimization
/// pipeline.
class SerializeSILPass : public SILModuleTransform {
  /// Removes [serialized] from all functions. This allows for more
  /// optimizations and for a better dead function elimination.
  void removeSerializedFlagFromAllFunctions(SILModule &M) {
    for (auto &F : M) {
      bool wasSerialized = F.isSerialized() != IsNotSerialized;
      F.setSerialized(IsNotSerialized);

      // We are removing [serialized] from the function. This will change how
      // opaque archetypes are lowered in SIL - they might lower to their
      // underlying type. Update the function's opaque archetypes.
      if (wasSerialized && F.isDefinition()) {
        updateOpaqueArchetypes(F);
        invalidateAnalysis(&F, SILAnalysis::InvalidationKind::Everything);
      }

      // After serialization we don't need to keep @alwaysEmitIntoClient
      // functions alive, i.e. we don't need to treat them as public functions.
      if (F.getLinkage() == SILLinkage::PublicNonABI && M.isWholeModule())
        F.setLinkage(SILLinkage::Shared);
    }

    for (auto &WT : M.getWitnessTables()) {
      WT.setSerialized(IsNotSerialized);
    }

    for (auto &VT : M.getVTables()) {
      VT.setSerialized(IsNotSerialized);
    }
  }

public:
  SerializeSILPass() {}
  void run() override {
    auto &M = *getModule();
    // Nothing to do if the module was serialized already.
    if (M.isSerialized())
      return;

    // Mark all reachable functions as "anchors" so that they are not
    // removed later by the dead function elimination pass. This
    // is required, because clients may reference any of the
    // serialized functions or anything referenced from them. Therefore,
    // to avoid linker errors, the object file of the current module should
    // contain all the symbols which were alive at the time of serialization.
    LLVM_DEBUG(llvm::dbgs() << "Serializing SILModule in SerializeSILPass\n");
    M.serialize();

    // If we are not optimizing, do not strip the [serialized] flag. We *could*
    // do this since after serializing [serialized] is irrelevent. But this
    // would incur an unnecessary compile time cost since if we are not
    // optimizing we are not going to perform any sort of DFE.
    if (!getOptions().shouldOptimize())
      return;
    removeSerializedFlagFromAllFunctions(M);
  }
};

SILTransform *swift::createSerializeSILPass() {
  return new SerializeSILPass();
}
