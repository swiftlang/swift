//===--- ClassAllocatorWitnesses.cpp - Class allocator witnesses ----------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// For every class whose metadata this compilation emits and whose value
// witness table has a class allocator witness (see
// swift/SIL/ClassAllocatorWitnesses.h), make sure that this module has the
// function that is the `deallocate` witness for its instance allocator `A`, a
// `ClassAllocator`, and keep it alive.
//
// If `A` is a non-generic struct or enum whose witness for
// `deallocate(_:byteCount:alignment:for:)` is its own method, with the
// entry's lowering (getClassAllocatorDeallocateMethod), the entry is that
// method: the pass declares it if it's in another file or module, loads its
// body if it has no symbol (`@_alwaysEmitIntoClient`), so that this module
// emits its own copy, as it does for any such function it uses, and marks it
// used, so that neither dead function elimination nor LLVM removes it.
// Otherwise it creates a shared function with the entry's signature, which
// calls the witness:
//
//   sil shared [used] @__swift_class_allocator_deallocate_<A>
//       : $@convention(thin) (UnsafeMutableRawPointer, Int, Int,
//                             @thick any AnyObject.Type) -> () {
//   bb0(%allocation, %size, %alignment, %type):
//     %a = metatype $@thick A.Type
//     %f = function_ref @<A's witness>
//     %r = apply %f(%allocation, %size, %alignment, %type, %a)
//     return %r
//   }
//
// It calls the witness with a function_ref if it can
// (getDirectlyCallableClassAllocatorDeallocate), specialized for `A` if it's
// generic (a protocol extension's default implementation, or a method of a
// generic type), and otherwise through the conformance, with witness_method.
// With optimization, the witness is inlined into it. It's shared, so that
// each image has one copy per allocator type, and used, because only
// IRGen-emitted value witness tables and IRGen-emitted calls reference it.
//
// IRGen also calls the deallocate entry, and the allocate witness
// `__swift_class_allocator_allocate_<A>`, of the instance allocator of a class
// whose stack-promoted allocation (`alloc_ref [stack]`) it can't put on the
// stack: it allocates the object with the allocator, and frees it through the
// allocator if its release was devirtualized. So the pass creates them for
// the allocator of every class, of this module or not, that a function
// stack-promotes. The allocate witness calls the `@_alwaysEmitIntoClient`
// stdlib helper `_classAllocatorAllocate<A>`.
//
// The pass runs in the mandatory pipeline, before the performance pipeline,
// so that the functions are optimized with the rest of the module; after the
// last stack promotion of the mid-level pipeline, so that the witnesses that
// stack-promoted allocations need are optimized too; and again before IRGen,
// for SIL that didn't go through those pipelines or that was stack-promoted
// later.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "class-allocator-witnesses"

#include "swift/SIL/ClassAllocatorWitnesses.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ConcreteDeclRef.h"
#include "swift/AST/ConformanceLookup.h"
#include "swift/AST/Decl.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/Basic/Assertions.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILVTable.h"
#include "swift/SIL/TypeLowering.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/SILOptFunctionBuilder.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/Support/Debug.h"

using namespace swift;

namespace {

class ClassAllocatorWitnesses : public SILModuleTransform {
  bool createdWitness = false;

  SILFunction *lookUpStdlibFunction(StringRef name) {
    SILModule &M = *getModule();
    SmallVector<ValueDecl *, 1> results;
    M.getASTContext().lookupInSwiftModule(name, results);
    if (results.size() != 1)
      return nullptr;
    auto *decl = dyn_cast<FuncDecl>(results.front());
    if (!decl)
      return nullptr;
    SILOptFunctionBuilder funcBuilder(*this);
    SILFunction *fn = funcBuilder.getOrCreateFunction(
        SILLocation(decl), SILDeclRef(decl, SILDeclRef::Kind::Func),
        NotForDefinition);
    if (fn && fn->isExternalDeclaration())
      M.loadFunction(fn, SILModule::LinkingMode::LinkAll);
    if (!fn || fn->isExternalDeclaration())
      return nullptr;
    return fn;
  }

  /// Whether this compilation emits the metadata of \p cls.
  bool isDefinedHere(ClassDecl *cls) {
    SILModule &M = *getModule();
    if (cls->getModuleContext() != M.getSwiftModule())
      return false;
    if (M.isWholeModule())
      return true;
    return cls->getDeclContext()->getModuleScopeContext() ==
           M.getAssociatedContext();
  }

  /// Create an empty thin function \p name, with arguments of types
  /// \p argTypes, returning \p resultType (or nothing): a class allocator
  /// witness. Return null if the module already has it.
  SILFunction *createWitnessFunction(StringRef name, ArrayRef<SILType> argTypes,
                                     std::optional<SILType> resultType) {
    SILModule &M = *getModule();
    ASTContext &ctx = M.getASTContext();
    if (M.lookUpFunction(name))
      return nullptr;
    createdWitness = true;

    SmallVector<SILParameterInfo, 4> params;
    for (SILType argType : argTypes)
      params.push_back(SILParameterInfo(argType.getASTType(),
                                        ParameterConvention::Direct_Unowned));
    SmallVector<SILResultInfo, 1> results;
    if (resultType)
      results.push_back(
          SILResultInfo(resultType->getASTType(), ResultConvention::Unowned));
    auto fnType = SILFunctionType::get(
        GenericSignature(), SILFunctionType::ExtInfo::getThin(),
        SILCoroutineKind::None, ParameterConvention::Direct_Unowned, params,
        /*yields*/ {}, results, /*errorResult*/ std::nullopt, SubstitutionMap(),
        SubstitutionMap(), ctx);

    auto loc = RegularLocation::getAutoGeneratedLocation();
    SILOptFunctionBuilder funcBuilder(*this);
    SILFunction *fn = funcBuilder.createFunction(
        SILLinkage::Shared, name, fnType, ActorIsolation(),
        /*genericEnv*/ nullptr, loc, IsBare, IsNotTransparent, IsNotSerialized,
        IsNotDynamic, IsNotDistributed, IsNotRuntimeAccessible);
    // Referenced only from value witness tables and calls that IRGen emits.
    fn->setMarkedAsUsed(true);
    // After ownership elimination when the pass runs for stack-promoted
    // allocations, like the rest of the module.
    if (M.hasRegisteredDeserializationNotificationHandlerForAllFuncOME())
      fn->setOwnershipEliminated();
    fn->setDebugScope(new (M) SILDebugScope(loc, fn));
    return fn;
  }

  /// Create the allocate witness for \p allocatorTy, which calls \p helper,
  /// `_classAllocatorAllocate<A>`.
  void createAllocateWitness(CanType allocatorTy, SILFunction *helper) {
    SILModule &M = *getModule();
    ASTContext &ctx = M.getASTContext();
    SILType rawPtr = SILType::getRawPointerType(ctx);
    SILType word = SILType::getBuiltinWordType(ctx);
    SILFunction *fn = createWitnessFunction(
        getClassAllocatorWitnessName(allocatorTy,
                                     ClassAllocatorWitnessKind::Allocate),
        {rawPtr, word, word}, rawPtr);
    if (!fn)
      return;

    auto loc = RegularLocation::getAutoGeneratedLocation();
    SILBasicBlock *entry = fn->createBasicBlock();
    SILBuilder B(entry);
    B.setCurrentDebugScope(fn->getDebugScope());
    SmallVector<SILValue, 4> args;
    args.push_back(B.createMetatype(
        loc, SILType::getPrimitiveObjectType(CanMetatypeType::get(
                 allocatorTy, MetatypeRepresentation::Thick))));
    for (SILType argType : {rawPtr, word, word})
      args.push_back(entry->createFunctionArgument(argType));
    auto genericSig =
        helper->getLoweredFunctionType()->getInvocationGenericSignature();
    auto subs = SubstitutionMap::get(genericSig, {allocatorTy},
                                     LookUpConformanceInModule());
    SILValue result =
        B.createApply(loc, B.createFunctionRef(loc, helper), subs, args);
    B.createReturn(loc, result);
  }

  /// The function \p ref, declared if this module doesn't define it, with
  /// its body if it has no symbol; or null if this module can't reference it.
  SILFunction *getReferenceableFunction(SILDeclRef ref) {
    SILModule &M = *getModule();
    SILFunction *fn = M.lookUpFunction(ref);
    if (!fn) {
      SILOptFunctionBuilder funcBuilder(*this);
      fn = funcBuilder.getOrCreateFunction(
          RegularLocation::getAutoGeneratedLocation(), ref, NotForDefinition);
    }
    if (fn && !isReferenceableClassAllocatorFunction(fn))
      M.loadFunction(fn, SILModule::LinkingMode::LinkAll);
    if (!fn || !isReferenceableClassAllocatorFunction(fn))
      return nullptr;
    return fn;
  }

  /// Make sure that this module has the `deallocate` entry for \p allocatorTy
  /// (see lookUpClassAllocatorDeallocateEntry): the allocator's method, or a
  /// function that calls its witness.
  void createDeallocateEntry(CanType allocatorTy) {
    SILModule &M = *getModule();
    if (M.lookUpFunction(getClassAllocatorWitnessName(
            allocatorTy, ClassAllocatorWitnessKind::Deallocate)))
      return;
    if (auto method = getClassAllocatorDeallocateMethod(M, allocatorTy)) {
      if (SILFunction *fn = getReferenceableFunction(*method)) {
        // Referenced from value witness tables, which IRGen emits.
        if (!fn->markedAsUsed()) {
          fn->setMarkedAsUsed(true);
          createdWitness = true;
        }
        return;
      }
    }

    ASTContext &ctx = M.getASTContext();
    auto *requirement = getClassAllocatorDeallocateRequirement(ctx);
    if (!requirement)
      return;
    SILType argTypes[] = {
        SILType::getPrimitiveObjectType(
            ctx.getUnsafeMutableRawPointerType()->getCanonicalType()),
        SILType::getPrimitiveObjectType(ctx.getIntType()->getCanonicalType()),
        SILType::getPrimitiveObjectType(ctx.getIntType()->getCanonicalType()),
        SILType::getPrimitiveObjectType(CanExistentialMetatypeType::get(
            ctx.getAnyObjectType(), MetatypeRepresentation::Thick)),
    };
    SILFunction *fn = createWitnessFunction(
        getClassAllocatorWitnessName(allocatorTy,
                                     ClassAllocatorWitnessKind::Deallocate),
        argTypes, std::nullopt);
    if (!fn)
      return;

    auto loc = RegularLocation::getAutoGeneratedLocation();
    SILBasicBlock *entry = fn->createBasicBlock();
    SILBuilder B(entry);
    B.setCurrentDebugScope(fn->getDebugScope());
    SmallVector<SILValue, 5> args;
    for (SILType argType : argTypes)
      args.push_back(entry->createFunctionArgument(argType));

    // Call the witness directly if possible, and otherwise through the
    // conformance.
    SILValue callee;
    SubstitutionMap subs;
    SILType selfType;
    if (ConcreteDeclRef witness =
            getDirectlyCallableClassAllocatorDeallocate(M, allocatorTy)) {
      if (SILFunction *witnessFn =
              getReferenceableFunction(SILDeclRef(witness.getDecl()))) {
        auto substType = witnessFn->getLoweredFunctionType();
        if (!witness.getSubstitutions().empty())
          substType = substType->substGenericArgs(
              M, witness.getSubstitutions(), fn->getTypeExpansionContext());
        SILFunctionConventions conv(substType,
                                    SILAddressConventions::forFunction(*fn));
        auto context = fn->getTypeExpansionContext();
        bool matches = conv.getNumSILArguments() == 5 &&
                       substType->getNumResults() == 0 &&
                       !substType->hasErrorResult() &&
                       !substType->isCoroutine() && !substType->isAsync();
        for (unsigned i = 0; matches && i != 4; ++i)
          matches = conv.getSILArgumentType(i, context) == argTypes[i];
        if (matches) {
          callee = B.createFunctionRef(loc, witnessFn);
          subs = witness.getSubstitutions();
          selfType = conv.getSILArgumentType(4, context);
        }
      }
    }
    if (!callee) {
      auto conformance =
          lookupConformance(allocatorTy, cast<ProtocolDecl>(requirement->getDeclContext()));
      SILDeclRef member(requirement, SILDeclRef::Kind::Func);
      SILType methodType =
          M.Types.getConstantInfo(fn->getTypeExpansionContext(), member)
              .getSILType();
      callee = B.createWitnessMethod(loc, allocatorTy, conformance, member,
                                     methodType);
      subs = SubstitutionMap::getProtocolSubstitutions(
          cast<ProtocolDecl>(requirement->getDeclContext()), allocatorTy, conformance);
      selfType = SILType::getPrimitiveObjectType(
          CanMetatypeType::get(allocatorTy, MetatypeRepresentation::Thick));
    }
    args.push_back(B.createMetatype(loc, selfType));
    B.createApply(loc, callee, subs, args);
    B.createReturn(loc, B.createTuple(loc, {}));
  }

  void run() override {
    SILModule &M = *getModule();
    if (M.getOptions().EmbeddedSwift)
      return;
    createdWitness = false;

    // The allocators whose witnesses this compilation's tables reference.
    llvm::SetVector<CanType> instanceAllocators;
    for (SILVTable *vtable : M.getVTables()) {
      if (vtable->isSpecialized())
        continue;
      ClassDecl *cls = vtable->getClass();
      if (!isDefinedHere(cls))
        continue;
      if (CanType allocatorTy = getClassAllocatorWitnessType(cls))
        instanceAllocators.insert(allocatorTy);
    }
    // The instance allocators that IRGen calls if it can't put a
    // stack-promoted object on the stack.
    llvm::SetVector<CanType> stackObjectAllocators;
    for (SILFunction &F : M) {
      for (SILBasicBlock &block : F) {
        for (SILInstruction &inst : block) {
          auto *ari = dyn_cast<AllocRefInst>(&inst);
          if (!ari || !ari->canAllocOnStack() || ari->isObjC() ||
              !ari->getTailAllocatedCounts().empty())
            continue;
          if (auto *cls = ari->getType().getClassDecl())
            if (Type allocatorTy =
                    cls->getDirectlyCallableAllocatorType(M.getSwiftModule()))
              stackObjectAllocators.insert(allocatorTy->getCanonicalType());
        }
      }
    }
    if (instanceAllocators.empty() && stackObjectAllocators.empty())
      return;

    for (CanType allocatorTy : instanceAllocators) {
      LLVM_DEBUG(llvm::dbgs() << "class-allocator-witnesses: instance "
                              << allocatorTy << "\n");
      createDeallocateEntry(allocatorTy);
    }
    if (!stackObjectAllocators.empty()) {
      if (SILFunction *allocate =
              lookUpStdlibFunction("_classAllocatorAllocate")) {
        for (CanType allocatorTy : stackObjectAllocators) {
          LLVM_DEBUG(llvm::dbgs() << "class-allocator-witnesses: stack object "
                                  << allocatorTy << "\n");
          createAllocateWitness(allocatorTy, allocate);
          createDeallocateEntry(allocatorTy);
        }
      }
    }
    if (createdWitness)
      invalidateAll();
  }
};

} // end anonymous namespace

SILTransform *swift::createClassAllocatorWitnesses() {
  return new ClassAllocatorWitnesses();
}
