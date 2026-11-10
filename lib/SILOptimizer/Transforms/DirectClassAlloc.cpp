//===--- DirectClassAlloc.cpp - Call class allocators directly ------------===//
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
// Instances of a class with a class allocator `A` (declared with
// `@allocator(A)` on the class or a superclass, or applied with
// `-default-class-allocator`) are allocated by `A.allocate` and freed by
// `A.deallocate`, which compiled code calls directly: the runtime never
// allocates such an instance. This mandatory pass rewrites the allocations
// and the deallocation in the deallocating deinit into calls of `A`.
//
// A heap allocation
//
//   %o = alloc_ref $C
//
// becomes
//
//   %cm = metatype $@thick C.Type
//   %am = metatype $@thick A.Type
//   %sz = builtin "classInstanceSize"<C>() : $Builtin.Word
//   %al = builtin "classInstanceAlignMask"<C>() : $Builtin.Word
//   %f  = function_ref @$ss22_allocateClassInstance...
//   %p  = apply %f<C, A>(%cm, %am, %sz, %al) : $Builtin.RawPointer
//   %o  = raw_pointer_to_ref %p : $Builtin.RawPointer to $C
//
// and `dealloc_ref %self : $C` in C's deallocating deinit becomes
//
//   apply _deallocateClassInstance<C, A>(%cm, %am, %rawself, %sz, %al)
//
// The stdlib helpers are `@_alwaysEmitIntoClient @inline(__always)`.
// `_allocateClassInstance` calls A.allocate and lets the runtime initialize
// the object header (swift_initHeapObject). `_deallocateClassInstance` lets
// the runtime do the bookkeeping that precedes a free
// (swift_deallocClassInstanceBegin) and, if no unowned references remain,
// calls A.deallocate. Otherwise the runtime frees the memory at the last
// unowned release, through the class's value witness table.
//
// With optimization, the pass runs at the start of the low-level function
// pipeline: after dead object elimination, so that allocations of objects
// that don't escape may be removed, and before the last inliner, so that the
// helpers and A's fast paths are specialized and inlined. It runs again, at
// every optimization level, before IRGen.
//
// The allocator of a class is always known where its instances are allocated
// and freed: in the module that declares the class, or in a client of a
// module built without library evolution, which records each class's
// allocator. In a module built with library evolution, only classes that
// clients outside the module's package can't see may have allocators. The
// other modules of the package allocate a package class's instances only if
// they access it without resilience (Package CMO), which relies on their
// being built with the module (see ClassDecl::getDirectlyCallableAllocatorType).
//
// Stack-promoted allocations (`alloc_ref [stack]`) aren't rewritten: a
// stack-promoted object never reaches the allocator. Neither is a
// `dealloc_ref` of a stack-promoted object, which stems from its devirtualized
// release (see ReleaseDevirtualizer). IRGen ignores both if it puts the object
// on the stack. If it can't (because of the function's stack budget, or a
// class layout that isn't fixed), it allocates the object with the allocator,
// through the allocator's allocate witness (see ClassAllocatorWitnesses), and
// lowers the `dealloc_ref` like the deallocating deinit. An object whose
// release isn't devirtualized is freed by its deallocating deinit, which frees
// only heap objects through the allocator.
//
// The ReleaseDevirtualizer calls the destroying deinit of a stack-promoted
// object, followed by a `dealloc_ref`, if it can. Otherwise (if the destroying
// deinit isn't visible) it calls the deallocating deinit, which, once
// rewritten and inlined, calls swift_deallocClassInstanceBegin for the object.
// That call is replaced by a `dealloc_ref` too, so that a stack object costs
// the same either way:
//
//   %d = begin_dealloc_ref %o of %a    // %a = alloc_ref [stack]
//   %r = ref_to_raw_pointer %d
//   %p = struct $UnsafeMutableRawPointer (%r)
//   %b = apply @swift_deallocClassInstanceBegin(%p)
//
// becomes
//
//   %d = begin_dealloc_ref %o of %a
//   dealloc_ref %d
//   %b = enum $Optional<UnsafeMutableRawPointer>, #Optional.none
//
// Not rewritten: `[objc]` allocations, allocations with tail elements and
// `alloc_ref_dynamic`, which can't allocate a class with an allocator.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "direct-class-alloc"

#include "swift/AST/ASTContext.h"
#include "swift/AST/Builtins.h"
#include "swift/AST/Decl.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/Basic/Assertions.h"
#include "swift/SIL/ClassAllocatorWitnesses.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/InstructionDeleter.h"
#include "swift/SILOptimizer/Utils/SILOptFunctionBuilder.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"

using namespace swift;

STATISTIC(NumDirectClassAllocs,
          "Number of alloc_refs rewritten into direct allocator calls");
STATISTIC(NumDirectClassDeallocs,
          "Number of dealloc_refs rewritten into direct deallocator calls");
STATISTIC(NumStackObjectDeallocBegins,
          "Number of swift_deallocClassInstanceBegin calls for stack-promoted "
          "objects replaced by dealloc_refs");

namespace {

class DirectClassAlloc : public SILFunctionTransform {
  SILFunction *allocHelper = nullptr;   // _allocateClassInstance
  SILFunction *deallocHelper = nullptr; // _deallocateClassInstance
  bool lookedUpHelpers = false;

  SILFunction *lookUpStdlibFunction(StringRef name) {
    SILModule &M = getFunction()->getModule();
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

  /// Find the stdlib helpers once per pass instance. Returns false if the
  /// stdlib doesn't provide them (in which case Sema rejects `@allocator`).
  bool lookUpHelpers() {
    if (!lookedUpHelpers) {
      lookedUpHelpers = true;
      allocHelper = lookUpStdlibFunction("_allocateClassInstance");
      deallocHelper = lookUpStdlibFunction("_deallocateClassInstance");
    }
    return allocHelper && deallocHelper;
  }

  /// The allocator type to call directly for \p ari, or null if \p ari
  /// isn't rewritten.
  CanType getAllocatorType(AllocRefInst *ari) {
    if (ari->isObjC() || !ari->getTailAllocatedCounts().empty())
      return CanType();
    // IRGen allocates a stack-promoted object with the allocator if it can't
    // put it on the stack.
    if (ari->canAllocOnStack())
      return CanType();
    return getAllocatorType(ari->getType().getASTType());
  }

  /// Whether \p dri deallocates a stack-promoted object, which IRGen frees
  /// only if it couldn't put the object on the stack.
  static bool isStackObjectDealloc(DeallocRefInst *dri) {
    SILValue object = dri->getOperand();
    if (auto *beginDealloc = dyn_cast<BeginDeallocRefInst>(object))
      object = beginDealloc->getAllocation();
    auto *ari = dyn_cast<AllocRefInstBase>(object);
    return ari && ari->canAllocOnStack();
  }

  /// If \p ai calls swift_deallocClassInstanceBegin for a stack-promoted
  /// instance of a class with a class allocator (from an inlined deallocating
  /// deinit), the reference to deallocate instead: the object's
  /// begin_dealloc_ref, or its alloc_ref.
  SILValue getDeallocBeginStackObject(ApplyInst *ai) {
    auto *callee = ai->getReferencedFunctionOrNull();
    if (!callee || callee->getName() != "swift_deallocClassInstanceBegin" ||
        ai->getNumArguments() != 1)
      return SILValue();
    SILValue pointer = ai->getArgument(0);
    if (auto *si = dyn_cast<StructInst>(pointer)) {
      if (si->getNumOperands() != 1)
        return SILValue();
      pointer = si->getOperand(0);
    }
    auto *rawPointer = dyn_cast<RefToRawPointerInst>(pointer);
    if (!rawPointer)
      return SILValue();
    SILValue object = rawPointer->getOperand();
    while (isa<UpcastInst>(object) || isa<UncheckedRefCastInst>(object) ||
           isa<EndInitLetRefInst>(object))
      object = cast<SingleValueInstruction>(object)->getOperand(0);
    SILValue allocation = object;
    if (auto *beginDealloc = dyn_cast<BeginDeallocRefInst>(object))
      allocation = beginDealloc->getAllocation();
    auto *ari = dyn_cast<AllocRefInst>(allocation);
    if (!ari || !ari->canAllocOnStack() || ari->isObjC() ||
        !ari->getTailAllocatedCounts().empty() ||
        !getAllocatorType(ari->getType().getASTType()))
      return SILValue();
    return object;
  }

  void replaceDeallocBegin(ApplyInst *ai, SILValue object) {
    SILLocation loc = ai->getLoc();
    SILBuilderWithScope B(ai);
    B.createDeallocRef(loc, object);
    SILValue none = B.createOptionalNone(loc, ai->getType());
    SILInstruction *pointer = ai->getArgument(0)->getDefiningInstruction();
    ai->replaceAllUsesWith(none);
    ai->eraseFromParent();
    InstructionDeleter deleter;
    deleter.trackIfDead(pointer);
    deleter.cleanupDeadInstructions();
    ++NumStackObjectDeallocBegins;
  }

  /// The allocator type to call directly for allocations and frees of
  /// \p classTy in this function, or null.
  CanType getAllocatorType(CanType classTy) {
    ClassDecl *cls = classTy.getClassDecl();
    if (!cls || classTy->hasTypeParameter())
      return CanType();
    Type allocatorType = cls->getDirectlyCallableAllocatorType(
        getFunction()->getModule().getSwiftModule());
    if (!allocatorType)
      return CanType();
    if (!lookUpHelpers())
      return CanType();
    return allocatorType->getCanonicalType();
  }

  struct HelperArgs {
    SILValue classMetatype, allocatorMetatype, size, alignMask;
    SubstitutionMap subs;
  };

  /// C.self, A.self, the instance size and alignment mask, and the <C, A>
  /// substitutions for \p helper, emitted at \p B's insertion point.
  HelperArgs emitHelperArgs(SILBuilder &B, SILLocation loc, CanType classTy,
                            CanType allocatorTy, SILFunction *helper) {
    ASTContext &ctx = B.getASTContext();
    auto thickMetatype = [&](CanType ty) {
      return SILType::getPrimitiveObjectType(
          CanMetatypeType::get(ty, MetatypeRepresentation::Thick));
    };
    HelperArgs args;
    args.classMetatype = B.createMetatype(loc, thickMetatype(classTy));
    args.allocatorMetatype = B.createMetatype(loc, thickMetatype(allocatorTy));
    SILType wordTy = SILType::getBuiltinWordType(ctx);
    auto builtinSubs = [&](StringRef name) {
      auto *decl = cast<FuncDecl>(getBuiltinValueDecl(ctx, name));
      return SubstitutionMap::get(decl->getGenericSignature(), {classTy},
                                  LookUpConformanceInModule());
    };
    args.size = B.createBuiltin(loc, ctx.getIdentifier("classInstanceSize"),
                                wordTy, builtinSubs("classInstanceSize"), {});
    args.alignMask =
        B.createBuiltin(loc, ctx.getIdentifier("classInstanceAlignMask"),
                        wordTy, builtinSubs("classInstanceAlignMask"), {});
    args.subs = SubstitutionMap::get(
        helper->getLoweredFunctionType()->getInvocationGenericSignature(),
        {classTy, allocatorTy}, LookUpConformanceInModule());
    return args;
  }

  void rewriteAlloc(AllocRefInst *ari, CanType allocatorTy) {
    SILLocation loc = ari->getLoc();
    CanType classTy = ari->getType().getASTType();
    SILBuilderWithScope B(ari);
    HelperArgs args = emitHelperArgs(B, loc, classTy, allocatorTy, allocHelper);
    SILValue fn = B.createFunctionRef(loc, allocHelper);
    SILValue raw = B.createApply(loc, fn, args.subs,
                                 {args.classMetatype, args.allocatorMetatype,
                                  args.size, args.alignMask});
    SILValue object = B.createRawPointerToRef(loc, raw, ari->getType(),
                                              /*isImmortal=*/false);
    ari->replaceAllUsesWith(object);
    ari->eraseFromParent();
    ++NumDirectClassAllocs;
  }

  void rewriteDealloc(DeallocRefInst *dri, CanType allocatorTy) {
    SILLocation loc = dri->getLoc();
    SILValue object = dri->getOperand();
    CanType classTy = object->getType().getASTType();
    SILBuilderWithScope B(dri);
    HelperArgs args =
        emitHelperArgs(B, loc, classTy, allocatorTy, deallocHelper);
    SILValue raw = B.createRefToRawPointer(
        loc, object, SILType::getRawPointerType(B.getASTContext()));
    SILValue fn = B.createFunctionRef(loc, deallocHelper);
    B.createApply(loc, fn, args.subs,
                  {args.classMetatype, args.allocatorMetatype, raw, args.size,
                   args.alignMask});
    dri->eraseFromParent();
    ++NumDirectClassDeallocs;
  }

  void run() override {
    SILFunction *F = getFunction();
    // raw_pointer_to_ref produces an unowned value, so run after ownership
    // elimination. Embedded Swift does not support `@allocator`.
    if (F->hasOwnership() || F->getModule().getOptions().EmbeddedSwift)
      return;

    SmallVector<std::pair<ApplyInst *, SILValue>, 2> deallocBegins;
    SmallVector<std::pair<SILInstruction *, CanType>, 4> candidates;
    for (auto &block : *F) {
      for (auto &inst : block) {
        if (auto *ai = dyn_cast<ApplyInst>(&inst)) {
          if (SILValue object = getDeallocBeginStackObject(ai))
            deallocBegins.push_back({ai, object});
        } else if (auto *ari = dyn_cast<AllocRefInst>(&inst)) {
          if (CanType allocatorTy = getAllocatorType(ari))
            candidates.push_back({ari, allocatorTy});
        } else if (auto *dri = dyn_cast<DeallocRefInst>(&inst)) {
          if (isStackObjectDealloc(dri))
            continue;
          CanType classTy = dri->getOperand()->getType().getASTType();
          // IRGen leaves these to the concurrency runtime, which frees them
          // through the class's allocator.
          if (auto *cls = classTy.getClassDecl())
            if (isActorFreedByConcurrencyRuntime(
                    cls, F->getModule().getSwiftModule()))
              continue;
          if (CanType allocatorTy = getAllocatorType(classTy))
            candidates.push_back({dri, allocatorTy});
        }
      }
    }
    if (candidates.empty() && deallocBegins.empty())
      return;

    for (auto &deallocBegin : deallocBegins)
      replaceDeallocBegin(deallocBegin.first, deallocBegin.second);
    for (auto &candidate : candidates) {
      LLVM_DEBUG(llvm::dbgs() << "direct-class-alloc in " << F->getName()
                              << ": " << *candidate.first);
      if (auto *ari = dyn_cast<AllocRefInst>(candidate.first))
        rewriteAlloc(ari, candidate.second);
      else
        rewriteDealloc(cast<DeallocRefInst>(candidate.first), candidate.second);
    }
    invalidateAnalysis(SILAnalysis::InvalidationKind::CallsAndInstructions);
  }
};

} // end anonymous namespace

SILTransform *swift::createDirectClassAlloc() { return new DirectClassAlloc(); }
