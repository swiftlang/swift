//===--- DirectClassAlloc.cpp - Inline calls to `@allocator` allocators ---===//
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
// Instances of a class declared with `@allocator(A)` are allocated by
// `A.allocate` and freed by `A.deallocate`. Without this pass, IRGen lowers
// their `alloc_ref` and `dealloc_ref` to runtime calls
// (swift_allocObjectMaybeClassAllocated and
// swift_deallocClassInstanceMaybeClassAllocated) that find A from the class
// metadata and call it through generic thunks. This pass instead calls A
// directly, so that its implementation -- typically an inlinable fast path --
// is inlined into the allocating initializer and the deallocating deinit.
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
// The stdlib helpers are `@_alwaysEmitIntoClient @inline(__always)`; the rest
// of the low-level function pipeline specializes and inlines them.
// `_deallocateClassInstance` lets the runtime do the bookkeeping that precedes
// a free (swift_deallocClassInstanceBegin) and, if no unowned references
// remain, calls A.deallocate. Otherwise the runtime frees the memory at the
// last unowned release, finding A from the metadata as usual.
//
// The rewrite is only correct if every allocation of a C instance and its free
// agree on A, so it is restricted to code in the module that declares both C
// and the `@allocator` that applies to C: other modules may have been compiled
// against a different version of C, and always use the runtime path, which
// finds the allocator recorded when C was compiled.
//
// Not rewritten (they keep the runtime path, which finds the same allocator):
// `alloc_ref [stack]` (a stack-promoted object never reaches the allocator),
// `[objc]`, tail-allocated elements, and `alloc_ref_dynamic`.
//
//===----------------------------------------------------------------------===//

#define DEBUG_TYPE "direct-class-alloc"

#include "swift/AST/ASTContext.h"
#include "swift/AST/Builtins.h"
#include "swift/AST/Decl.h"
#include "swift/AST/GenericSignature.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/Basic/Assertions.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"
#include "swift/SILOptimizer/Utils/SILOptFunctionBuilder.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"

using namespace swift;

STATISTIC(NumDirectClassAllocs,
          "Number of alloc_refs rewritten into direct allocator calls");
STATISTIC(NumDirectClassDeallocs,
          "Number of dealloc_refs rewritten into direct deallocator calls");

static llvm::cl::opt<bool> DisableDirectClassAlloc(
    "sil-disable-direct-class-alloc", llvm::cl::init(false),
    llvm::cl::desc("Allocate and free instances of classes with an "
                   "@allocator through the runtime"));

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
  /// stdlib does not provide them.
  bool lookUpHelpers() {
    if (!lookedUpHelpers) {
      lookedUpHelpers = true;
      allocHelper = lookUpStdlibFunction("_allocateClassInstance");
      deallocHelper = lookUpStdlibFunction("_deallocateClassInstance");
    }
    return allocHelper && deallocHelper;
  }

  /// The allocator type to call directly for \p ari, or null if the
  /// allocation must keep the runtime path.
  CanType getAllocatorType(AllocRefInst *ari) {
    if (ari->isAllocatingStack() || ari->isObjC() ||
        !ari->getTailAllocatedCounts().empty())
      return CanType();
    return getAllocatorType(ari->getType().getASTType());
  }

  /// The allocator type to call directly for allocations and frees of
  /// \p classTy in this function, or null.
  CanType getAllocatorType(CanType classTy) {
    ClassDecl *cls = classTy.getClassDecl();
    if (!cls || classTy->hasTypeParameter())
      return CanType();
    Type allocatorType = cls->getAllocatorType();
    if (!allocatorType)
      return CanType();
    // Only the module that declares the class and its allocator knows that
    // every allocation and free of the class's instances agrees on it.
    ModuleDecl *module = getFunction()->getModule().getSwiftModule();
    if (cls->getModuleContext() != module ||
        cls->getAllocatorDeclaringClass()->getModuleContext() != module)
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
    if (DisableDirectClassAlloc)
      return;
    SILFunction *F = getFunction();
    // raw_pointer_to_ref produces an unowned value, so run after ownership
    // elimination. Embedded Swift does not support `@allocator`.
    if (F->hasOwnership() || F->getModule().getOptions().EmbeddedSwift)
      return;

    SmallVector<std::pair<SILInstruction *, CanType>, 4> candidates;
    for (auto &block : *F) {
      for (auto &inst : block) {
        if (auto *ari = dyn_cast<AllocRefInst>(&inst)) {
          if (CanType allocatorTy = getAllocatorType(ari))
            candidates.push_back({ari, allocatorTy});
        } else if (auto *dri = dyn_cast<DeallocRefInst>(&inst)) {
          if (CanType allocatorTy =
                  getAllocatorType(dri->getOperand()->getType().getASTType()))
            candidates.push_back({dri, allocatorTy});
        }
      }
    }
    if (candidates.empty())
      return;

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
