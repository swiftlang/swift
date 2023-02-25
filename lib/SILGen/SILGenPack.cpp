//===--- SILGenPack.cpp - Helper routines for lowering variadic packs -----===//
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

#include "SILGenFunction.h"
#include "Scope.h"
#include "swift/AST/GenericEnvironment.h"

using namespace swift;
using namespace Lowering;

namespace {
/// Cleanup to deallocate a now-uninitialized pack.
class DeallocPackCleanup : public Cleanup {
  SILValue Addr;
public:
  DeallocPackCleanup(SILValue addr) : Addr(addr) {}

  void emit(SILGenFunction &SGF, CleanupLocation l,
            ForUnwind_t forUnwind) override {
    SGF.B.createDeallocPack(l, Addr);
  }

  void dump(SILGenFunction &) const override {
#ifndef NDEBUG
    llvm::errs() << "DeallocPackCleanup\n"
                 << "State:" << getState() << "\n"
                 << "Addr:" << Addr << "\n";
#endif
  }
};

/// Cleanup to destroy all the values in a pack.
class DestroyPackCleanup : public Cleanup {
  SILValue Addr;
  CanPackType FormalPackType;
public:
  DestroyPackCleanup(SILValue addr, CanPackType formalPackType)
    : Addr(addr), FormalPackType(formalPackType) {}

  void emit(SILGenFunction &SGF, CleanupLocation l,
            ForUnwind_t forUnwind) override {
    SGF.emitDestroyPack(l, Addr, FormalPackType);
  }

  void dump(SILGenFunction &) const override {
#ifndef NDEBUG
    llvm::errs() << "DestroyPackCleanup\n"
                 << "State:" << getState() << "\n"
                 << "Addr:" << Addr << "\n"
                 << "FormalPackType:" << FormalPackType << "\n";
#endif
  }
};

/// Cleanup to destroy the preceding values in a pack-expansion
/// component of a pack.
class PartialDestroyPackCleanup : public Cleanup {
  SILValue Addr;
  unsigned PackComponentIndex;
  SILValue LimitWithinComponent;
  CanPackType FormalPackType;
public:
  PartialDestroyPackCleanup(SILValue addr,
                            CanPackType formalPackType,
                            unsigned packComponentIndex,
                            SILValue limitWithinComponent)
    : Addr(addr), PackComponentIndex(packComponentIndex),
      LimitWithinComponent(limitWithinComponent),
      FormalPackType(formalPackType) {}

  void emit(SILGenFunction &SGF, CleanupLocation l,
            ForUnwind_t forUnwind) override {
    SGF.emitPartialDestroyPack(l, Addr, FormalPackType, PackComponentIndex,
                               LimitWithinComponent);
  }

  void dump(SILGenFunction &) const override {
#ifndef NDEBUG
    llvm::errs() << "PartialDestroyPackCleanup\n"
                 << "State:" << getState() << "\n"
                 << "Addr:" << Addr << "\n"
                 << "FormalPackType:" << FormalPackType << "\n"
                 << "ComponentIndex:" << PackComponentIndex << "\n"
                 << "LimitWithinComponent:" << LimitWithinComponent << "\n";
#endif
  }
};
} // end anonymous namespace

CleanupHandle SILGenFunction::enterDeallocPackCleanup(SILValue temp) {
  assert(temp->getType().isAddress() &&  "dealloc must have an address type");
  assert(temp->getType().is<SILPackType>());
  Cleanups.pushCleanup<DeallocPackCleanup>(temp);
  return Cleanups.getTopCleanup();
}

CleanupHandle SILGenFunction::enterDestroyPackCleanup(SILValue addr,
                                                   CanPackType formalPackType) {
  Cleanups.pushCleanup<DestroyPackCleanup>(addr, formalPackType);
  return Cleanups.getTopCleanup();
}

CleanupHandle
SILGenFunction::enterPartialDestroyPackCleanup(SILValue addr,
                                               CanPackType formalPackType,
                                               unsigned packComponentIndex,
                                               SILValue limitWithinComponent) {
  Cleanups.pushCleanup<PartialDestroyPackCleanup>(addr, formalPackType,
                                                  packComponentIndex,
                                                  limitWithinComponent);
  return Cleanups.getTopCleanup();
}

void SILGenFunction::emitDestroyPack(SILLocation loc, SILValue packAddr,
                                     CanPackType formalPackType) {
  auto packTy = packAddr->getType().castTo<SILPackType>();

  // Destroy each of the elements of the pack.
  for (auto componentIndex : indices(packTy->getElementTypes())) {
    auto eltTy = packTy->getSILElementType(componentIndex);

    // If it's an expansion component, emit a "partial"-destroy loop.
    if (auto expansion = eltTy.getAs<PackExpansionType>()) {
      // We can skip this if the whole thing is trivial.
      auto &patternTypeTL = getTypeLowering(
              SILType::getPrimitiveAddressType(expansion.getPatternType()));
      if (patternTypeTL.isTrivial()) continue;

      emitPartialDestroyPack(loc, packAddr, formalPackType, componentIndex,
                             /*limit*/ nullptr);

    // If it's a scalar component, project and destroy it.
    } else {
      // We can skip this if the element is trivial.
      auto &eltTL = getTypeLowering(eltTy);
      if (eltTL.isTrivial()) continue;

      // Index into the pack and extract the component.
      auto packIndex =
        B.createScalarPackIndex(loc, componentIndex, formalPackType);
      auto eltAddr =
        B.createPackElementGet(loc, packIndex, packAddr, eltTy);
      B.createDestroyAddr(loc, eltAddr);
    }
  }
}

static bool isPatternInvariantToExpansion(CanType patternType,
                                          CanPackArchetypeType countArchetype) {
  return patternType.findIf([&](CanType type) {
    if (auto archetype = dyn_cast<PackArchetypeType>(type)) {
      return archetype == countArchetype ||
             archetype->getReducedShape() == countArchetype->getReducedShape();
    }
    return false;
  });
}

static std::pair<GenericEnvironment*, SILType>
deriveOpenedElementTypeForPackExpansion(SILGenModule &SGM,
                                        CanPackExpansionType expansion) {
  // If the pattern type is invariant to the expansion, we don't need
  // to open anything.
  auto countArchetype = cast<PackArchetypeType>(expansion.getCountType());
  auto patternType = expansion.getPatternType();
  if (isPatternInvariantToExpansion(patternType, countArchetype)) {
    return std::make_pair(nullptr,
                          SILType::getPrimitiveAddressType(patternType));
  }

  // Otherwise, make a new opened element environment for the signature
  // of the archetype we're expanding over.
  // TODO: consider minimizing this signature down to only what we need to
  // destroy the elements
  auto context =
    OpenedElementContext::createForContextualExpansion(SGM.getASTContext(),
                                                       expansion);
  auto elementType =
    context.environment->mapPackTypeIntoElementContext(patternType)
                       ->getCanonicalType();
  return std::make_pair(context.environment,
                        SILType::getPrimitiveAddressType(elementType));
}

void SILGenFunction::emitPartialDestroyPack(SILLocation loc, SILValue packAddr,
                                            CanPackType formalPackType,
                                            unsigned componentIndex,
                                            SILValue limitWithinComponent) {
  auto packTy = packAddr->getType().castTo<SILPackType>();
  auto packExpansionTy =
    cast<PackExpansionType>(packTy->getElementType(componentIndex));

  auto result = deriveOpenedElementTypeForPackExpansion(SGM, packExpansionTy);
  auto elementEnv = result.first;
  auto elementTy = result.second;

  emitDynamicPackLoop(loc, formalPackType, componentIndex, limitWithinComponent,
                      elementEnv, /*reverse*/ true,
                      [&](SILValue indexWithinComponent,
                          SILValue packExpansionIndex,
                          SILValue packIndex) {
    auto eltAddr = B.createPackElementGet(loc, packIndex, packAddr, elementTy);
    B.createDestroyAddr(loc, eltAddr);
  });
}

void SILGenFunction::emitDynamicPackLoop(SILLocation loc,
                                         CanPackType formalPackType,
                                         unsigned componentIndex,
                                         SILValue limitWithinComponent,
                                         GenericEnvironment *openedElementEnv,
                                         bool reverse,
                      llvm::function_ref<void(SILValue indexWithinComponent,
                                              SILValue packExpansionIndex,
                                              SILValue packIndex)> emitBody) {
  assert(isa<PackExpansionType>(formalPackType.getElementType(componentIndex)));
  ASTContext &ctx = SGM.getASTContext();

  auto wordTy = SILType::getBuiltinWordType(ctx);
  auto boolTy = SILType::getBuiltinIntegerType(1, ctx);
  auto zero = B.createIntegerLiteral(loc, wordTy, 0);
  auto one = B.createIntegerLiteral(loc, wordTy, 1);

  // The formal type of the component of the pack that we're iterating over.
  // If this isn't the entire pack, we'll dynamically index into just the
  // expansion component and then compose that into an index into the larger
  // pack.
  CanPackType formalDynamicPackType = formalPackType;
  bool needsSlicing = formalPackType->getNumElements() != 1;
  if (needsSlicing) {
    formalDynamicPackType =
      CanPackType::get(ctx, formalPackType.getElementType(componentIndex));
  }

  // If the caller didn't give us a limit, use the full length of the
  // pack expansion.
  if (!limitWithinComponent) {
    limitWithinComponent = B.createPackLength(loc, formalDynamicPackType);
  }

  // Branch to the loop condition block, passing the initial value
  // (0 if forward, the limit if reverse).
  auto condBB = createBasicBlock();
  B.createBranch(loc, condBB, { reverse ? limitWithinComponent : zero });

  // Condition block:
  B.emitBlock(condBB);
  auto incomingIndex = condBB->createPhiArgument(wordTy, OwnershipKind::None);

  // Branch to the end block if the incoming index value is equal to the
  // end index (the limit if forward, 0 if reverse).
  auto atEnd =
    B.createBuiltinBinaryFunction(loc, "cmp_eq", wordTy, boolTy,
                                  { incomingIndex,
                                    reverse ? zero : limitWithinComponent });
  auto bodyBB = createBasicBlock();
  auto endBB = createBasicBlockAfter(bodyBB);
  B.createCondBranch(loc, atEnd, endBB, bodyBB);

  // Body block:
  B.emitBlock(bodyBB);

  // The index to use in this iteration (the incoming index if forward,
  // the incoming index - 1 if reverse)
  SILValue curIndex = incomingIndex;
  if (reverse) {
    curIndex = B.createBuiltinBinaryFunction(loc, "sub", wordTy, wordTy,
                                             { incomingIndex, one });
  }

  // Construct the dynamic pack index into the component.
  SILValue packExpansionIndex =
    B.createDynamicPackIndex(loc, curIndex, formalDynamicPackType);

  // If there's an opened element environment, open it here.
  if (openedElementEnv) {
    B.createOpenPackElement(loc, packExpansionIndex, openedElementEnv);
  }

  // If there are multiple pack components in the overall pack, construct
  // the overall pack index.
  SILValue packIndex = packExpansionIndex;
  if (needsSlicing) {
    packIndex = B.createPackPackIndex(loc, componentIndex, packIndex,
                                      formalPackType);
  }

  // Emit the loop body in a scope as a convenience, since it's necessary
  // to avoid dominance problems anyway.
  {
    FullExpr scope(Cleanups, CleanupLocation(loc));
    emitBody(curIndex, packExpansionIndex, packIndex);
  }

  // The index to pass to the loop condition block (the current index + 1
  // if forward, the current index if reverse)
  SILValue outgoingIndex = curIndex;
  if (!reverse) {
    outgoingIndex = B.createBuiltinBinaryFunction(loc, "add", wordTy, wordTy,
                                                  { curIndex, one });
  }
  B.createBranch(loc, condBB, {outgoingIndex});

  // End block:
  B.emitBlock(endBB);
}
