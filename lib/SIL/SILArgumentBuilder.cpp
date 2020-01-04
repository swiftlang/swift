//===--- SILArgumentBuilder.cpp -------------------------------------------===//
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

#include "swift/SIL/SILArgumentBuilder.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILUndef.h"

using namespace swift;

void SILArgumentBuilder::validateEntryBlock(SILArgumentKind kind) const {
  if (disableEntryBlockVerification)
    return;

  assert((kind != SILArgumentKind::SILFunctionArgument ||
          disableEntryBlockVerification || block->isEntry()) &&
         "Function Arguments can only be in the entry block");
  assert((kind == SILArgumentKind::SILFunctionArgument || !block->isEntry()) &&
         "Non function arguments can not be in the entry block");
}

/// A helper routine that dispatches a constructor call to the appropriate
/// non-function argument constructor.
template <typename... ArgTys>
SILArgument *
SILArgumentBuilder::constructArgumentInternal(SILArgumentKind argKind,
                                              ArgTys &&... argTys) {
  auto &mod = block->getModule();
  switch (argKind) {
  case SILArgumentKind::SILPhiArgument:
    return new (mod) SILPhiArgument(std::forward<ArgTys>(argTys)...);
  case SILArgumentKind::SILFunctionArgument:
    return new (mod)
        SILFunctionArgument(std::forward<ArgTys>(argTys)...);
  }
  llvm_unreachable("Covered switch isn't covered?!");
}

/// Replace the \p{i}th BB arg with a new BBArg with SILType \p Ty and
/// ValueDecl \p D.
///
/// NOTE: This assumes that the current argument in position \p i has had its
/// uses eliminated. To replace/replace all uses with, use
/// replacePhiArgumentAndRAUW.
SILArgument *SILArgumentBuilder::replaceArgument(
    SILArgumentKind argKind, unsigned i, SILType type,
    ValueOwnershipKind ownershipKind, const ValueDecl *decl) {
  validateEntryBlock(argKind);

  if (type.isTrivial(*block->getParent()))
    ownershipKind = ValueOwnershipKind::None;

  SILModule &mod = block->getModule();

  auto argIter = std::next(block->args_begin(), i);
  assert((*argIter)->use_empty() && "Expected no uses of the old BB arg!");

  // Notify the delete handlers that this argument is being deleted.
  mod.notifyDeleteHandlers(*argIter);

  auto *newArg = constructArgumentInternal(argKind, type, ownershipKind, decl);
  newArg->setParent(block);

  // TODO: When we switch to malloc/free allocation we'll be leaking memory
  // here.
  *argIter = newArg;

  return newArg;
}

/// Replace phi argument \p i and RAUW all uses.
SILArgument *SILArgumentBuilder::replaceArgumentAndReplaceAllUses(
    SILArgumentKind argKind, unsigned i, SILType type,
    ValueOwnershipKind ownershipKind, const ValueDecl *decl) {
  // Put in an undef placeholder before we do the replacement since
  // replacePhiArgument() expects the replaced argument to not have
  // any uses.
  SmallVector<Operand *, 16> operands;
  SILValue undef = SILUndef::get(type, *block->getParent());
  for (auto *use : block->getArgument(i)->getUses()) {
    use->set(undef);
    operands.push_back(use);
  }

  // Perform the replacement.
  auto *newArg = replaceArgument(argKind, i, type, ownershipKind, decl);

  // Wire back up the uses.
  while (!operands.empty()) {
    operands.pop_back_val()->set(newArg);
  }

  return newArg;
}

/// Allocate a new argument of type \p Ty and append it to the argument
/// list. Optionally you can pass in a value decl parameter.
SILArgument *
SILArgumentBuilder::createArgument(SILArgumentKind argKind, SILType type,
                                   ValueOwnershipKind ownershipKind,
                                   const ValueDecl *decl) {
  validateEntryBlock(argKind);
  if (type.isTrivial(*block->getParent()))
    ownershipKind = ValueOwnershipKind::None;
  return constructArgumentInternal(argKind, block, type, ownershipKind, decl);
}

/// Insert a new SILPhiArgument with type \p Ty and \p Decl at position \p
/// Pos.
SILArgument *SILArgumentBuilder::insertArgument(
    SILArgumentKind argKind, arg_iterator insertPt, SILType type,
    ValueOwnershipKind ownershipKind, const ValueDecl *decl) {
  validateEntryBlock(argKind);
  if (type.isTrivial(*block->getParent()))
    ownershipKind = ValueOwnershipKind::None;
  return constructArgumentInternal(argKind, block, insertPt, type, ownershipKind,
                                   decl);
}
