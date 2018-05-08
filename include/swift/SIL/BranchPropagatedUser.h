//===--- BranchPropagatedUser.h -------------------------------------------===//
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

#ifndef SWIFT_SIL_BRANCHPROPAGATEDUSER_H
#define SWIFT_SIL_BRANCHPROPAGATEDUSER_H

#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILInstruction.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/Support/PointerLikeTypeTraits.h"

namespace swift {

/// This is a class that models normal users and also cond_br users that are
/// associated with the block in the target block. This is safe to do since in
/// Semantic SIL, cond_br with non-trivial arguments are not allowed to have
/// critical edges. In non-semantic SIL, it is expected that any user of
/// BranchPropagatedUser and friends break all such critical edges.
class BranchPropagatedUser {
  using InnerTy = llvm::PointerIntPair<SILInstruction *, 1>;
  InnerTy User;

public:
  BranchPropagatedUser(SILInstruction *I) : User(I) {
    assert(!isa<CondBranchInst>(I));
  }

  BranchPropagatedUser(CondBranchInst *I) : User(I) {}

  BranchPropagatedUser(CondBranchInst *I, unsigned SuccessorIndex)
      : User(I, SuccessorIndex) {
    assert(SuccessorIndex == CondBranchInst::TrueIdx ||
           SuccessorIndex == CondBranchInst::FalseIdx);
  }

  BranchPropagatedUser(const BranchPropagatedUser &Other) : User(Other.User) {}
  BranchPropagatedUser &operator=(const BranchPropagatedUser &Other) {
    User = Other.User;
    return *this;
  }

  operator SILInstruction *() { return User.getPointer(); }
  operator const SILInstruction *() const { return User.getPointer(); }

  SILInstruction *getInst() const { return User.getPointer(); }

  SILBasicBlock *getParent() const {
    if (!isCondBranchUser()) {
      return getInst()->getParent();
    }

    auto *CBI = cast<CondBranchInst>(getInst());
    unsigned Number = getCondBranchSuccessorID();
    if (Number == CondBranchInst::TrueIdx)
      return CBI->getTrueBB();
    return CBI->getFalseBB();
  }

  bool isCondBranchUser() const {
    return isa<CondBranchInst>(User.getPointer());
  }

  unsigned getCondBranchSuccessorID() const {
    assert(isCondBranchUser());
    return User.getInt();
  }

  SILBasicBlock::iterator getIterator() const {
    return User.getPointer()->getIterator();
  }

  void *getAsOpaqueValue() const {
    return llvm::PointerLikeTypeTraits<InnerTy>::getAsVoidPointer(User);
  }

  static BranchPropagatedUser getFromOpaqueValue(void *p) {
    InnerTy TmpUser =
        llvm::PointerLikeTypeTraits<InnerTy>::getFromVoidPointer(p);
    if (auto *CBI = dyn_cast<CondBranchInst>(TmpUser.getPointer())) {
      return BranchPropagatedUser(CBI, TmpUser.getInt());
    }
    return BranchPropagatedUser(TmpUser.getPointer());
  }

  enum {
    NumLowBitsAvailable =
        llvm::PointerLikeTypeTraits<InnerTy>::NumLowBitsAvailable
  };
};

} // namespace swift

namespace llvm {

template <> struct PointerLikeTypeTraits<swift::BranchPropagatedUser> {
public:
  using BranchPropagatedUser = swift::BranchPropagatedUser;

  static void *getAsVoidPointer(BranchPropagatedUser v) {
    return v.getAsOpaqueValue();
  }

  static BranchPropagatedUser getFromVoidPointer(void *p) {
    return BranchPropagatedUser::getFromOpaqueValue(p);
  }

  enum { NumLowBitsAvailable = BranchPropagatedUser::NumLowBitsAvailable };
};

} // namespace llvm

#endif
