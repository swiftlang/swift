//===--- TerminatorUtils.h ------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// \file
///
/// ADTs for working with various forms of terminators.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_TERMINATORUTILS_H
#define SWIFT_SIL_TERMINATORUTILS_H

#include "swift/Basic/LLVM.h"
#include "swift/SIL/SILInstruction.h"

#include "llvm/ADT/PointerUnion.h"

namespace swift {

/// An ADT for writing generic code against SwitchEnumAddrInst and
/// SwitchEnumInst.
///
/// We use this instead of SwitchEnumInstBase for this purpose in order to avoid
/// the need for templating SwitchEnumInstBase from causing this ADT type of
/// usage to require templates.
class SwitchEnumTermInst {
  PointerUnion<SwitchEnumAddrInst *, SwitchEnumInst *> value;

public:
  SwitchEnumTermInst(SwitchEnumAddrInst *seai) : value(seai) {}
  SwitchEnumTermInst(SwitchEnumInst *seai) : value(seai) {}
  SwitchEnumTermInst(SILInstruction *i) : value(nullptr) {
    if (auto *seai = dyn_cast<SwitchEnumAddrInst>(i)) {
      value = seai;
      return;
    }

    if (auto *sei = dyn_cast<SwitchEnumInst>(i)) {
      value = sei;
      return;
    }
  }

  SwitchEnumTermInst(const SILInstruction *i)
      : SwitchEnumTermInst(const_cast<SILInstruction *>(i)) {}

  operator TermInst *() const {
    if (auto *seai = value.dyn_cast<SwitchEnumAddrInst *>())
      return seai;
    return cast<SwitchEnumInst *>(value);
  }

  TermInst *operator*() const {
    if (auto *seai = value.dyn_cast<SwitchEnumAddrInst *>())
      return seai;
    return cast<SwitchEnumInst *>(value);
  }

  TermInst *operator->() const {
    if (auto *seai = value.dyn_cast<SwitchEnumAddrInst *>())
      return seai;
    return cast<SwitchEnumInst *>(value);
  }

  operator bool() const { return bool(value); }

  SILValue getOperand() {
    if (auto *sei = value.dyn_cast<SwitchEnumInst *>())
      return sei->getOperand();
    return cast<SwitchEnumAddrInst *>(value)->getOperand();
  }

  unsigned getNumCases() {
    if (auto *sei = value.dyn_cast<SwitchEnumInst *>())
      return sei->getNumCases();
    return cast<SwitchEnumAddrInst *>(value)->getNumCases();
  }

  std::pair<EnumElementDecl *, SILBasicBlock *> getCase(unsigned i) const {
    if (auto *sei = value.dyn_cast<SwitchEnumInst *>())
      return sei->getCase(i);
    return cast<SwitchEnumAddrInst *>(value)->getCase(i);
  }

  SILBasicBlock *getCaseDestination(EnumElementDecl *decl) const {
    if (auto *sei = value.dyn_cast<SwitchEnumInst *>())
      return sei->getCaseDestination(decl);
    return cast<SwitchEnumAddrInst *>(value)->getCaseDestination(decl);
  }

  ProfileCounter getCaseCount(unsigned i) const {
    if (auto *sei = value.dyn_cast<SwitchEnumInst *>())
      return sei->getCaseCount(i);
    return cast<SwitchEnumAddrInst *>(value)->getCaseCount(i);
  }

  ProfileCounter getDefaultCount() const {
    if (auto *sei = value.dyn_cast<SwitchEnumInst *>())
      return sei->getDefaultCount();
    return cast<SwitchEnumAddrInst *>(value)->getDefaultCount();
  }

  bool hasDefault() const {
    if (auto *sei = value.dyn_cast<SwitchEnumInst *>())
      return sei->hasDefault();
    return cast<SwitchEnumAddrInst *>(value)->hasDefault();
  }

  SILBasicBlock *getDefaultBB() const {
    if (auto *sei = value.dyn_cast<SwitchEnumInst *>())
      return sei->getDefaultBB();
    return cast<SwitchEnumAddrInst *>(value)->getDefaultBB();
  }

  NullablePtr<SILBasicBlock> getDefaultBBOrNull() const {
    if (auto *sei = value.dyn_cast<SwitchEnumInst *>())
      return sei->getDefaultBBOrNull();
    return cast<SwitchEnumAddrInst *>(value)->getDefaultBBOrNull();
  }

  /// If the default refers to exactly one case decl, return it.
  NullablePtr<EnumElementDecl> getUniqueCaseForDefault() const {
    if (auto *sei = value.dyn_cast<SwitchEnumInst *>())
      return sei->getUniqueCaseForDefault();
    return cast<SwitchEnumAddrInst *>(value)->getUniqueCaseForDefault();
  }

  /// If the given block only has one enum element decl matched to it,
  /// return it.
  NullablePtr<EnumElementDecl>
  getUniqueCaseForDestination(SILBasicBlock *BB) const {
    if (auto *sei = value.dyn_cast<SwitchEnumInst *>())
      return sei->getUniqueCaseForDestination(BB);
    return cast<SwitchEnumAddrInst *>(value)->getUniqueCaseForDestination(BB);
  }
};

} // namespace swift

#endif
