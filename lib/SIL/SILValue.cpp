//===--- SILValue.cpp - Implementation for SILValue -----------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/SIL/SILValue.h"
#include "swift/SIL/SILArgument.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                       Check SILValue Type Properties
//===----------------------------------------------------------------------===//

/// These are just for performance and verification. If one needs to make
/// changes that cause the asserts the fire, please update them. The purpose is
/// to prevent these predicates from changing values by mistake.
static_assert(std::is_standard_layout<SILValue>::value,
              "Expected SILValue to be standard layout");
static_assert(sizeof(SILValue) == sizeof(uintptr_t),
              "SILValue should be pointer sized");

//===----------------------------------------------------------------------===//
//                              Utility Methods
//===----------------------------------------------------------------------===//

SILBasicBlock *ValueBase::getParentBlock() const {
  auto *NonConstThis = const_cast<ValueBase *>(this);
  if (auto *Inst = dyn_cast<SILInstruction>(NonConstThis))
    return Inst->getParent();
  if (auto *Arg = dyn_cast<SILArgument>(NonConstThis))
    return Arg->getParent();
  return nullptr;
}

SILFunction *ValueBase::getFunction() const {
  auto *NonConstThis = const_cast<ValueBase *>(this);
  if (auto *Inst = dyn_cast<SILInstruction>(NonConstThis))
    return Inst->getFunction();
  if (auto *Arg = dyn_cast<SILArgument>(NonConstThis))
    return Arg->getFunction();
  return nullptr;
}

SILModule *ValueBase::getModule() const {
  auto *NonConstThis = const_cast<ValueBase *>(this);
  if (auto *Inst = dyn_cast<SILInstruction>(NonConstThis))
    return &Inst->getModule();
  if (auto *Arg = dyn_cast<SILArgument>(NonConstThis))
    return &Arg->getModule();
  return nullptr;
}
