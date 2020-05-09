//===--- SILUndef.cpp -----------------------------------------------------===//
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

#include "swift/SIL/SILUndef.h"
#include "swift/SIL/SILModule.h"

using namespace swift;

static ValueOwnershipKind getOwnershipKindForUndef(SILType type, const SILFunction &f) {
  if (type.isAddress() || type.isTrivial(f))
    return ValueOwnershipKind::None;
  return ValueOwnershipKind::Owned;
}

SILUndef::SILUndef(SILType type, ValueOwnershipKind ownershipKind)
    : ValueBase(ValueKind::SILUndef, type, IsRepresentative::Yes),
      ownershipKind(ownershipKind) {}

SILUndef *SILUndef::get(SILType ty, SILModule &m, ValueOwnershipKind ownershipKind) {
  SILUndef *&entry = m.UndefValues[std::make_pair(ty, unsigned(ownershipKind))];
  if (entry == nullptr)
    entry = new (m) SILUndef(ty, ownershipKind);
  return entry;
}

SILUndef *SILUndef::get(SILType ty, const SILFunction &f) {
  auto ownershipKind = getOwnershipKindForUndef(ty, f);
  return SILUndef::get(ty, f.getModule(), ownershipKind);
}
