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

static ValueOwnershipKind getOwnershipKindForUndef(SILType type, SILModule &m) {
  if (type.isTrivial(m))
    return ValueOwnershipKind::Any;
  return ValueOwnershipKind::Owned;
}

SILUndef::SILUndef(SILType type, SILModule &m)
    : ValueBase(ValueKind::SILUndef, type, IsRepresentative::Yes),
      ownershipKind(getOwnershipKindForUndef(type, m)) {}

SILUndef *SILUndef::get(SILType ty, SILModule &m) {
  // Unique these.
  SILUndef *&entry = m.UndefValues[ty];
  if (entry == nullptr)
    entry = new (m) SILUndef(ty, m);
  return entry;
}
