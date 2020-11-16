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

SILUndef::SILUndef(SILType type)
    : ValueBase(ValueKind::SILUndef, type, IsRepresentative::Yes) {}

SILUndef *SILUndef::get(SILType ty, SILModule &m) {
  SILUndef *&entry = m.UndefValues[ty];
  if (entry == nullptr)
    entry = new (m) SILUndef(ty);
  return entry;
}

SILUndef *SILUndef::get(SILType ty, const SILFunction &f) {
  return SILUndef::get(ty, f.getModule());
}
