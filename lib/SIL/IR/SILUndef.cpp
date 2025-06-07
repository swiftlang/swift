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

SILUndef::SILUndef(SILFunction *parent, SILType type)
    : ValueBase(ValueKind::SILUndef, type), parent(parent) {}

SILUndef *SILUndef::get(SILFunction *fn, SILType ty) {
  SILUndef *&entry = fn->undefValues[ty];
  if (entry == nullptr)
    entry = new (fn->getModule()) SILUndef(fn, ty);
  return entry;
}
