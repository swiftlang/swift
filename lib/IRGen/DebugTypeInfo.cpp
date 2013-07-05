//===--- DebugTypeInfo.h - Type Info for Debugging --------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the data structure that holds all the debug info
// we want to emit for types.
//
//===----------------------------------------------------------------------===//

#include "DebugTypeInfo.h"
#include "IRGen.h"
#include "FixedTypeInfo.h"

using namespace swift;
using namespace irgen;

DebugTypeInfo::DebugTypeInfo(CanType CTy, uint64_t Size, uint64_t Align)
  : CanTy(CTy), SizeInBits(Size), AlignmentInBits(Align) {
}

DebugTypeInfo::DebugTypeInfo(CanType CTy, Size Size, Alignment Align)
  : CanTy(CTy), SizeInBits(Size.getValue()), AlignmentInBits(Align.getValue()) {
}

DebugTypeInfo::DebugTypeInfo(CanType CTy, const TypeInfo &Info)
  : CanTy(CTy) {
  if (Info.isFixedSize()) {
    const FixedTypeInfo &FixTy = *cast<const FixedTypeInfo>(&Info);
    SizeInBits = FixTy.getFixedSize().getValue();
    AlignmentInBits = FixTy.getBestKnownAlignment().getValue();
  }
}

DebugTypeInfo::DebugTypeInfo(const ValueDecl &Decl, const TypeInfo &Info)
  : CanTy(Decl.getType()->getCanonicalType()) {
  // Same as above.
  if (Info.isFixedSize()) {
    const FixedTypeInfo &FixTy = *cast<const FixedTypeInfo>(&Info);
    SizeInBits = FixTy.getFixedSize().getValue();
    AlignmentInBits = FixTy.getBestKnownAlignment().getValue();
  }
}


bool DebugTypeInfo::operator==(DebugTypeInfo T) const {
  return CanTy == T.CanTy
    && SizeInBits == T.SizeInBits
    && AlignmentInBits == T.AlignmentInBits;
}

bool DebugTypeInfo::operator!=(DebugTypeInfo T) const {
  return !operator==(T);
}
