//===--- FunctionRefInfo.cpp - Function reference info --------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements the FunctionRefInfo class.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/FunctionRefInfo.h"
#include "swift/AST/DeclNameLoc.h"
#include "swift/AST/Identifier.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

FunctionRefInfo FunctionRefInfo::unapplied(DeclNameLoc nameLoc) {
  return FunctionRefInfo(ApplyLevel::Unapplied, nameLoc.isCompound(),
                         nameLoc.getModuleSelectorLoc().isValid());
}

FunctionRefInfo FunctionRefInfo::unapplied(DeclNameRef nameRef) {
  return FunctionRefInfo(ApplyLevel::Unapplied, nameRef.isCompoundName(),
                         nameRef.hasModuleSelector());
}

FunctionRefInfo FunctionRefInfo::addingApplicationLevel() const {
  auto withApply = [&]() {
    switch (getApplyLevel()) {
    case ApplyLevel::Unapplied:
      return ApplyLevel::SingleApply;
    case ApplyLevel::SingleApply:
    case ApplyLevel::DoubleApply:
      return ApplyLevel::DoubleApply;
    }
  };
  return FunctionRefInfo(withApply(), isCompoundName(), hasModuleSelector());
}

void FunctionRefInfo::dump(raw_ostream &os) const {
  switch (getApplyLevel()) {
  case ApplyLevel::Unapplied:
    os << "unapplied";
    break;
  case ApplyLevel::SingleApply:
    os << "single apply";
    break;
  case ApplyLevel::DoubleApply:
    os << "double apply";
    break;
  }
  if (isCompoundName() || hasModuleSelector()) {
    os << " (";
    if (isCompoundName())
      os << "compound";
    if (isCompoundName() && hasModuleSelector())
      os << ", ";
    if (hasModuleSelector())
      os << "module selector";
    os << ")";
  }
}

void FunctionRefInfo::dump() const {
  dump(llvm::errs());
}
