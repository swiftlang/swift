//===--- CaptureInfo.cpp - Data Structure for Capture Lists ---------------===//
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

#include "swift/AST/CaptureInfo.h"
#include "swift/AST/Decl.h"
#include "llvm/Support/raw_ostream.h"

using namespace swift;

bool CaptureInfo::hasLocalCaptures() const {
  for (auto VD : getCaptures())
    if (VD->getDeclContext()->isLocalContext())
      return true;
  return false;
}

std::vector<ValueDecl *> CaptureInfo::getLocalCaptures() const {
  if (!hasLocalCaptures())
    return std::vector<ValueDecl *>();

  std::vector<ValueDecl *> Result;
  Result.reserve(Captures.size());

  // Filter out global variables.
  for (auto VD : Captures)
    if (VD->getDeclContext()->isLocalContext())
      Result.push_back(VD);

  return Result;
}

void CaptureInfo::dump() const {
  print(llvm::errs());
  llvm::errs() << '\n';
}

void CaptureInfo::print(raw_ostream &OS) const {
  OS << "captures=(";
  OS << getCaptures()[0]->getName();
  for (auto VD : getCaptures().slice(1)) {
    OS << ", " << VD->getName();
  }
  OS << ')';
}

