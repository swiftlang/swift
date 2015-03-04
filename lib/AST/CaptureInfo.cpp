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
  for (auto capture : getCaptures())
    if (capture.getDecl()->getDeclContext()->isLocalContext())
      return true;
  return false;
}


void CaptureInfo::
getLocalCaptures(SmallVectorImpl<CapturedValue> &Result) const {
  if (!hasLocalCaptures()) return;

  Result.reserve(Captures.size());

  // Filter out global variables.
  for (auto capture : Captures) {
    if (!capture.getDecl()->getDeclContext()->isLocalContext())
      continue;

    Result.push_back(capture);
  }
}

void CaptureInfo::dump() const {
  print(llvm::errs());
  llvm::errs() << '\n';
}

void CaptureInfo::print(raw_ostream &OS) const {
  OS << "captures=(";
  bool isFirst = true;
  
  for (auto capture : getCaptures()) {
    if (isFirst)
      isFirst = false;
    else
      OS << ", ";
    OS << capture.getDecl()->getName();
    
    if (capture.isDirect())
      OS << "<direct>";
  }
  OS << ')';
}

