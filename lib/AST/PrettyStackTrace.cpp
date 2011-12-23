//===--- PrettyStackTrace.cpp - Swift-specific PrettyStackTraceEntries ----===//
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
//  This file implements several Swift-specific implementations of
//  PrettyStackTraceEntry.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/AST.h"
#include "swift/AST/PrettyStackTrace.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SourceMgr.h"

using namespace swift;

void PrettyStackTraceDecl::print(llvm::raw_ostream &out) const {
  out << What << " for ";
  if (!TheDecl) {
    out << "NULL declaration!\n";
    return;
  }

  if (NamedDecl *named = dyn_cast<NamedDecl>(TheDecl)) {
    out << named->getName().str();
  } else {
    out << "declaration";
  }
  out << " at ";
  SourceLoc loc = TheDecl->getLocStart();
  llvm::SourceMgr &srcMgr = TheDecl->getASTContext().SourceMgr;

  if (!loc.isValid()) {
    out << "invalid location\n";
    return;
  }

  int bufferForLoc = srcMgr.FindBufferContainingLoc(loc.Value);
  if (bufferForLoc == -1) {
    out << "location in unknown buffer\n";
    return;
  }

  out << srcMgr.getBufferInfo(bufferForLoc).Buffer->getBufferIdentifier()
      << ":" << srcMgr.FindLineNumber(loc.Value, bufferForLoc) << "\n";
}
