//===--- SemaBase.cpp - Swift Language Semantic Analysis Utilities --------===//
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
//  This file implements the SemaBase interface
//
//===----------------------------------------------------------------------===//

#include "swift/Parse/SemaBase.h"
#include "swift/Parse/Sema.h"
#include "swift/AST/ASTContext.h"
#include "llvm/Support/SourceMgr.h"
using namespace swift;

void SemaBase::note(SMLoc Loc, const Twine &Message) {
  S.Context.SourceMgr.PrintMessage(Loc, Message, "note");
}
void SemaBase::warning(SMLoc Loc, const Twine &Message) {
  S.Context.SourceMgr.PrintMessage(Loc, Message, "warning");
}
void SemaBase::error(SMLoc Loc, const Twine &Message) {
  S.Context.setHadError();
  S.Context.SourceMgr.PrintMessage(Loc, Message, "error");
}
