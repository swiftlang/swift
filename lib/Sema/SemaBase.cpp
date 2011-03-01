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

#include "swift/Sema/SemaBase.h"
#include "swift/Sema/Sema.h"
#include "swift/AST/ASTContext.h"
#include "llvm/Support/SourceMgr.h"
using namespace swift;

void SemaBase::Note(llvm::SMLoc Loc, const llvm::Twine &Message) {
  S.Context.SourceMgr.PrintMessage(Loc, Message, "note");
}
void SemaBase::Warning(llvm::SMLoc Loc, const llvm::Twine &Message) {
  S.Context.SourceMgr.PrintMessage(Loc, Message, "warning");
}
void SemaBase::Error(llvm::SMLoc Loc, const llvm::Twine &Message) {
  S.Context.setHadError();
  S.Context.SourceMgr.PrintMessage(Loc, Message, "error");
}
