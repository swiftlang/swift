//===--- RewriteBufferEditsReceiver.cpp -----------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Migrator/RewriteBufferEditsReceiver.h"

using namespace swift;
using namespace swift::migrator;

void RewriteBufferEditsReceiver::insert(clang::SourceLocation ClangLoc,
                                                StringRef NewText) {
  auto Offset = ClangSourceManager.getFileOffset(ClangLoc);
  RewriteBuf.InsertText(Offset, NewText);
}

void RewriteBufferEditsReceiver::replace(clang::CharSourceRange ClangRange,
                                         StringRef ReplacementText) {
  auto StartOffset = ClangSourceManager.getFileOffset(ClangRange.getBegin());
  auto EndOffset = ClangSourceManager.getFileOffset(ClangRange.getEnd());
  auto Length = EndOffset - StartOffset;
  RewriteBuf.ReplaceText(StartOffset, Length, ReplacementText);
}

void RewriteBufferEditsReceiver::printResult(llvm::raw_ostream &OS) const {
  RewriteBuf.write(OS);
}
