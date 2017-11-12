//===--- RewriteBufferEditsReceiver.h ---------------------------*- C++ -*-===//
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

#ifndef SWIFT_MIGRATOR_REWRITEBUFFEREDITSRECEIVER_H
#define SWIFT_MIGRATOR_REWRITEBUFFEREDITSRECEIVER_H

#include "clang/Basic/SourceManager.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Edit/EditsReceiver.h"
#include "clang/Rewrite/Core/RewriteBuffer.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/raw_ostream.h"

using llvm::StringRef;

namespace swift {
namespace migrator {

/// An EditsReceiver that collects edits from an EditedSource and directly
/// applies it to a clang::RewriteBuffer.
class RewriteBufferEditsReceiver final : public clang::edit::EditsReceiver {
  const clang::SourceManager &ClangSourceManager;
  const clang::FileID InputFileID;
  const StringRef InputText;
  clang::RewriteBuffer RewriteBuf;
public:
  RewriteBufferEditsReceiver(const clang::SourceManager &ClangSourceManager,
                             const clang::FileID InputFileID,
                             const StringRef InputText)
    : ClangSourceManager(ClangSourceManager),
      InputFileID(InputFileID),
      InputText(InputText) {
    RewriteBuf.Initialize(InputText);
  }

  virtual void insert(clang::SourceLocation Loc, StringRef Text) override;
  virtual void replace(clang::CharSourceRange Range, StringRef Text) override;

  /// Print the result of all of the edits to the given output stream.
  void printResult(llvm::raw_ostream &OS) const;
};

} // end namespace migrator
} // end namespace swift

#endif // SWIFT_MIGRATOR_REWRITEBUFFEREDITSRECEIVER_H
