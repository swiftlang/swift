//===--- EditorAdapter.h ----------------------------------------*- C++ -*-===//
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
//
// This class wraps a clang::edit::Commit, taking Swift source locations and
// ranges, transforming them to Clang source locations and ranges, and pushes
// them into the textual editing infrastructure. This is a temporary measure
// while lib/Syntax bringup is happening.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_MIGRATOR_EDITORADAPTER_H
#define SWIFT_MIGRATOR_EDITORADAPTER_H

#include "swift/Migrator/Replacement.h"
#include "clang/Basic/LangOptions.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Edit/Commit.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallSet.h"

namespace swift {

class SourceLoc;
class SourceRange;
class CharSourceRange;
class SourceManager;

namespace migrator {

class EditorAdapter {
  swift::SourceManager &SwiftSrcMgr;
  clang::SourceManager &ClangSrcMgr;

  /// This holds a mapping of identical buffers, one that exist in the
  /// SwiftSrcMgr and one that exists in the ClangSrcMgr.
  ///
  /// This is marked mutable because it is lazily populated internally
  /// in the `getClangFileIDForSwiftBufferID` method below.
  mutable llvm::SmallDenseMap<unsigned, clang::FileID> SwiftToClangBufferMap;

  /// Tracks a history of edits outside of the clang::edit::Commit collector
  /// below. That doesn't handle duplicate or redundant changes.
  mutable llvm::SmallSet<Replacement, 32> Replacements;

  bool CacheEnabled;

  /// A running transactional collection of basic edit operations.
  /// Clang uses this transaction concept to cancel a batch of edits due to
  /// incompatibilities, such as those due to macro expansions, but we don't
  /// have macros in Swift. However, as a temporary adapter API, we use this
  /// to keep things simple.
  clang::edit::Commit Edits;

  /// Translate a Swift SourceLoc using the SwiftSrcMgr to a
  /// clang::SourceLocation using the ClangSrcMgr.
  clang::SourceLocation translateSourceLoc(SourceLoc SwiftLoc) const;

  /// Translate a Swift SourceRange using the SwiftSrcMgr to a
  /// clang::SourceRange using the ClangSrcMgr.
  clang::SourceRange
  translateSourceRange(SourceRange SwiftSourceRange) const;

  /// Translate a Swift CharSourceRange using the SwiftSrcMgr to a
  /// clang::CharSourceRange using the ClangSrcMgr.
  clang::CharSourceRange
  translateCharSourceRange(CharSourceRange SwiftSourceSourceRange) const;

  /// Returns the buffer ID and absolute offset for a Swift SourceLoc.
  std::pair<unsigned, unsigned> getLocInfo(swift::SourceLoc Loc) const;

  /// Returns true if the replacement has already been booked. Otherwise,
  /// returns false and adds it to the replacement set.
  bool cacheReplacement(CharSourceRange Range, StringRef Text) const;

public:
  EditorAdapter(swift::SourceManager &SwiftSrcMgr,
                clang::SourceManager &ClangSrcMgr)
    : SwiftSrcMgr(SwiftSrcMgr), ClangSrcMgr(ClangSrcMgr), CacheEnabled(true),
      Edits(clang::edit::Commit(ClangSrcMgr, clang::LangOptions())) {}

  /// Lookup the BufferID in the SwiftToClangBufferMap. If it doesn't exist,
  /// copy the corresponding buffer into the ClangSrcMgr.
  clang::FileID getClangFileIDForSwiftBufferID(unsigned BufferID) const;

  bool insert(SourceLoc Loc, StringRef Text, bool AfterToken = false,
              bool BeforePreviousInsertions = false);

  bool insertAfterToken(SourceLoc Loc, StringRef Text,
                        bool BeforePreviousInsertions = false) {
    return insert(Loc, Text, /*AfterToken=*/true, BeforePreviousInsertions);
  }

  bool insertBefore(SourceLoc Loc, StringRef Text) {
    return insert(Loc, Text, /*AfterToken=*/false,
                  /*BeforePreviousInsertions=*/true);
  }

  bool insertFromRange(SourceLoc Loc, CharSourceRange Range,
                       bool AfterToken = false,
                       bool BeforePreviousInsertions = false);
  bool insertWrap(StringRef before, CharSourceRange Range, StringRef after);

  bool remove(CharSourceRange Range);

  bool replace(CharSourceRange Range, StringRef Text);
  bool replaceWithInner(CharSourceRange Range, CharSourceRange innerRange);
  bool replaceText(SourceLoc Loc, StringRef Text,
                   StringRef replacementText);

  bool insertFromRange(SourceLoc Loc, SourceRange TokenRange,
                       bool AfterToken = false,
                       bool BeforePreviousInsertions = false);
  bool insertWrap(StringRef before, SourceRange TokenRange, StringRef after);
  bool remove(SourceLoc TokenLoc);
  bool remove(SourceRange TokenRange);
  bool replace(SourceRange TokenRange, StringRef Text);
  bool replaceToken(SourceLoc TokenLoc, StringRef Text);
  bool replaceWithInner(SourceRange TokenRange, SourceRange TokenInnerRange);

  /// Return the batched edits encountered so far.
  const clang::edit::Commit &getEdits() const {
    return Edits;
  }
  void enableCache() { CacheEnabled = true; }
  void disableCache() { CacheEnabled = false; }
};

} // end namespace migrator
} // end namespace swift

#endif // SWIFT_MIGRATOR_EDITORADAPTER_H
