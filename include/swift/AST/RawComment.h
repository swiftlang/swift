//===--- RawComment.h - Extraction of raw comments --------------*- C++ -*-===//
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

#ifndef SWIFT_AST_RAW_COMMENT_H
#define SWIFT_AST_RAW_COMMENT_H

#include "swift/Basic/Fingerprint.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/SourceLoc.h"
#include "swift/Basic/SourceManager.h"

namespace swift {

class SourceFile;

struct SingleRawComment {
  enum class CommentKind {
    OrdinaryLine,  ///< Any normal // comments
    OrdinaryBlock, ///< Any normal /* */ comment
    LineDoc,       ///< \code /// stuff \endcode
    BlockDoc,      ///< \code /** stuff */ \endcode
  };

  CharSourceRange Range;
  StringRef RawText;

  unsigned Kind : 8;
  unsigned StartColumn : 16;
  unsigned StartLine;
  unsigned EndLine;

  SingleRawComment(CharSourceRange Range, const SourceManager &SourceMgr);
  SingleRawComment(StringRef RawText, unsigned StartColumn);

  SingleRawComment(const SingleRawComment &) = default;
  SingleRawComment &operator=(const SingleRawComment &) = default;

  CommentKind getKind() const LLVM_READONLY {
    return static_cast<CommentKind>(Kind);
  }

  bool isOrdinary() const LLVM_READONLY {
    return getKind() == CommentKind::OrdinaryLine ||
           getKind() == CommentKind::OrdinaryBlock;
  }

  bool isLine() const LLVM_READONLY {
    return getKind() == CommentKind::OrdinaryLine ||
           getKind() == CommentKind::LineDoc;
  }
};

struct RawComment {
  ArrayRef<SingleRawComment> Comments;

  RawComment() {}
  RawComment(ArrayRef<SingleRawComment> Comments) : Comments(Comments) {}

  RawComment(const RawComment &) = default;
  RawComment &operator=(const RawComment &) = default;

  bool isEmpty() const {
    return Comments.empty();
  }

  CharSourceRange getCharSourceRange();
};

struct CommentInfo {
  StringRef Brief;
  RawComment Raw;
  uint32_t Group;
  uint32_t SourceOrder;
};

struct LineColumn {
  uint32_t Line = 0;
  uint32_t Column = 0;
  bool isValid() const { return Line && Column; }
};

struct BasicDeclLocs {
  StringRef SourceFilePath;
  SmallVector<std::pair<LineColumn, uint32_t>, 4> DocRanges;
  LineColumn Loc;
  LineColumn StartLoc;
  LineColumn EndLoc;
};

class BasicSourceFileInfo {
  /// If this is non-null, fields other than 'FilePath' hasn't been populated.
  /// The 'getInt()' part indicates this instance is constructed with a
  /// SourceFile.
  llvm::PointerIntPair<const SourceFile *, 1, bool> SFAndIsFromSF;

  StringRef FilePath;
  Fingerprint InterfaceHashIncludingTypeMembers = Fingerprint::ZERO();
  /// Does *not* include the type-body hashes of the top level types.
  /// Just the `SourceFile` hashes.
  /// Used for incremental imports.
  Fingerprint InterfaceHashExcludingTypeMembers = Fingerprint::ZERO();
  llvm::sys::TimePoint<> LastModified = {};
  uint64_t FileSize = 0;

  // Populate the from 'SF' member if exist. 'SF' will be cleared.
  void populateWithSourceFileIfNeeded();

public:
  BasicSourceFileInfo(StringRef FilePath,
                      Fingerprint InterfaceHashIncludingTypeMembers,
                      Fingerprint InterfaceHashExcludingTypeMembers,
                      llvm::sys::TimePoint<> LastModified, uint64_t FileSize)
      : FilePath(FilePath),
        InterfaceHashIncludingTypeMembers(InterfaceHashIncludingTypeMembers),
        InterfaceHashExcludingTypeMembers(InterfaceHashExcludingTypeMembers),
        LastModified(LastModified), FileSize(FileSize) {}

  ///  Construct with a `SourceFile`. `getInterfaceHashIncludingTypeMembers()`,
  ///  `getInterfaceHashExcludingTypeMembers()`, `getLastModified()` and `getFileSize()` are laizily
  ///  populated when accessed.
  BasicSourceFileInfo(const SourceFile *SF);

  bool isFromSourceFile() const;

  StringRef getFilePath() const { return FilePath; }

  Fingerprint getInterfaceHashIncludingTypeMembers() const {
    const_cast<BasicSourceFileInfo *>(this)->populateWithSourceFileIfNeeded();
    return InterfaceHashIncludingTypeMembers;
  }

  Fingerprint getInterfaceHashExcludingTypeMembers() const {
    const_cast<BasicSourceFileInfo *>(this)->populateWithSourceFileIfNeeded();
    return InterfaceHashExcludingTypeMembers;
  }

  llvm::sys::TimePoint<> getLastModified() const {
    const_cast<BasicSourceFileInfo *>(this)->populateWithSourceFileIfNeeded();
    return LastModified;
  }

  uint64_t getFileSize() const {
    const_cast<BasicSourceFileInfo *>(this)->populateWithSourceFileIfNeeded();
    return FileSize;
  }
};

} // namespace swift

#endif // LLVM_SWIFT_AST_RAW_COMMENT_H

