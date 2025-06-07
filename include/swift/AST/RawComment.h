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

#include "swift/Basic/SourceLoc.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/StringRef.h"

namespace swift {

class SourceFile;
class SourceManager;

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
  unsigned ColumnIndent : 16;

  SingleRawComment(CharSourceRange Range, const SourceManager &SourceMgr);
  SingleRawComment(StringRef RawText, unsigned ColumnIndent);

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

  bool isGyb() const {
    return RawText.starts_with("// ###");
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

} // namespace swift

#endif // LLVM_SWIFT_AST_RAW_COMMENT_H

