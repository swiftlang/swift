//===--- RawComment.h - Extraction of raw comments ------------------------===//
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

#ifndef SWIFT_AST_RAW_COMMENT_H
#define SWIFT_AST_RAW_COMMENT_H

#include "swift/Basic/LLVM.h"
#include "swift/Basic/SourceLoc.h"
#include "swift/Basic/SourceManager.h"

namespace swift {
struct SingleRawComment {
  enum class CommentKind {
    OrdinaryBCPL,      ///< Any normal BCPL comments
    OrdinaryC,         ///< Any normal C comment
    BCPLSlash,         ///< \code /// stuff \endcode
    JavaDoc,           ///< \code /** stuff */ \endcode
  };

  const CharSourceRange Range;
  const StringRef RawText;

  const CommentKind Kind;
  const unsigned StartLine;
  const unsigned EndLine;

  SingleRawComment(CharSourceRange Range, const SourceManager &SourceMgr);

  SingleRawComment(const SingleRawComment &) = default;
  SingleRawComment &operator=(const SingleRawComment &) = default;

  bool isOrdinary() const LLVM_READONLY {
    return Kind == CommentKind::OrdinaryBCPL ||
           Kind == CommentKind::OrdinaryC;
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
};

} // namespace swift

#endif // LLVM_SWIFT_AST_RAW_COMMENT_H

