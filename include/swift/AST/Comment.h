//===--- Comment.h - Swift-specific comment parsing -----------------------===//
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

#ifndef SWIFT_AST_COMMENT_H
#define SWIFT_AST_COMMENT_H

#include "swift/ReST/Parser.h"
#include "llvm/ADT/Optional.h"

namespace swift {
class Decl;
class FullComment;
struct RawComment;

class CommentContext final {
  SmallVector<FullComment *, 4> FullComments;

public:
  llvm::rest::ReSTContext TheReSTContext;

  CommentContext();

  ~CommentContext();
};

class FullComment {
public:
  class CommentParts {
  public:
    SmallVector<const llvm::rest::Field *, 8> Params;
    SmallVector<const llvm::rest::Field *, 4> Returns;
    const llvm::rest::Paragraph *Brief = nullptr;
    SmallVector<const llvm::rest::ReSTASTNode *, 4> MiscTopLevelNodes;
  };

private:
  const Decl *D;
  const llvm::rest::Document *Doc;
  mutable llvm::Optional<CommentParts> Parts;

public:
  FullComment(const Decl *D, const llvm::rest::Document *Doc)
      : D(D), Doc(Doc) {}
  const Decl *getDecl() const { return D; }
  const llvm::rest::Document *getDocument() const { return Doc; }

  const CommentParts &getParts() const;

  // Only allow allocation using the allocator in ReSTContext or by placement
  // new.
  void *operator new(size_t Bytes, llvm::rest::ReSTContext &C,
                     unsigned Alignment = alignof(FullComment));
  void *operator new(size_t Bytes, void *Mem) {
    assert(Mem);
    return Mem;
  }

  // Make vanilla new/delete illegal.
  void *operator new(size_t Bytes) = delete;
  void operator delete(void *Data) = delete;
};

FullComment *getFullComment(CommentContext &Context, const Decl *D);

} // namespace swift

#endif // LLVM_SWIFT_AST_COMMENT_H

