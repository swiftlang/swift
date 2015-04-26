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

#include "swift/Markup/Markup.h"
#include "llvm/ADT/Optional.h"

namespace swift {
class Decl;
class DocComment;
struct RawComment;

class DocComment {
public:
  struct CommentParts {
    Optional<const llvm::markup::Paragraph *>Brief;
    SmallVector<const llvm::markup::MarkupASTNode *, 4> BodyNodes;
    SmallVector<const llvm::markup::ParamField *, 8> ParamFields;
    Optional<const llvm::markup::ReturnsField *>ReturnsField;

    bool isEmpty() const {
      return !Brief.hasValue() && !ReturnsField.hasValue() && BodyNodes.empty() && ParamFields.empty();
    }
  };

private:
  const Decl *D;
  const llvm::markup::Document *Doc = nullptr;
  const CommentParts Parts;

public:
  DocComment(const Decl *D, llvm::markup::Document *Doc,
             CommentParts Parts)
      : D(D), Doc(Doc), Parts(Parts) {}

  const Decl *getDecl() const { return D; }

  const llvm::markup::Document *getDocument() const { return Doc; }

  CommentParts getParts() const {
    return Parts;
  }

  Optional<const llvm::markup::Paragraph *>getBrief() const {
    return Parts.Brief;
  }

  Optional<const llvm::markup::ReturnsField *>getReturnsField() const {
    return Parts.ReturnsField;
  }

  ArrayRef<const llvm::markup::ParamField *> getParamFields() const {
    return Parts.ParamFields;
  }

  ArrayRef<const llvm::markup::MarkupASTNode *> getBodyNodes() const {
    return Parts.BodyNodes;
  }

  bool isEmpty() const {
    return Parts.isEmpty();
  }

  // Only allow allocation using the allocator in MarkupContext or by
  // placement new.
  void *operator new(size_t Bytes, llvm::markup::MarkupContext &MC,
                     unsigned Alignment = alignof(DocComment));
  void *operator new(size_t Bytes, void *Mem) {
    assert(Mem);
    return Mem;
  }

  // Make vanilla new/delete illegal.
  void *operator new(size_t Bytes) = delete;
  void operator delete(void *Data) = delete;
};

Optional<DocComment *>getDocComment(llvm::markup::MarkupContext &Context,
                                    const Decl *D);

} // namespace swift

#endif // LLVM_SWIFT_AST_COMMENT_H

