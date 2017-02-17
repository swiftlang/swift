//===--- Comment.h - Swift-specific comment parsing -------------*- C++ -*-===//
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

#ifndef SWIFT_AST_COMMENT_H
#define SWIFT_AST_COMMENT_H

#include "swift/Markup/Markup.h"
#include "llvm/ADT/Optional.h"

namespace swift {
class Decl;
class DocComment;
struct RawComment;

class DocComment {
  const Decl *D;
  const swift::markup::Document *Doc = nullptr;
  const swift::markup::CommentParts Parts;

public:
  DocComment(const Decl *D, swift::markup::Document *Doc,
             swift::markup::CommentParts Parts)
      : D(D), Doc(Doc), Parts(Parts) {}

  const Decl *getDecl() const { return D; }

  const swift::markup::Document *getDocument() const { return Doc; }

  swift::markup::CommentParts getParts() const {
    return Parts;
  }

  ArrayRef<StringRef> getTags() const {
    return Parts.Tags;
  }

  Optional<const swift::markup::Paragraph *> getBrief() const {
    return Parts.Brief;
  }

  Optional<const swift::markup::ReturnsField * >getReturnsField() const {
    return Parts.ReturnsField;
  }

  Optional<const swift::markup::ThrowsField*> getThrowsField() const {
    return Parts.ThrowsField;
  }

  ArrayRef<const swift::markup::ParamField *> getParamFields() const {
    return Parts.ParamFields;
  }

  ArrayRef<const swift::markup::MarkupASTNode *> getBodyNodes() const {
    return Parts.BodyNodes;
  }

  Optional<const markup::LocalizationKeyField *>
  getLocalizationKeyField() const {
    return Parts.LocalizationKeyField;
  }

  bool isEmpty() const {
    return Parts.isEmpty();
  }

  // Only allow allocation using the allocator in MarkupContext or by
  // placement new.
  void *operator new(size_t Bytes, swift::markup::MarkupContext &MC,
                     unsigned Alignment = alignof(DocComment));
  void *operator new(size_t Bytes, void *Mem) {
    assert(Mem);
    return Mem;
  }

  // Make vanilla new/delete illegal.
  void *operator new(size_t Bytes) = delete;
  void operator delete(void *Data) = delete;
};

/// Get a parsed documentation comment for the declaration, if there is one.
Optional<DocComment *>getSingleDocComment(swift::markup::MarkupContext &Context,
                                          const Decl *D);

/// Attempt to get a doc comment from the declaration, or other inherited
/// sources, like from base classes or protocols.
Optional<DocComment *> getCascadingDocComment(swift::markup::MarkupContext &MC,
                                             const Decl *D);

} // namespace swift

#endif // LLVM_SWIFT_AST_COMMENT_H

