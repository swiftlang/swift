//===--- CommentAST.h - Swift-specific AST extensions for ReST ------------===//
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

#ifndef SWIFT_AST_COMMENT_AST_H
#define SWIFT_AST_COMMENT_AST_H

#include "swift/ReST/AST.h"

namespace swift {

enum class ReSTASTExtensionKind : uint8_t {
  ParamField = 0,
};

namespace comments {
class ReSTASTExtensionBase : public llvm::rest::PrivateExtension {
  ReSTASTExtensionKind ExtensionKind;

public:
  ReSTASTExtensionBase(ReSTASTExtensionKind ExtensionKind)
      : ExtensionKind(ExtensionKind) {}

  ReSTASTExtensionKind getExtensionKind() const { return ExtensionKind; }
};

class ParamField : public ReSTASTExtensionBase {
  llvm::rest::TextAndInline *FieldName;
  llvm::rest::LinePart ParamName;
  ArrayRef<llvm::rest::ReSTASTNode *> BodyChildren;

public:
  ParamField(llvm::rest::TextAndInline *FieldName,
             llvm::rest::LinePart ParamName,
             ArrayRef<llvm::rest::ReSTASTNode *> BodyChildren)
      : ReSTASTExtensionBase(ReSTASTExtensionKind::ParamField),
        FieldName(FieldName), ParamName(ParamName), BodyChildren(BodyChildren) {
  }

  const llvm::rest::TextAndInline *getFieldName() const { return FieldName; }
  llvm::rest::TextAndInline *getFieldName() { return FieldName; }

  llvm::rest::LinePart getParamName() const { return ParamName; }

  ArrayRef<const ReSTASTNode *> getBodyChildren() const {
    return ArrayRef<const ReSTASTNode *>(BodyChildren.data(),
                                         BodyChildren.size());
  }
  ArrayRef<ReSTASTNode *> getBodyChildren() { return BodyChildren; }

  static bool classof(const ReSTASTNode *N) {
    auto *Ext = dyn_cast<PrivateExtension>(N);
    return Ext && classof(static_cast<const ReSTASTExtensionBase *>(Ext));
  }

  static bool classof(const ReSTASTExtensionBase *N) {
    return N->getExtensionKind() == ReSTASTExtensionKind::ParamField;
  }
};

} // namespace comments

} // namespace swift

#endif // LLVM_SWIFT_AST_COMMENT_AST_H

