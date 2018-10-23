//===- ShouldPrintForParseableInterface.h - Parseable Interface filtering -===//
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

#ifndef SWIFT_AST_SHOULDPRINTFORPARSEABLEINTERFACE_H
#define SWIFT_AST_SHOULDPRINTFORPARSEABLEINTERFACE_H

#include "swift/AST/PrintOptions.h"
#include "swift/Basic/LLVM.h"
#include "llvm/ADT/SmallPtrSet.h"

namespace swift {

class AbstractStorageDecl;
class Decl;
class TypeDecl;
class ValueDecl;

class ShouldPrintForParseableInterface : public ShouldPrintChecker {
  SmallPtrSet<TypeDecl *, 8> referencedNonPublicTypeDecls;
public:
  static std::shared_ptr<ShouldPrintForParseableInterface>
  create(ModuleDecl *module);

  bool shouldPrint(const Decl *D, const PrintOptions &options) override;
};

/// Determines if a declaration is public or has the @usableFromInline
/// attribute.
bool isPublicOrUsableFromInline(const ValueDecl *VD);

/// Determines if this storage decl resides as a stored instance member
/// of a non-resilient nominal type.
bool contributesToParentTypeStorage(const AbstractStorageDecl *ASD);

} // end namespace swift

#endif // !defined(SWIFT_AST_SHOULDPRINTFORPARSEABLEINTERFACE_H)
