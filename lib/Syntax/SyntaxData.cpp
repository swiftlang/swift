//===--- SyntaxData.cpp - Swift Syntax Data Implementation ----------------===//
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

#include "swift/Syntax/DeclSyntax.h"
#include "swift/Syntax/ExprSyntax.h"
#include "swift/Syntax/GenericSyntax.h"
#include "swift/Syntax/TypeSyntax.h"
#include "swift/Syntax/StmtSyntax.h"
#include "swift/Syntax/UnknownSyntax.h"

using namespace swift;
using namespace swift::syntax;

RC<SyntaxData> SyntaxData::make(RC<RawSyntax> Raw,
                                const SyntaxData *Parent,
                                CursorIndex IndexInParent) {
  return RC<SyntaxData> {
    new SyntaxData(Raw, Parent, IndexInParent)
  };
}

RC<SyntaxData> SyntaxData::makeDataFromRaw(RC<RawSyntax> Raw,
                                           const SyntaxData *Parent,
                                           CursorIndex IndexInParent) {
  switch (Raw->Kind) {
#define SYNTAX(Id, ParentType) \
  case SyntaxKind::Id: \
    return Id##SyntaxData::make(Raw, Parent, IndexInParent);

#define MISSING_SYNTAX(Id, ParentType) \
  case SyntaxKind::Id: \
    return ParentType##Data::make(Raw, Parent, IndexInParent);

#include "swift/Syntax/SyntaxKinds.def"
  case SyntaxKind::Token:
    llvm_unreachable("Can't make a SyntaxData from a Token!");
  }
}

bool SyntaxData::isType() const {
  return Raw->isType();
}

bool SyntaxData::isStmt() const {
  return Raw->isStmt();
}

bool SyntaxData::isDecl() const {
  return Raw->isDecl();
}

bool SyntaxData::isExpr() const {
  return Raw->isExpr();
}

bool SyntaxData::isUnknown() const {
  return Raw->isUnknown();
}

void SyntaxData::dump(llvm::raw_ostream &OS) const {
  Raw->dump(OS, 0);
}
