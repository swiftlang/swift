//===--- SyntaxData.cpp - Swift Syntax Data Implementation ------*- C++ -*-===//
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

#include "swift/Syntax/SyntaxData.h"
#include "swift/Syntax/TypeSyntax.h"
#include "swift/Syntax/StmtSyntax.h"

using namespace swift;
using namespace swift::syntax;

RC<SyntaxData> SyntaxData::make(RC<RawSyntax> Raw,
                                const SyntaxData *Parent,
                                CursorIndex IndexInParent) {
  return RC<SyntaxData> {
    new SyntaxData(Raw, Parent, IndexInParent)
  };
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

void SyntaxData::dump(llvm::raw_ostream &OS) const {
  Raw->dump(OS, 0);
}

#pragma mark - unknown-syntax Data

RC<UnknownSyntaxData> UnknownSyntaxData::make(RC<RawSyntax> Raw) {
  return RC<UnknownSyntaxData> {
    new UnknownSyntaxData(Raw)
  };
}

bool UnknownSyntaxData::classof(const SyntaxData *SD) {
  return SD->getKind() == SyntaxKind::Unknown;
}

