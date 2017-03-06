//===--- Syntax.cpp - Swift Syntax Implementation -------------------------===//
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

#include "swift/Syntax/Syntax.h"
#include "swift/Syntax/SyntaxData.h"

using namespace swift;
using namespace swift::syntax;

Syntax::Syntax(const RC<SyntaxData> Root, const SyntaxData *Data)
  : Root(Root), Data(Data) {}

RC<RawSyntax> Syntax::getRaw() const {
  return Data->getRaw();
}

SyntaxKind Syntax::getKind() const {
  return getRaw()->Kind;
}

void Syntax::print(llvm::raw_ostream &OS) const {
  getRaw()->print(OS);
}

void Syntax::dump() const {
  getRaw()->dump();
}

void Syntax::dump(llvm::raw_ostream &OS, unsigned Indent) const {
  getRaw()->dump(OS, 0);
}

bool Syntax::isType() const {
  return Data->isType();
}

bool Syntax::isDecl() const {
  return Data->isDecl();
}

bool Syntax::isStmt() const {
  return Data->isStmt();
}

bool Syntax::isExpr() const {
  return Data->isExpr();
}

bool Syntax::isUnknown() const {
  return Data->isUnknown();
}

