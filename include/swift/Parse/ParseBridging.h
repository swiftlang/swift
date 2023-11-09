//===--- ParseBridging.h --------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_PARSE_PARSEBRIDGING_H
#define SWIFT_PARSE_PARSEBRIDGING_H

#include "swift/AST/ASTBridging.h"
#include "swift/Basic/BasicBridging.h"

#ifdef USED_IN_CPP_SOURC
#include "swift/Parse/Parser.h"
#else
namespace swift {
class Parser;
}
#endif

SWIFT_BEGIN_NULLABILITY_ANNOTATIONS

class BridgedLegacyParser {
  swift::Parser *_Nonnull const handle;

public:
#ifdef USED_IN_CPP_SOURCE
  BridgedLegacyParser(swift::Parser &P) : handle(&P) {}

  swift::Parser &unbridged() const { return *handle; }
#endif

  BridgedASTContext getASTContext() const;
  BridgedDiagnosticEngine getDiagnosticEngine() const;
  BridgedStringRef getSourceBuffer() const;
  BridgedDeclContext getCurrDeclContext() const;

  BridgedExpr parseExpr(BridgedSourceLoc loc, BridgedDeclContext DC,
                        bool isExprBasic) const;
  BridgedDecl parseDecl(BridgedSourceLoc loc, BridgedDeclContext DC) const;
  BridgedStmt parseStmt(BridgedSourceLoc loc, BridgedDeclContext DC) const;
  BridgedTypeRepr parseType(BridgedSourceLoc loc, BridgedDeclContext DC) const;
};

SWIFT_END_NULLABILITY_ANNOTATIONS

#endif // SWIFT_PARSE_PARSEBRIDGING_H
