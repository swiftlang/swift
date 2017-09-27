//===----------- SyntaxParsingContext.h -==============----------*- C++ -*-===//
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

#ifndef SWIFT_SYNTAX_PARSING_CONTEXT_H
#define SWIFT_SYNTAX_PARSING_CONTEXT_H

#include "swift/Syntax/RawTokenSyntax.h"
#include "swift/Syntax/TokenSyntax.h"
#include "swift/Syntax/References.h"
#include "swift/Syntax/RawSyntax.h"
#include "swift/Syntax/Syntax.h"
#include "swift/Syntax/TokenKinds.h"
#include "swift/Syntax/Trivia.h"

namespace swift {
  class SourceFile;

namespace syntax {

struct RawTokenInfo {
  SourceLoc Loc;
  RC<RawTokenSyntax> Token;
};

// The context that disables syntax tree building.
class SyntaxParsingContext {
public:
  virtual void addTokenSyntax(SourceLoc Loc) {}
  virtual void makeIntegerLiteralExp() {};
  virtual ~SyntaxParsingContext() = default;
};

class EnabledSyntaxParsingContext: public SyntaxParsingContext {
  SourceFile &File;
  std::vector<Syntax> PendingSyntax;
  Optional<TokenSyntax> checkBackToken(tok Kind);
public:
  EnabledSyntaxParsingContext(SourceFile &SF, unsigned BufferID);
  ~EnabledSyntaxParsingContext();
  void addTokenSyntax(SourceLoc Loc) override;
  void makeIntegerLiteralExp() override;
};
}
}
#endif // SWIFT_SYNTAX_PARSING_CONTEXT_H

