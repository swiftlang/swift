//===- DelayedParsingCallbacks.h - Callbacks for Parser's delayed parsing -===//
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

#ifndef SWIFT_PARSE_DELAYED_PARSING_CALLBACKS_H
#define SWIFT_PARSE_DELAYED_PARSING_CALLBACKS_H

#include "swift/Basic/SourceLoc.h"

namespace swift {
  class FuncExpr;
  class Parser;

/// \brief Callbacks for Parser's delayed parsing.
class DelayedParsingCallbacks {
  virtual void anchor();

public:
  virtual ~DelayedParsingCallbacks() {}

  virtual bool shouldDelayFunctionBodyParsing(Parser &TheParser,
                                              FuncExpr *FE,
                                              SourceRange BodyRange) = 0;
};

class AlwaysDelayedCallbacks : public DelayedParsingCallbacks {
  bool shouldDelayFunctionBodyParsing(Parser &TheParser,
                                      FuncExpr *FE,
                                      SourceRange BodyRange) override {
    return true;
  }
};

/// \brief Implementation of callbacks that guide the parser in delayed
/// parsing for code completion.
class CodeCompleteDelayedCallbacks : public DelayedParsingCallbacks {
  SourceLoc CodeCompleteLoc;
public:
  explicit CodeCompleteDelayedCallbacks(SourceLoc CodeCompleteLoc)
    : CodeCompleteLoc(CodeCompleteLoc) {
  }

  bool shouldDelayFunctionBodyParsing(Parser &TheParser,
                                      FuncExpr *FE,
                                      SourceRange BodyRange) override {
    // Delay parsing if the code completion point is in the function body.
    return CodeCompleteLoc.Value.getPointer() >
               BodyRange.Start.Value.getPointer() &&
        CodeCompleteLoc.Value.getPointer() <= BodyRange.End.Value.getPointer();
  }
};

} // namespace swift

#endif
