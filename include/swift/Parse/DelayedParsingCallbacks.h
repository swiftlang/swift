//===--- DelayedParsingCallbacks.h - Delayed parsing callbacks --*- C++ -*-===//
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

#ifndef SWIFT_PARSE_DELAYED_PARSING_CALLBACKS_H
#define SWIFT_PARSE_DELAYED_PARSING_CALLBACKS_H

#include "swift/Basic/SourceLoc.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Parse/Parser.h"

namespace swift {
  class DeclAttributes;
  class AbstractFunctionDecl;

/// \brief Callbacks for Parser's delayed parsing.
class DelayedParsingCallbacks {
  virtual void anchor();

public:
  virtual ~DelayedParsingCallbacks() = default;

  /// Checks if a function body should be delayed or skipped altogether.
  virtual bool shouldDelayFunctionBodyParsing(Parser &TheParser,
                                              AbstractFunctionDecl *AFD,
                                              const DeclAttributes &Attrs,
                                              SourceRange BodyRange) = 0;
};

class AlwaysDelayedCallbacks : public DelayedParsingCallbacks {
  bool shouldDelayFunctionBodyParsing(Parser &TheParser,
                                      AbstractFunctionDecl *AFD,
                                      const DeclAttributes &Attrs,
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
                                      AbstractFunctionDecl *AFD,
                                      const DeclAttributes &Attrs,
                                      SourceRange BodyRange) override {
    // Delay parsing if the code completion point is in the function body.
    return TheParser.SourceMgr
        .rangeContainsTokenLoc(BodyRange, CodeCompleteLoc);
  }
};

} // namespace swift

#endif
