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

namespace swift {
  class FuncExpr;

/// \brief Callbacks for Parser's delayed parsing.
class DelayedParsingCallbacks {
  virtual void anchor();

public:
  virtual ~DelayedParsingCallbacks() {}

  virtual bool shouldDelayFunctionBodyParsing(Parser &TheParser,
                                              FuncExpr *FE,
                                              SourceRange BodyRange) = 0;
};

} // namespace swift

#endif
