//===--- SILParserFunctionBuilder.h ---------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_PARSESIL_SILTYPEPARSER_H
#define SWIFT_PARSESIL_SILTYPEPARSER_H

#include "swift/AST/GenericEnvironment.h"
#include "swift/Parse/Parser.h"
#include "swift/SIL/SILFunctionBuilder.h"
#include "swift/SIL/SILModule.h"

namespace swift {

class SILTypeParser {
  Parser &P;
  const std::function<bool(TypeLoc&, GenericEnvironment *)> checkType;
  const std::function<void(Type)> callback;

public:
  SILTypeParser(
      Parser &parser,
      const std::function<bool(TypeLoc&, GenericEnvironment *)> checkType,
      const std::function<void(Type)> callback)
      : P(parser), checkType(checkType), callback(callback){};

  bool parseSILType(SILType &Result, GenericEnvironment *&parsedGenericEnv,
                    bool IsFuncDecl = false,
                    GenericEnvironment *parentGenericEnv = nullptr);
  bool parseSILType(SILType &Result);
  bool parseSILType(SILType &Result, SourceLoc &TypeLoc);
  bool parseSILType(SILType &Result, SourceLoc &TypeLoc,
                    GenericEnvironment *&parsedGenericEnv,
                    GenericEnvironment *parentGenericEnv = nullptr);
};

} // namespace swift

#endif // SWIFT_PARSESIL_SILTYPEPARSER_H
