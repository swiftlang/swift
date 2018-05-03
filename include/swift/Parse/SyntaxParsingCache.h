//===----------- SyntaxParsingCache.h -================----------*- C++ -*-===//
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

#ifndef SWIFT_PARSE_SYNTAXPARSINGCACHE_H
#define SWIFT_PARSE_SYNTAXPARSINGCACHE_H

#include "swift/Syntax/SyntaxNodes.h"

namespace swift {

using namespace swift::syntax;

class SyntaxParsingCache {
  /// The syntax tree prior to the edit
  SourceFileSyntax OldSyntaxTree;

public:
  SyntaxParsingCache(SourceFileSyntax OldSyntaxTree)
      : OldSyntaxTree(OldSyntaxTree) {}

  llvm::Optional<Syntax> lookUp(size_t NewPosition, SyntaxKind Kind) const;
};

} // namespace swift

#endif // SWIFT_SYNTAX_PARSING_CACHE_H
