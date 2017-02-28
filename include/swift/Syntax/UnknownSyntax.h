//===--- UnknownSyntax.h - Swift Unknown Syntax Interface -----------------===//
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

#ifndef SWIFT_SYNTAX_UNKNOWNSYNTAX_H
#define SWIFT_SYNTAX_UNKNOWNSYNTAX_H

#include "swift/Syntax/SyntaxData.h"
#include "swift/Syntax/Syntax.h"

#include <vector>

namespace swift {
namespace syntax {

#pragma mark unknown-syntax Data

class UnknownSyntaxData : public SyntaxData {
  friend class SyntaxData;
  friend class UnknownSyntax;
  friend struct SyntaxFactory;
  friend class LegacyASTTransformer;

protected:
  std::vector<RC<SyntaxData>> CachedChildren;

  UnknownSyntaxData(const RC<RawSyntax> Raw,
                    const SyntaxData *Parent = nullptr,
                    const CursorIndex IndexInParent = 0);
public:

  static RC<UnknownSyntaxData> make(RC<RawSyntax> Raw,
                                    const SyntaxData *Parent = nullptr,
                                    CursorIndex IndexInParent = 0);

  size_t getNumChildren() const {
    return CachedChildren.size();
  }

  /// Get the child at the given Index.
  ///
  /// Precondition: Index <= getNumChildren();
  Syntax getChild(size_t Index) const;

  static bool classof(const SyntaxData *SD) {
    return SD->isUnknown();
  }
};

#pragma mark unknown-syntax API

/// A chunk of "unknown" syntax.
///
/// Effectively wraps a tree of RawSyntax.
///
/// This should not be vended by SyntaxFactory.
class UnknownSyntax : public Syntax {
  friend struct SyntaxFactory;
  friend class Syntax;

  using DataType = UnknownSyntaxData;

public:
  UnknownSyntax(const RC<SyntaxData> Root, const UnknownSyntaxData *Data);

  /// Get the number of child nodes in this piece of syntax, not including
  /// tokens.
  size_t getNumChildren() const;

  /// Get the Nth child of this piece of syntax.
  Syntax getChild(const size_t N) const;

  static bool classof(const Syntax *S) {
    return S->isUnknown();
  }
};

} // end namespace syntax
} // end namespace swift

#endif // SWIFT_SYNTAX_UNKNOWNSYNTAX_H
