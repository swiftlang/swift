//===--- ParseDeclName.h ----------------------------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_PARSE_PARSEDECLNAME_H
#define SWIFT_PARSE_PARSEDECLNAME_H

#include "swift/AST/Identifier.h"
#include "swift/Basic/LLVM.h"
#include "swift/Parse/Lexer.h"

namespace swift {

/// Describes a parsed declaration name.
struct ParsedDeclName {
  /// The name of the context of which the corresponding entity should
  /// become a member.
  StringRef ContextName;

  /// The base name of the declaration.
  StringRef BaseName;

  /// The argument labels for a function declaration.
  SmallVector<StringRef, 4> ArgumentLabels;

  /// Whether this is a function name (vs. a value name).
  bool IsFunctionName = false;

  /// Whether this is a getter for the named property.
  bool IsGetter = false;

  /// Whether this is a setter for the named property.
  bool IsSetter = false;

  bool IsSubscript = false;

  /// For a declaration name that makes the declaration into an
  /// instance member, the index of the "Self" parameter.
  std::optional<unsigned> SelfIndex;

  /// Determine whether this is a valid name.
  explicit operator bool() const { return !BaseName.empty(); }

  /// Whether this declaration name turns the declaration into a
  /// member of some named context.
  bool isMember() const { return !ContextName.empty(); }

  /// Whether the result is translated into an instance member.
  bool isInstanceMember() const {
    return isMember() && static_cast<bool>(SelfIndex);
  }

  /// Whether the result is translated into a static/class member.
  bool isClassMember() const {
    return isMember() && !static_cast<bool>(SelfIndex);
  }

  /// Whether this is a property accessor.
  bool isPropertyAccessor() const { return IsGetter || IsSetter; }

  /// Whether this is an operator.
  bool isOperator() const { return Lexer::isOperator(BaseName); }

  /// Form a declaration name from this parsed declaration name.
  DeclName formDeclName(ASTContext &ctx, bool isSubscript = false,
                        bool isCxxClassTemplateSpec = false) const;

  /// Form a declaration name from this parsed declaration name.
  DeclNameRef formDeclNameRef(ASTContext &ctx, bool isSubscript = false,
                              bool isCxxClassTemplateSpec = false) const;
};

/// Parse a stringified Swift declaration name,
/// e.g. "Foo.translateBy(self:x:y:)".
ParsedDeclName parseDeclName(StringRef name) LLVM_READONLY;

/// Form a Swift declaration name from its constituent parts.
DeclName formDeclName(ASTContext &ctx, StringRef baseName,
                      ArrayRef<StringRef> argumentLabels, bool isFunctionName,
                      bool isInitializer, bool isSubscript = false,
                      bool isCxxClassTemplateSpec = false);

/// Form a Swift declaration name reference from its constituent parts.
DeclNameRef formDeclNameRef(ASTContext &ctx, StringRef baseName,
                            ArrayRef<StringRef> argumentLabels,
                            bool isFunctionName, bool isInitializer,
                            bool isSubscript = false,
                            bool isCxxClassTemplateSpec = false);

} // namespace swift

#endif // SWIFT_PARSE_PARSEDECLNAME_H
