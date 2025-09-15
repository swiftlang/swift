//===--- ParseDeclName.cpp ------------------------------------------------===//
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

#include "swift/Parse/ParseDeclName.h"
#include "swift/AST/ASTContext.h"

using namespace swift;

ParsedDeclName swift::parseDeclName(StringRef name) {
  if (name.empty())
    return ParsedDeclName();

  // Local function to handle the parsing of the base name + context.
  //
  // Returns true if an error occurred, without recording the base name.
  ParsedDeclName result;
  auto parseBaseName = [&](StringRef text) -> bool {
    // Split the text into context name and base name.
    StringRef contextName, baseName;
    std::tie(contextName, baseName) = text.rsplit('.');
    if (baseName.empty()) {
      baseName = contextName;
      contextName = StringRef();
    } else if (contextName.empty()) {
      return true;
    }

    auto isValidIdentifier = [](StringRef text) -> bool {
      return Lexer::isIdentifier(text) && text != "_";
    };

    // Make sure we have an identifier for the base name.
    if (!isValidIdentifier(baseName))
      return true;

    // If we have a context, make sure it is an identifier, or a series of
    // dot-separated identifiers.
    // FIXME: What about generic parameters?
    if (!contextName.empty()) {
      StringRef first;
      StringRef rest = contextName;
      do {
        std::tie(first, rest) = rest.split('.');
        if (!isValidIdentifier(first))
          return true;
      } while (!rest.empty());
    }

    // Record the results.
    result.ContextName = contextName;
    result.BaseName = baseName;
    return false;
  };

  // If this is not a function name, just parse the base name and
  // we're done.
  if (name.back() != ')') {
    if (Lexer::isOperator(name))
      result.BaseName = name;
    else if (parseBaseName(name))
      return ParsedDeclName();
    return result;
  }

  // We have a function name.
  result.IsFunctionName = true;

  // Split the base name from the parameters.
  StringRef baseName, parameters;
  std::tie(baseName, parameters) = name.split('(');
  if (parameters.empty())
    return ParsedDeclName();

  // If the base name is prefixed by "getter:" or "setter:", it's an
  // accessor.
  if (baseName.starts_with("getter:")) {
    result.IsGetter = true;
    result.IsFunctionName = false;
    baseName = baseName.substr(7);
  } else if (baseName.starts_with("setter:")) {
    result.IsSetter = true;
    result.IsFunctionName = false;
    baseName = baseName.substr(7);
  }

  // If the base name is prefixed by "subscript", it's an subscript.
  if (baseName == "subscript") {
    result.IsSubscript = true;
  }

  // Parse the base name.
  if (parseBaseName(baseName))
    return ParsedDeclName();

  parameters = parameters.drop_back(); // ')'
  if (parameters.empty())
    return result;

  if (parameters.back() != ':')
    return ParsedDeclName();

  bool isMember = !result.ContextName.empty();
  do {
    StringRef NextParam;
    std::tie(NextParam, parameters) = parameters.split(':');

    if (!Lexer::isIdentifier(NextParam))
      return ParsedDeclName();
    if (NextParam == "_") {
      result.ArgumentLabels.push_back("");
    } else if (isMember && NextParam == "self") {
      // For a member, "self" indicates the self parameter. There can
      // only be one such parameter.
      if (result.SelfIndex)
        return ParsedDeclName();
      result.SelfIndex = result.ArgumentLabels.size();
    } else {
      result.ArgumentLabels.push_back(NextParam);
    }
  } while (!parameters.empty());

  return result;
}

DeclName ParsedDeclName::formDeclName(ASTContext &ctx, bool isSubscript,
                                      bool isCxxClassTemplateSpec) const {
  return formDeclNameRef(ctx, isSubscript, isCxxClassTemplateSpec)
      .getFullName();
}

DeclNameRef ParsedDeclName::formDeclNameRef(ASTContext &ctx, bool isSubscript,
                                            bool isCxxClassTemplateSpec) const {
  return swift::formDeclNameRef(ctx, BaseName, ArgumentLabels, IsFunctionName,
                                /*IsInitializer=*/true, isSubscript,
                                isCxxClassTemplateSpec);
}

DeclName swift::formDeclName(ASTContext &ctx, StringRef baseName,
                             ArrayRef<StringRef> argumentLabels,
                             bool isFunctionName, bool isInitializer,
                             bool isSubscript, bool isCxxClassTemplateSpec) {
  return formDeclNameRef(ctx, baseName, argumentLabels, isFunctionName,
                         isInitializer, isSubscript, isCxxClassTemplateSpec)
      .getFullName();
}

DeclNameRef swift::formDeclNameRef(ASTContext &ctx, StringRef baseName,
                                   ArrayRef<StringRef> argumentLabels,
                                   bool isFunctionName, bool isInitializer,
                                   bool isSubscript,
                                   bool isCxxClassTemplateSpec) {
  // We cannot import when the base name is not an identifier.
  if (baseName.empty())
    return DeclNameRef();

  if (!Lexer::isIdentifier(baseName) && !Lexer::isOperator(baseName) &&
      !isCxxClassTemplateSpec)
    return DeclNameRef();

  // Get the identifier for the base name. Special-case `init`.
  DeclBaseName baseNameId;
  if (isInitializer && baseName == "init")
    baseNameId = DeclBaseName::createConstructor();
  else if (isSubscript && baseName == "subscript")
    baseNameId = DeclBaseName::createSubscript();
  else
    baseNameId = ctx.getIdentifier(baseName);

  // For non-functions, just use the base name.
  if (!isFunctionName && !baseNameId.isSubscript())
    return DeclNameRef(baseNameId);

  // For functions, we need to form a complete name.

  // Convert the argument names.
  SmallVector<Identifier, 4> argumentLabelIds;
  for (auto argName : argumentLabels) {
    if (argumentLabels.empty() || !Lexer::isIdentifier(argName)) {
      argumentLabelIds.push_back(Identifier());
      continue;
    }

    argumentLabelIds.push_back(ctx.getIdentifier(argName));
  }

  // Build the result.
  return DeclNameRef({ctx, baseNameId, argumentLabelIds});
}
