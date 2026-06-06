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
#include "swift/AST/ASTPrinter.h"
#include "swift/Basic/StringExtras.h"
#include "swift/Parse/Lexer.h"

using namespace swift;

static bool isValidIdentifierInDeclName(StringRef text, bool allowUnderscore) {
  if (Lexer::isIdentifier(text) && (allowUnderscore || text != "_"))
    return true;
  if (text.size() > 2 && text.front() == '`' && text.back() == '`')
    return Lexer::isValidAsEscapedIdentifier(stripBackticks(text));
  return false;
}

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
    std::tie(contextName, baseName) = backtickAwareRSplit(text, '.');
    if (baseName.empty()) {
      baseName = contextName;
      contextName = StringRef();
    } else if (contextName.empty()) {
      return true;
    }

    auto isValidIdentifier = [](StringRef text) -> bool {
      return isValidIdentifierInDeclName(text, /*allowUnderscore=*/false);
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
        std::tie(first, rest) = backtickAwareSplit(rest, '.');
        if (!isValidIdentifier(first))
          return true;
        result.ContextNames.push_back(stripBackticks(first));
      } while (!rest.empty());
    }

    // Record the results.
    if (baseName == "init") {
      result.BaseNameKind = DeclBaseName::Kind::Constructor;
    } else if (baseName == "deinit") {
      result.BaseNameKind = DeclBaseName::Kind::Destructor;
    } else if (result.IsSubscript || baseName == "subscript") {
      result.BaseNameKind = DeclBaseName::Kind::Subscript;
    } else {
      result.BaseNameKind = DeclBaseName::Kind::Normal;
    }
    result.BaseName = stripBackticks(baseName);
    return false;
  };

  // If this is not a function name, just parse the base name and
  // we're done.
  if (name.back() != ')') {
    if (Lexer::isOperator(name)) {
      result.BaseName = name;
      result.BaseNameKind = DeclBaseName::Kind::Normal;
    } else if (parseBaseName(name))
      return ParsedDeclName();
    return result;
  }

  // We have a function name.
  result.IsFunctionName = true;

  // Split the base name from the parameters.
  StringRef baseName, parameters;
  std::tie(baseName, parameters) = backtickAwareSplit(name, '(');
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

  bool isMember = !result.ContextNames.empty();
  do {
    StringRef NextParam;
    std::tie(NextParam, parameters) = backtickAwareSplit(parameters, ':');

    if (!isValidIdentifierInDeclName(NextParam, /*allowUnderscore=*/true))
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
      result.ArgumentLabels.push_back(stripBackticks(NextParam));
    }
  } while (!parameters.empty());

  return result;
}

std::string ParsedDeclName::fullContextName() const {
  std::string result;
  llvm::raw_string_ostream os(result);
  bool first = true;
  for (auto component : ContextNames) {
    if (!first)
      os << '.';
    printIdentifierEscapingIfNeeded(component, os,
                                    first ? PrintNameContext::Normal
                                          : PrintNameContext::TypeMember);
    first = false;
  }
  return result;
}

std::string ParsedDeclName::baseNameSpelling(PrintNameContext context) const {
  switch (BaseNameKind) {
  case DeclBaseName::Kind::Subscript:
    return "subscript";
  case DeclBaseName::Kind::Constructor:
    return "init";
  case DeclBaseName::Kind::Destructor:
    return "deinit";
  case DeclBaseName::Kind::Normal:
    return identifierEscapingIfNeeded(BaseName, context);
  }
  llvm_unreachable("Unhandled DeclBaseName::Kind in switch.");
}

DeclName ParsedDeclName::formDeclName(ASTContext &ctx,
                                      bool isCxxClassTemplateSpec) const {
  return formDeclNameRef(ctx, isCxxClassTemplateSpec).getFullName();
}

DeclNameRef ParsedDeclName::formDeclNameRef(ASTContext &ctx,
                                            bool isCxxClassTemplateSpec) const {
  return swift::formDeclNameRef(ctx, BaseName, ArgumentLabels, IsFunctionName,
                                BaseNameKind, isCxxClassTemplateSpec);
}

DeclName swift::formDeclName(ASTContext &ctx, StringRef baseName,
                             ArrayRef<StringRef> argumentLabels,
                             bool isFunctionName,
                             DeclBaseName::Kind baseNameKind,
                             bool isCxxClassTemplateSpec) {
  return formDeclNameRef(ctx, baseName, argumentLabels, isFunctionName,
                         baseNameKind, isCxxClassTemplateSpec)
      .getFullName();
}

DeclNameRef swift::formDeclNameRef(ASTContext &ctx, StringRef baseName,
                                   ArrayRef<StringRef> argumentLabels,
                                   bool isFunctionName,
                                   DeclBaseName::Kind baseNameKind,
                                   bool isCxxClassTemplateSpec) {
  // We cannot import when the base name is not an identifier.
  if (baseName.empty())
    return DeclNameRef();

  // This function receives identifiers that, if they were raw identifiers,
  // have had their backticks stripped. Thus, we use
  // `Lexer::isValidAsEscapedIdentifier` here and below instead of
  // `isValidIdentifierInDeclName`, which expects that the only valid raw
  // identifiers it sees will have their backticks.
  if (!Lexer::isValidAsEscapedIdentifier(baseName) &&
      !Lexer::isOperator(baseName) && !isCxxClassTemplateSpec)
    return DeclNameRef();

  // Get the identifier for the base name. Special-case init/subscript/deinit.
  DeclBaseName baseNameId;
  switch (baseNameKind) {
  case DeclBaseName::Kind::Constructor:
    baseNameId = DeclBaseName::createConstructor();
    break;
  case DeclBaseName::Kind::Subscript:
    baseNameId = DeclBaseName::createSubscript();
    break;
  case DeclBaseName::Kind::Destructor:
    baseNameId = DeclBaseName::createDestructor();
    break;
  case DeclBaseName::Kind::Normal:
    baseNameId = ctx.getIdentifier(stripBackticks(baseName));
    break;
  }

  // For non-functions, just use the base name.
  if (!isFunctionName && !baseNameId.isSubscript())
    return DeclNameRef(baseNameId);

  // For functions, we need to form a complete name.

  // Convert the argument names.
  SmallVector<Identifier, 4> argumentLabelIds;
  for (auto argName : argumentLabels) {
    if (argumentLabels.empty() || !Lexer::isValidAsEscapedIdentifier(argName)) {
      argumentLabelIds.push_back(Identifier());
      continue;
    }

    argumentLabelIds.push_back(ctx.getIdentifier(stripBackticks(argName)));
  }

  // Build the result.
  return DeclNameRef({ctx, baseNameId, argumentLabelIds});
}
