//===--- SyntaxSerialization.h - Swift Syntax Serialization -----*- C++ -*-===//
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
//
// This file provides the serialization of RawSyntax nodes and their
// constituent parts to JSON.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SYNTAX_SERIALIZATION_SYNTAXSERIALIZATION_H
#define SWIFT_SYNTAX_SERIALIZATION_SYNTAXSERIALIZATION_H

#include "swift/Syntax/RawSyntax.h"
#include "swift/Basic/JSONSerialization.h"
#include "swift/Basic/StringExtras.h"
#include "llvm/ADT/StringSwitch.h"
#include <forward_list>

namespace swift {
namespace json {

/// Serialization traits for SourcePresence.
template <>
struct ScalarEnumerationTraits<syntax::SourcePresence> {
  static void enumeration(json::Output &out, syntax::SourcePresence &value) {
    out.enumCase(value, "Present", syntax::SourcePresence::Present);
    out.enumCase(value, "Missing", syntax::SourcePresence::Missing);
  }
};

/// Serialization traits for SyntaxKind.
template <>
struct ScalarEnumerationTraits<syntax::SyntaxKind> {
  static void enumeration(Output &out, syntax::SyntaxKind &value) {
#define DEFINE_MAP(Id) \
    out.enumCase(value, #Id, syntax::SyntaxKind::Id);
#define SYNTAX(Id, Parent) DEFINE_MAP(Id)
#define SYNTAX_COLLECTION(Id, Element) DEFINE_MAP(Id)
#include "swift/Syntax/SyntaxKinds.def"
  }
};

/// Serialization traits for swift::tok.
template <>
struct ScalarEnumerationTraits<tok> {
  static void enumeration(Output &out, tok &value) {
#define EXPAND(Str, Case) \
    out.enumCase(value, Str, Case);
#define LITERAL(X) EXPAND(#X, tok::X)
#define MISC(X) EXPAND(#X, tok::X)
#define KEYWORD(X) EXPAND("kw_" #X, tok::kw_##X)
#define PUNCTUATOR(X, Y) EXPAND(#X, tok::X)
#define POUND_KEYWORD(X) EXPAND("pound_" #X, tok::pound_##X)
#include "swift/Syntax/TokenKinds.def"
  }
};

/// Serialization traits for TriviaPiece.
/// - All trivia pieces will have a "kind" key that contains the serialized
///   name of the trivia kind.
/// - Comment trivia will have the associated text of the comment under the
///   "value" key.
/// - All other trivia will have the associated integer count of their
///   occurrences under the "value" key.
template<>
struct ObjectTraits<syntax::TriviaPiece> {
  static void mapping(Output &out, syntax::TriviaPiece &value) {
    out.mapRequired("kind", value.Kind);
    switch (value.Kind) {
      case syntax::TriviaKind::Space:
      case syntax::TriviaKind::Tab:
      case syntax::TriviaKind::VerticalTab:
      case syntax::TriviaKind::Formfeed:
      case syntax::TriviaKind::Newline:
      case syntax::TriviaKind::Backtick:
        out.mapRequired("value", value.Count);
        break;
      case syntax::TriviaKind::LineComment:
      case syntax::TriviaKind::BlockComment:
      case syntax::TriviaKind::DocLineComment:
      case syntax::TriviaKind::DocBlockComment: {
        auto text = value.Text.str();
        out.mapRequired("value", text);
        break;
      }
    }
  }
};

/// Serialization traits for TriviaKind.
template <>
struct ScalarEnumerationTraits<syntax::TriviaKind> {
  static void enumeration(Output &out, syntax::TriviaKind &value) {
    out.enumCase(value, "Space", syntax::TriviaKind::Space);
    out.enumCase(value, "Tab", syntax::TriviaKind::Tab);
    out.enumCase(value, "VerticalTab", syntax::TriviaKind::VerticalTab);
    out.enumCase(value, "Formfeed", syntax::TriviaKind::Formfeed);
    out.enumCase(value, "Newline", syntax::TriviaKind::Newline);
    out.enumCase(value, "LineComment", syntax::TriviaKind::LineComment);
    out.enumCase(value, "BlockComment", syntax::TriviaKind::BlockComment);
    out.enumCase(value, "DocLineComment", syntax::TriviaKind::DocLineComment);
    out.enumCase(value, "DocBlockComment", syntax::TriviaKind::DocBlockComment);
    out.enumCase(value, "Backtick", syntax::TriviaKind::Backtick);
  }
};

/// Serialization traits for Trivia.
/// Trivia will serialize as an array of the underlying TriviaPieces.
template<>
struct ArrayTraits<syntax::Trivia> {
  static size_t size(Output &out, syntax::Trivia &seq) {
    return seq.Pieces.size();
  }
  static syntax::TriviaPiece& element(Output &out, syntax::Trivia &seq,
                                      size_t index) {
    return seq.Pieces[index];
  }
};

/// An adapter struct that provides a nested structure for token content.
struct TokenDescription {
  tok Kind;
  StringRef Text;
};

/// Serialization traits for TokenDescription.
/// TokenDescriptions are always serialized this way:
/// ```
/// {
///   "kind": <token name, e.g. "kw_struct">,
///   "text": <token text, e.g. "struct">
/// }
/// ```
template<>
struct ObjectTraits<TokenDescription> {
  static void mapping(Output &out, TokenDescription &value) {
    out.mapRequired("kind", value.Kind);
    out.mapRequired("text", value.Text);
  }
};

/// Serialization traits for RC<RawSyntax>.
/// This will be different depending if the raw syntax node is a Token or not.
/// Token nodes will always have this structure:
/// ```
/// {
///   "tokenKind": { "kind": <token kind>, "text": <token text> },
///   "leadingTrivia": [ <trivia pieces...> ],
///   "trailingTrivia": [ <trivia pieces...> ],
///   "presence": <"Present" or "Missing">
/// }
/// ```
/// All other raw syntax nodes will have this structure:
/// ```
/// {
///   "kind": <syntax kind>,
///   "layout": [ <raw syntax nodes...> ],
///   "presence": <"Present" or "Missing">
/// }
/// ```
template<>
struct ObjectTraits<RC<syntax::RawSyntax>> {
  static void mapping(Output &out, RC<syntax::RawSyntax> &value) {
    auto kind = value->Kind;
    switch (kind) {
    case syntax::SyntaxKind::Token: {
      auto Tok = cast<syntax::TokenSyntax>(value);
      auto tokenKind = Tok->getTokenKind();
      auto text = Tok->getText();
      auto description = TokenDescription { tokenKind, text };
      out.mapRequired("tokenKind", description);

      auto leadingTrivia = Tok->LeadingTrivia;
      out.mapRequired("leadingTrivia", leadingTrivia);

      auto trailingTrivia = Tok->TrailingTrivia;
      out.mapRequired("trailingTrivia", trailingTrivia);
      break;
    }
    default: {
      out.mapRequired("kind", kind);

      auto layout = value->Layout;
      out.mapRequired("layout", layout);

      break;
    }
    }
    auto presence = value->Presence;
    out.mapRequired("presence", presence);
  }
};
} // end namespace json
} // end namespace swift

#endif /* SWIFT_SYNTAX_SERIALIZATION_SYNTAXSERIALIZATION_H */
