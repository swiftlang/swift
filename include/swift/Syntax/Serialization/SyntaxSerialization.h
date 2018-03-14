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

/// Serialization traits for swift::tok.
template <>
struct ScalarEnumerationTraits<tok> {
  static void enumeration(Output &out, tok &value) {
#define TOKEN(name) \
    out.enumCase(value, #name, tok::name);
#include "swift/Syntax/TokenKinds.def"
  }
};

/// Serialization traits for Trivia.
/// Trivia will serialize as an array of the underlying TriviaPieces.
template<>
struct ArrayTraits<ArrayRef<syntax::TriviaPiece>> {
  static size_t size(Output &out, ArrayRef<syntax::TriviaPiece> &seq) {
    return seq.size();
  }
  static syntax::TriviaPiece &
  element(Output &out, ArrayRef<syntax::TriviaPiece> &seq, size_t index) {
    return const_cast<syntax::TriviaPiece &>(seq[index]);
  }
};

/// Serialization traits for RawSyntax list.
template<>
struct ArrayTraits<ArrayRef<RC<syntax::RawSyntax>>> {
  static size_t size(Output &out, ArrayRef<RC<syntax::RawSyntax>> &seq) {
    return seq.size();
  }
  static RC<syntax::RawSyntax> &
  element(Output &out, ArrayRef<RC<syntax::RawSyntax>> &seq, size_t index) {
    return const_cast<RC<syntax::RawSyntax> &>(seq[index]);
  }
};

/// An adapter struct that provides a nested structure for token content.
struct TokenDescription {
  tok Kind;
  StringRef Text;
};

/// Serialization traits for TokenDescription.
/// TokenDescriptions always serialized with a token kind, which is
/// the stringified version of their name in the tok:: enum.
/// ```
/// {
///   "kind": <token name, e.g. "kw_struct">,
/// }
/// ```
///
/// For tokens that have some kind of text attached, like literals or
/// identifiers, the serialized form will also have a "text" key containing
/// that text as the value.
template<>
struct ObjectTraits<TokenDescription> {
  static void mapping(Output &out, TokenDescription &value) {
    out.mapRequired("kind", value.Kind);
    if (!isTokenTextDetermined(value.Kind)) {
      out.mapRequired("text", value.Text);
    }
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
struct ObjectTraits<syntax::RawSyntax> {
  static void mapping(Output &out, syntax::RawSyntax &value) {
    if (value.isToken()) {
      auto tokenKind = value.getTokenKind();
      auto text = value.getTokenText();
      auto description = TokenDescription { tokenKind, text };
      out.mapRequired("tokenKind", description);

      auto leadingTrivia = value.getLeadingTrivia();
      out.mapRequired("leadingTrivia", leadingTrivia);

      auto trailingTrivia = value.getTrailingTrivia();
      out.mapRequired("trailingTrivia", trailingTrivia);
    } else {
      auto kind = value.getKind();
      out.mapRequired("kind", kind);

      auto layout = value.getLayout();
      out.mapRequired("layout", layout);
    }
    auto presence = value.getPresence();
    out.mapRequired("presence", presence);
  }
};

template<>
struct NullableTraits<RC<syntax::RawSyntax>> {
  using value_type = syntax::RawSyntax;
  static bool isNull(RC<syntax::RawSyntax> &value) {
    return value == nullptr;
  }
  static syntax::RawSyntax &get(RC<syntax::RawSyntax> &value) {
    return *value;
  }
};
} // end namespace json
} // end namespace swift

#endif /* SWIFT_SYNTAX_SERIALIZATION_SYNTAXSERIALIZATION_H */
