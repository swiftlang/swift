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

#include "swift/Basic/ByteTreeSerialization.h"
#include "swift/Basic/JSONSerialization.h"
#include "swift/Basic/StringExtras.h"
#include "swift/Syntax/RawSyntax.h"
#include "llvm/ADT/StringSwitch.h"
#include <forward_list>
#include <unordered_set>

namespace swift {
namespace json {

/// The associated value will be interpreted as \c bool. If \c true the node IDs
/// will not be included in the serialized JSON.
static void *DontSerializeNodeIdsUserInfoKey = &DontSerializeNodeIdsUserInfoKey;

/// The user info key pointing to a std::unordered_set of IDs of nodes that
/// shall be omitted when the tree gets serialized
static void *OmitNodesUserInfoKey = &OmitNodesUserInfoKey;

/// Serialization traits for SourcePresence.
template <>
struct ScalarReferenceTraits<syntax::SourcePresence> {
  static StringRef stringRef(const syntax::SourcePresence &value) {
    switch (value) {
    case syntax::SourcePresence::Present:
      return "\"Present\"";
    case syntax::SourcePresence::Missing:
      return "\"Missing\"";
    }
    llvm_unreachable("unhandled presence");
  }

  static bool mustQuote(StringRef) {
    // The string is already quoted. This is more efficient since it does not
    // check for characters that need to be escaped
    return false;
  }
};

/// Serialization traits for swift::tok.
template <>
struct ScalarReferenceTraits<tok> {
  static StringRef stringRef(const tok &value) {
    switch (value) {
#define TOKEN(name) \
    case tok::name: return "\"" #name "\"";
#include "swift/Syntax/TokenKinds.def"
    default: llvm_unreachable("Unknown token kind");
    }
  }

  static bool mustQuote(StringRef) {
    // The string is already quoted. This is more efficient since it does not
    // check for characters that need to be escaped
    return false;
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
    bool dontSerializeIds =
        (bool)out.getUserInfo()[DontSerializeNodeIdsUserInfoKey];
    if (!dontSerializeIds) {
      auto nodeId = value.getId();
      out.mapRequired("id", nodeId);
    }

    auto omitNodes =
        (std::unordered_set<unsigned> *)out.getUserInfo()[OmitNodesUserInfoKey];

    if (omitNodes && omitNodes->count(value.getId()) > 0) {
      bool omitted = true;
      out.mapRequired("omitted", omitted);
      return;
    }

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

namespace byteTree {

/// Increase the major version for every change that is not just adding a new
/// field at the end of an object. Older swiftSyntax clients will no longer be
/// able to deserialize the format.
const uint16_t SYNTAX_TREE_VERSION_MAJOR = 1; // Last change: initial version
/// Increase the minor version if only new field has been added at the end of
/// an object. Older swiftSyntax clients will still be able to deserialize the
/// format.
const uint8_t SYNTAX_TREE_VERSION_MINOR = 0; // Last change: initial version

// Combine the major and minor version into one. The first three bytes
// represent the major version, the last byte the minor version.
const uint32_t SYNTAX_TREE_VERSION =
    SYNTAX_TREE_VERSION_MAJOR << 8 | SYNTAX_TREE_VERSION_MINOR;

/// The key for a ByteTree serializion user info of type
/// `std::unordered_set<unsigned> *`. Specifies the IDs of syntax nodes that
/// shall be omitted when the syntax tree gets serialized.
static void *UserInfoKeyReusedNodeIds = &UserInfoKeyReusedNodeIds;

/// The key for a ByteTree serializion user info interpreted as `bool`.
/// If specified, additional fields will be added to objects in the ByteTree
/// to test forward compatibility.
static void *UserInfoKeyAddInvalidFields = &UserInfoKeyAddInvalidFields;

template <>
struct WrapperTypeTraits<tok> {
  static uint8_t numericValue(const tok &Value);

  static void write(ByteTreeWriter &Writer, const tok &Value, unsigned Index) {
    Writer.write(numericValue(Value), Index);
  }
};

template <>
  struct WrapperTypeTraits<syntax::SourcePresence> {
  static uint8_t numericValue(const syntax::SourcePresence &Presence) {
    switch (Presence) {
    case syntax::SourcePresence::Missing: return 0;
    case syntax::SourcePresence::Present: return 1;
    }
    llvm_unreachable("unhandled presence");
  }

  static void write(ByteTreeWriter &Writer,
                    const syntax::SourcePresence &Presence, unsigned Index) {
    Writer.write(numericValue(Presence), Index);
  }
};

template <>
struct ObjectTraits<ArrayRef<syntax::TriviaPiece>> {
  static unsigned numFields(const ArrayRef<syntax::TriviaPiece> &Trivia,
                            UserInfoMap &UserInfo) {
    return Trivia.size();
  }

  static void write(ByteTreeWriter &Writer,
                    const ArrayRef<syntax::TriviaPiece> &Trivia,
                    UserInfoMap &UserInfo) {
    for (unsigned I = 0, E = Trivia.size(); I < E; ++I) {
      Writer.write(Trivia[I], /*Index=*/I);
    }
  }
};

template <>
struct ObjectTraits<ArrayRef<RC<syntax::RawSyntax>>> {
  static unsigned numFields(const ArrayRef<RC<syntax::RawSyntax>> &Layout,
                            UserInfoMap &UserInfo) {
    return Layout.size();
  }

  static void write(ByteTreeWriter &Writer,
                    const ArrayRef<RC<syntax::RawSyntax>> &Layout,
                    UserInfoMap &UserInfo);
};

template <>
struct ObjectTraits<std::pair<tok, StringRef>> {
  static unsigned numFields(const std::pair<tok, StringRef> &Pair,
                            UserInfoMap &UserInfo) {
    return 2;
  }

  static void write(ByteTreeWriter &Writer,
                    const std::pair<tok, StringRef> &Pair,
                    UserInfoMap &UserInfo) {
    Writer.write(Pair.first, /*Index=*/0);
    Writer.write(Pair.second, /*Index=*/1);
  }
};

template <>
struct ObjectTraits<syntax::RawSyntax> {
  enum NodeKind { Token = 0, Layout = 1, Omitted = 2 };

  static bool shouldOmitNode(const syntax::RawSyntax &Syntax,
                             UserInfoMap &UserInfo) {
    if (auto ReusedNodeIds = static_cast<std::unordered_set<unsigned> *>(
            UserInfo[UserInfoKeyReusedNodeIds])) {
      return ReusedNodeIds->count(Syntax.getId()) > 0;
    } else {
      return false;
    }
  }

  static NodeKind nodeKind(const syntax::RawSyntax &Syntax,
                           UserInfoMap &UserInfo) {
    if (shouldOmitNode(Syntax, UserInfo)) {
      return Omitted;
    } else if (Syntax.isToken()) {
      return Token;
    } else {
      return Layout;
    }
  }

  static unsigned numFields(const syntax::RawSyntax &Syntax,
                            UserInfoMap &UserInfo) {
    // FIXME: We know this is never set in production builds. Should we
    // disable this code altogether in that case
    // (e.g. if assertions are not enabled?)
    if (UserInfo[UserInfoKeyAddInvalidFields]) {
      switch (nodeKind(Syntax, UserInfo)) {
      case Token: return 7;
      case Layout: return 6;
      case Omitted: return 2;
      }
      llvm_unreachable("unhandled kind");
    } else {
      switch (nodeKind(Syntax, UserInfo)) {
      case Token: return 6;
      case Layout: return 5;
      case Omitted: return 2;
      }
      llvm_unreachable("unhandled kind");
    }
  }

  static void write(ByteTreeWriter &Writer, const syntax::RawSyntax &Syntax,
                    UserInfoMap &UserInfo) {
    auto Kind = nodeKind(Syntax, UserInfo);

    Writer.write(static_cast<uint8_t>(Kind), /*Index=*/0);
    Writer.write(static_cast<uint32_t>(Syntax.getId()), /*Index=*/1);

    switch (Kind) {
    case Token:
      Writer.write(Syntax.getPresence(), /*Index=*/2);
      Writer.write(std::make_pair(Syntax.getTokenKind(), Syntax.getTokenText()),
                   /*Index=*/3);
      Writer.write(Syntax.getLeadingTrivia(), /*Index=*/4);
      Writer.write(Syntax.getTrailingTrivia(), /*Index=*/5);
      // FIXME: We know this is never set in production builds. Should we
      // disable this code altogether in that case
      // (e.g. if assertions are not enabled?)
      if (UserInfo[UserInfoKeyAddInvalidFields]) {
        // Test adding a new scalar field
        StringRef Str = "invalid forward compatible field";
        Writer.write(Str, /*Index=*/6);
      }
      break;
    case Layout:
      Writer.write(Syntax.getPresence(), /*Index=*/2);
      Writer.write(Syntax.getKind(), /*Index=*/3);
      Writer.write(Syntax.getLayout(), /*Index=*/4);
      // FIXME: We know this is never set in production builds. Should we
      // disable this code altogether in that case
      // (e.g. if assertions are not enabled?)
      if (UserInfo[UserInfoKeyAddInvalidFields]) {
        // Test adding a new object
        auto Piece = syntax::TriviaPiece::spaces(2);
        ArrayRef<syntax::TriviaPiece> SomeTrivia(Piece);
        Writer.write(SomeTrivia, /*Index=*/5);
      }
      break;
    case Omitted:
      // Nothing more to write
      break;
    }
  }
};

} // end namespace byteTree

} // end namespace swift

#endif /* SWIFT_SYNTAX_SERIALIZATION_SYNTAXSERIALIZATION_H */
