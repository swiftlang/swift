//===--- SyntaxDeserialization.h - Swift Syntax Deserialization --*- C++-*-===//
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
//
// This file provides the deserialization from JSON to RawSyntax nodes and their
// constituent parts.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SYNTAX_SERIALIZATION_SYNTAXDESERIALIZATION_H
#define SWIFT_SYNTAX_SERIALIZATION_SYNTAXDESERIALIZATION_H

#include "swift/Basic/StringExtras.h"
#include "swift/Syntax/RawSyntax.h"
#include "swift/Syntax/SyntaxNodes.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/YAMLTraits.h"
#include <forward_list>

namespace llvm {
namespace yaml {

/// Deserialization traits for SourcePresence.
template <> struct ScalarEnumerationTraits<swift::SourcePresence> {
  static void enumeration(IO &in, swift::SourcePresence &value) {
    in.enumCase(value, "Present", swift::SourcePresence::Present);
    in.enumCase(value, "Missing", swift::SourcePresence::Missing);
  }
};

/// Deserialization traits for swift::tok.
template <> struct ScalarEnumerationTraits<swift::tok> {
  static void enumeration(IO &in, swift::tok &value) {
#define TOKEN(name) in.enumCase(value, #name, swift::tok::name);
#include "swift/Syntax/TokenKinds.def"
  }
};

/// Deserialization traits for Trivia.
/// An array of the underlying TriviaPieces will be deserialized as Trivia.
template <typename Context> swift::TriviaPiece yamlize(IO &io, Context &Ctx) {
  io.beginMapping();
  auto ret = MappingTraits<swift::TriviaPiece>::mapping(io);
  io.endMapping();
  return ret;
}

template <typename Context>
void yamlize(IO &io, std::vector<swift::TriviaPiece> &Seq, bool, Context &Ctx) {
  unsigned incnt = io.beginSequence();
  for (unsigned i = 0; i < incnt; ++i) {
    void *SaveInfo;
    if (io.preflightElement(i, SaveInfo)) {
      Seq.push_back(yamlize(io, Ctx));
      io.postflightElement(SaveInfo);
    }
  }
  io.endSequence();
}

template <> struct SequenceTraits<std::vector<swift::TriviaPiece>> {
  static size_t size(IO &in, std::vector<swift::TriviaPiece> &seq) {
    return seq.size();
  }
};

/// Deserialization traits for RawSyntax list.
template <> struct SequenceTraits<std::vector<swift::RC<swift::RawSyntax>>> {
  static size_t size(IO &in, std::vector<swift::RC<swift::RawSyntax>> &seq) {
    return seq.size();
  }
  static swift::RC<swift::RawSyntax> &
  element(IO &in, std::vector<swift::RC<swift::RawSyntax>> &seq, size_t index) {
    if (seq.size() <= index) {
      seq.resize(index + 1);
    }
    return const_cast<swift::RC<swift::RawSyntax> &>(seq[index]);
  }
};

/// An adapter struct that provides a nested structure for token content.
struct TokenDescription {
  bool hasValue = false;
  swift::tok Kind;
  StringRef Text;
};

/// Desrialization traits for TokenDescription.
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
template <> struct MappingTraits<TokenDescription> {
  static void mapping(IO &in, TokenDescription &value) {
    in.mapRequired("kind", value.Kind);
    value.hasValue = true;
    if (!swift::isTokenTextDetermined(value.Kind)) {
      in.mapRequired("text", value.Text);
    } else {
      value.Text = swift::getTokenText(value.Kind);
    }
  }
};

/// Deserialization traits for RC<RawSyntax>.
/// First it will check whether the node is null.
/// Then this will be different depending if the raw syntax node is a Token or
/// not. Token nodes will always have this structure:
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

template <> struct MappingTraits<swift::RC<swift::RawSyntax>> {
  static void mapping(IO &in, swift::RC<swift::RawSyntax> &value) {
    TokenDescription description;
    auto input = static_cast<Input *>(&in);
    /// Check whether this is null
    if (input->getCurrentNode()->getType() != Node::NodeKind::NK_Mapping) {
      return;
    }
    in.mapOptional("tokenKind", description);
    if (description.hasValue) {
      swift::tok tokenKind = description.Kind;
      StringRef text = description.Text;
      std::vector<swift::TriviaPiece> leadingTrivia;
      in.mapRequired("leadingTrivia", leadingTrivia);
      std::vector<swift::TriviaPiece> trailingTrivia;
      in.mapRequired("trailingTrivia", trailingTrivia);
      swift::SourcePresence presence;
      in.mapRequired("presence", presence);
      value = swift::RawSyntax::make(tokenKind, text, leadingTrivia,
                                     trailingTrivia, presence, nullptr);
    } else {
      swift::SyntaxKind kind;
      in.mapRequired("kind", kind);
      std::vector<swift::RC<swift::RawSyntax>> layout;
      in.mapRequired("layout", layout);
      swift::SourcePresence presence;
      in.mapRequired("presence", presence);
      value = swift::RawSyntax::make(kind, layout, presence, nullptr);
    }
  }
};

} // end namespace yaml
} // end namespace llvm

namespace swift {
namespace json {
class SyntaxDeserializer {
  llvm::yaml::Input Input;

public:
  SyntaxDeserializer(llvm::StringRef InputContent) : Input(InputContent) {}
  SyntaxDeserializer(llvm::MemoryBufferRef buffer) : Input(buffer) {}
  llvm::Optional<swift::SourceFileSyntax> getSourceFileSyntax() {
    swift::RC<swift::RawSyntax> raw;
    Input >> raw;
    return swift::make<swift::SourceFileSyntax>(raw);
  }
};
} // namespace json
} // namespace swift

#endif /* SWIFT_SYNTAX_SERIALIZATION_SYNTAXDESERIALIZATION_H */
