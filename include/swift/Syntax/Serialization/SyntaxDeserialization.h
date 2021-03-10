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

namespace swift {
namespace json {

using namespace swift::syntax;

/// Input to the llvm::yaml / json parser that deserialises to a \c RawSyntax
/// tree. Contains a \c SyntaxArena in which the tree shall be created.
class SyntaxInput : public llvm::yaml::Input {
public:
  RC<SyntaxArena> Arena;

  SyntaxInput(llvm::StringRef InputContent, const RC<SyntaxArena> &Arena)
      : llvm::yaml::Input(InputContent), Arena(Arena) {}
  SyntaxInput(llvm::MemoryBufferRef buffer, const RC<SyntaxArena> &Arena)
      : llvm::yaml::Input(buffer), Arena(Arena) {}
};
} // end namespace json
} // end namespace swift

namespace llvm {
namespace yaml {

using swift::json::SyntaxInput;
using namespace swift::syntax;

/// Deserialization traits for SourcePresence.
template <> struct ScalarEnumerationTraits<SourcePresence> {
  static void enumeration(IO &in, SourcePresence &value) {
    in.enumCase(value, "Present", SourcePresence::Present);
    in.enumCase(value, "Missing", SourcePresence::Missing);
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
template <typename Context> TriviaPiece yamlize(IO &io, Context &Ctx) {
  io.beginMapping();
  auto ret = MappingTraits<TriviaPiece>::mapping(io);
  io.endMapping();
  return ret;
}

template <typename Context>
void yamlize(IO &io, std::vector<TriviaPiece> &Seq, bool, Context &Ctx) {
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

template <> struct SequenceTraits<std::vector<TriviaPiece>> {
  static size_t size(IO &in, std::vector<TriviaPiece> &seq) {
    return seq.size();
  }
};

/// Deserialization traits for RawSyntax list.
template <> struct SequenceTraits<std::vector<const RawSyntax *>> {
  static size_t size(IO &in, std::vector<const RawSyntax *> &seq) {
    return seq.size();
  }
  static const RawSyntax *&
  element(IO &in, std::vector<const RawSyntax *> &seq, size_t index) {
    if (seq.size() <= index) {
      seq.resize(index + 1);
    }
    return const_cast<const RawSyntax *&>(seq[index]);
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

/// Deserialization traits for RawSyntax *.
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

template <> struct MappingTraits<const RawSyntax *> {
  static void mapping(IO &in, const RawSyntax *&value) {
    TokenDescription description;
    // RawSyntax trees must always be generated from a SyntaxInput. Otherwise
    // we don't have an arena to create the nodes in.
    auto input = static_cast<SyntaxInput *>(&in);
    /// Check whether this is null
    if (input->getCurrentNode()->getType() != Node::NodeKind::NK_Mapping) {
      return;
    }
    in.mapOptional("tokenKind", description);
    if (description.hasValue) {
      swift::tok tokenKind = description.Kind;
      StringRef text = description.Text;
      StringRef leadingTrivia;
      in.mapRequired("leadingTrivia", leadingTrivia);
      StringRef trailingTrivia;
      in.mapRequired("trailingTrivia", trailingTrivia);
      SourcePresence presence;
      in.mapRequired("presence", presence);
      /// FIXME: This is a workaround for existing bug from llvm yaml parser
      /// which would raise error when deserializing number with trailing
      /// character like "1\n". See https://bugs.llvm.org/show_bug.cgi?id=15505
      StringRef nodeIdString;
      in.mapRequired("id", nodeIdString);
      unsigned nodeId = std::atoi(nodeIdString.data());
      value = RawSyntax::makeAndCalcLength(
          tokenKind, text, leadingTrivia, trailingTrivia, presence,
          input->Arena, nodeId);
    } else {
      SyntaxKind kind;
      in.mapRequired("kind", kind);
      std::vector<const RawSyntax *> layout;
      in.mapRequired("layout", layout);
      SourcePresence presence;
      in.mapRequired("presence", presence);
      /// FIXME: This is a workaround for existing bug from llvm yaml parser
      /// which would raise error when deserializing number with trailing
      /// character like "1\n". See https://bugs.llvm.org/show_bug.cgi?id=15505
      StringRef nodeIdString;
      in.mapRequired("id", nodeIdString);
      unsigned nodeId = std::atoi(nodeIdString.data());
      value = RawSyntax::makeAndCalcLength(kind, layout, presence,
                                                  input->Arena, nodeId);
    }
  }
};

} // end namespace yaml
} // end namespace llvm

namespace swift {
namespace json {
class SyntaxDeserializer {
  SyntaxInput Input;

public:
  SyntaxDeserializer(llvm::StringRef InputContent,
                     RC<SyntaxArena> Arena = SyntaxArena::make())
      : Input(InputContent, Arena) {}
  SyntaxDeserializer(llvm::MemoryBufferRef buffer,
                     RC<SyntaxArena> Arena = SyntaxArena::make())
      : Input(buffer, Arena) {}
  llvm::Optional<SourceFileSyntax> getSourceFileSyntax() {
    const RawSyntax *raw;
    Input >> raw;
    return makeRoot<SourceFileSyntax>(raw);
  }
};
} // namespace json
} // namespace swift

namespace swift {
namespace serialization {

using namespace swift::syntax;

/// Return the \c SyntaxKind corresponding to the numeric value produced by
/// \c serialization::getNumericValue(SyntaxKind).
SyntaxKind getSyntaxKindFromNumericValue(uint16_t Value);

/// Return the \c tok kind corresponding to the numeric value produced by
/// \c serialization::getNumericValue(tok).
tok getTokFromNumericValue(uint8_t Value);

} // end namespace serialization
} // end namespace swift

#endif // SWIFT_SYNTAX_SERIALIZATION_SYNTAXDESERIALIZATION_H
