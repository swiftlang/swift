//===--- libSwiftSyntaxParser.cpp - C API for Swift Syntax Parsing --------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This C API is primarily intended to serve as the Swift parsing component
// of SwiftSyntax (https://github.com/apple/swift-syntax).
//
//===----------------------------------------------------------------------===//

#include "swift-c/SyntaxParser/SwiftSyntaxParser.h"
#include "swift/AST/Module.h"
#include "swift/Basic/LangOptions.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Parse/Parser.h"
#include "swift/Parse/SyntaxParseActions.h"
#include "swift/Syntax/Serialization/SyntaxSerialization.h"
#include "swift/Syntax/SyntaxNodes.h"
#include "swift/Subsystems.h"
#include <Block.h>

using namespace swift;
using namespace swift::syntax;
using namespace swift::byteTree;

typedef swiftparse_range_t CRange;
typedef swiftparse_client_node_t CClientNode;
typedef swiftparse_syntax_node_t CRawSyntaxNode;
typedef swiftparse_trivia_piece_t CTriviaPiece;
typedef swiftparse_syntax_kind_t CSyntaxKind;

namespace {
class SynParser {
  swiftparse_node_handler_t NodeHandler = nullptr;
  swiftparse_node_lookup_t NodeLookup = nullptr;

public:
  swiftparse_node_handler_t getNodeHandler() const {
    return NodeHandler;
  }

  swiftparse_node_lookup_t getNodeLookup() const {
    return NodeLookup;
  }

  void setNodeHandler(swiftparse_node_handler_t hdl) {
    auto prevBlk = NodeHandler;
    NodeHandler = Block_copy(hdl);
    Block_release(prevBlk);
  }

  void setNodeLookup(swiftparse_node_lookup_t lookupBlk) {
    auto prevBlk = NodeLookup;
    NodeLookup = Block_copy(lookupBlk);
    Block_release(prevBlk);
  }

  ~SynParser() {
    setNodeHandler(nullptr);
    setNodeLookup(nullptr);
  }

  swiftparse_client_node_t parse(const char *source);
};

class CLibParseActions : public SyntaxParseActions {
  SynParser &SynParse;
  SourceManager &SM;
  unsigned BufferID;

public:
  CLibParseActions(SynParser &synParse, SourceManager &sm, unsigned bufID)
  : SynParse(synParse), SM(sm), BufferID(bufID) {}

private:
  swiftparse_node_handler_t getNodeHandler() const {
    return SynParse.getNodeHandler();
  }

  swiftparse_node_lookup_t getNodeLookup() const {
    return SynParse.getNodeLookup();
  }

  static void makeCTrivia(SmallVectorImpl<CTriviaPiece> &c_trivia,
                          ArrayRef<ParsedTriviaPiece> trivia) {
    for (const auto &piece : trivia) {
      CTriviaPiece c_piece;
      auto numValue =
        WrapperTypeTraits<TriviaKind>::numericValue(piece.getKind());
      c_piece.kind = numValue;
      assert(c_piece.kind == numValue && "trivia kind value is too large");
      c_piece.length = piece.getLength();
      c_trivia.push_back(c_piece);
    }
  }

  void makeCRange(CRange &c_range, CharSourceRange range) {
    if (range.isValid()) {
      c_range.offset = SM.getLocOffsetInBuffer(range.getStart(), BufferID);
      c_range.length = range.getByteLength();
    } else {
      c_range.offset = 0;
      c_range.length = 0;
    }
  }

  void makeCRawToken(CRawSyntaxNode &node,
                     tok kind,
                     ArrayRef<CTriviaPiece> leadingTrivia,
                     ArrayRef<CTriviaPiece> trailingTrivia,
                     CharSourceRange range) {
    node.kind = WrapperTypeTraits<SyntaxKind>::numericValue(SyntaxKind::Token);
    auto numValue = WrapperTypeTraits<swift::tok>::numericValue(kind);
    node.token_data.kind = numValue;
    assert(node.token_data.kind == numValue && "token kind value is too large");
    node.token_data.leading_trivia = leadingTrivia.data();
    node.token_data.leading_trivia_count = leadingTrivia.size();
    assert(node.token_data.leading_trivia_count == leadingTrivia.size() &&
           "leading trivia count value is too large");
    node.token_data.trailing_trivia = trailingTrivia.data();
    node.token_data.trailing_trivia_count = trailingTrivia.size();
    assert(node.token_data.trailing_trivia_count == trailingTrivia.size() &&
           "trailing trivia count value is too large");
    makeCRange(node.range, range);
    node.present = true;
  }

  OpaqueSyntaxNode recordToken(tok tokenKind,
                               ArrayRef<ParsedTriviaPiece> leadingTrivia,
                               ArrayRef<ParsedTriviaPiece> trailingTrivia,
                               CharSourceRange range) override {
    SmallVector<CTriviaPiece, 8> c_leadingTrivia, c_trailingTrivia;
    makeCTrivia(c_leadingTrivia, leadingTrivia);
    makeCTrivia(c_trailingTrivia, trailingTrivia);
    CRawSyntaxNode node;
    makeCRawToken(node, tokenKind, c_leadingTrivia, c_trailingTrivia,
                  range);
    return getNodeHandler()(&node);
  }

  OpaqueSyntaxNode recordMissingToken(tok tokenKind, SourceLoc loc) override {
    CRawSyntaxNode node;
    makeCRawToken(node, tokenKind, {}, {}, CharSourceRange{loc, 0});
    node.present = false;
    return getNodeHandler()(&node);
  }

  OpaqueSyntaxNode recordRawSyntax(SyntaxKind kind,
                                   ArrayRef<OpaqueSyntaxNode> elements,
                                   CharSourceRange range) override {
    CRawSyntaxNode node;
    auto numValue = WrapperTypeTraits<SyntaxKind>::numericValue(kind);
    node.kind = numValue;
    assert(node.kind == numValue && "syntax kind value is too large");
    node.layout_data.nodes = elements.data();
    node.layout_data.nodes_count = elements.size();
    makeCRange(node.range, range);
    node.present = true;
    return getNodeHandler()(&node);
  }

  std::pair<size_t, OpaqueSyntaxNode>
  lookupNode(size_t lexerOffset, SyntaxKind kind) override {
    auto NodeLookup = getNodeLookup();
    if (!NodeLookup) {
      return {0, nullptr};
    }
    auto numValue = WrapperTypeTraits<SyntaxKind>::numericValue(kind);
    CSyntaxKind ckind = numValue;
    assert(ckind == numValue && "syntax kind value is too large");
    auto result = NodeLookup(lexerOffset, ckind);
    return {result.length, result.node};
  }
};
}

swiftparse_client_node_t SynParser::parse(const char *source) {
  SourceManager SM;
  unsigned bufID = SM.addNewSourceBuffer(
    llvm::MemoryBuffer::getMemBuffer(source, "syntax_parse_source"));
  LangOptions langOpts;
  langOpts.BuildSyntaxTree = true;
  langOpts.CollectParsedToken = false;
  // Disable name lookups during parsing.
  langOpts.EnableASTScopeLookup = true;

  auto parseActions =
    std::make_shared<CLibParseActions>(*this, SM, bufID);
  ParserUnit PU(SM, SourceFileKind::Library, bufID, langOpts,
                "syntax_parse_module", std::move(parseActions),
                /*SyntaxCache=*/nullptr);
  return PU.parse();
}

//===--- C API ------------------------------------------------------------===//

swiftparse_parser_t
swiftparse_parser_create(void) {
  return new SynParser();
}

void
swiftparse_parser_dispose(swiftparse_parser_t c_parser) {
  SynParser *parser = static_cast<SynParser*>(c_parser);
  delete parser;
}

void
swiftparse_parser_set_node_handler(swiftparse_parser_t c_parser,
                                   swiftparse_node_handler_t hdl) {
  SynParser *parser = static_cast<SynParser*>(c_parser);
  parser->setNodeHandler(hdl);
}

void
swiftparse_parser_set_node_lookup(swiftparse_parser_t c_parser,
                                  swiftparse_node_lookup_t lookup) {
  SynParser *parser = static_cast<SynParser*>(c_parser);
  parser->setNodeLookup(lookup);
}

swiftparse_client_node_t
swiftparse_parse_string(swiftparse_parser_t c_parser, const char *source) {
  SynParser *parser = static_cast<SynParser*>(c_parser);
  return parser->parse(source);
}

const char* swiftparse_syntax_structure_versioning_identifier(void) {
  return getSyntaxStructureVersioningIdentifier();
}
