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

static unsigned getByteOffset(SourceLoc Loc, SourceManager &SM,
                              unsigned BufferID) {
  return Loc.isValid() ? SM.getLocOffsetInBuffer(Loc, BufferID) : 0;
}

static void initCRange(CRange &c_range, CharSourceRange range, SourceManager &SM,
                       unsigned BufferID) {
  if (range.isValid()) {
    c_range.offset = getByteOffset(range.getStart(), SM, BufferID);
    c_range.length = range.getByteLength();
  } else {
    c_range.offset = 0;
    c_range.length = 0;
  }
}

class SynParser {
  swiftparse_node_handler_t NodeHandler = nullptr;
  swiftparse_node_lookup_t NodeLookup = nullptr;
  swiftparse_diagnostic_handler_t DiagHandler = nullptr;

public:
  swiftparse_node_handler_t getNodeHandler() const {
    return NodeHandler;
  }

  swiftparse_node_lookup_t getNodeLookup() const {
    return NodeLookup;
  }

  swiftparse_diagnostic_handler_t getDiagnosticHandler() const {
    return DiagHandler;
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

  void setDiagnosticHandler(swiftparse_diagnostic_handler_t hdl) {
    auto prevBlk = DiagHandler;
    DiagHandler = Block_copy(hdl);
    Block_release(prevBlk);
  }

  ~SynParser() {
    setNodeHandler(nullptr);
    setNodeLookup(nullptr);
    setDiagnosticHandler(nullptr);
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
    return initCRange(c_range, range, SM, BufferID);
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

  void discardRecordedNode(OpaqueSyntaxNode node) override {
    // FIXME: This method should not be called at all.
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

static swiftparser_diagnostic_severity_t getSeverity(DiagnosticKind Kind) {
  switch (Kind) {
  case swift::DiagnosticKind::Error:
    return SWIFTPARSER_DIAGNOSTIC_SEVERITY_ERROR;
  case swift::DiagnosticKind::Warning:
    return SWIFTPARSER_DIAGNOSTIC_SEVERITY_WARNING;
  case swift::DiagnosticKind::Note:
    return SWIFTPARSER_DIAGNOSTIC_SEVERITY_NOTE;
  default:
    llvm_unreachable("unrecognized diagnostic kind.");
  }
}

struct DiagnosticDetail {
  const char* Message;
  unsigned Offset;
  std::vector<CRange> CRanges;
  swiftparser_diagnostic_severity_t Severity;
  std::vector<swiftparse_diagnostic_fixit_t> AllFixits;
};

struct SynParserDiagConsumer: public DiagnosticConsumer {
  SynParser &Parser;
  const unsigned BufferID;
  SynParserDiagConsumer(SynParser &Parser, unsigned BufferID):
    Parser(Parser), BufferID(BufferID) {}
  void handleDiagnostic(SourceManager &SM,
                        const DiagnosticInfo &Info) override {
    assert(Info.Kind != DiagnosticKind::Remark &&
           "Shouldn't see this in parser.");
    // The buffer where all char* will point into.
    llvm::SmallString<256> Buffer;
    auto getCurrentText = [&]() -> const char* {
      return Buffer.data() + Buffer.size();
    };
    DiagnosticDetail Result;
    Result.Severity = getSeverity(Info.Kind);
    Result.Offset = getByteOffset(Info.Loc, SM, BufferID);

    // Terminate each printed text with 0 so the client-side can use char* directly.
    char NullTerm = '\0';
    {
      // Print the error message to buffer and record it.
      llvm::raw_svector_ostream OS(Buffer);
      Result.Message = getCurrentText();
      DiagnosticEngine::formatDiagnosticText(OS, Info.FormatString,
                                             Info.FormatArgs);
      OS << NullTerm;
    }
    for (auto R: Info.Ranges) {
      Result.CRanges.emplace_back();
      initCRange(Result.CRanges.back(), R, SM, BufferID);
    }
    for (auto Fixit: Info.FixIts) {
      Result.AllFixits.push_back({CRange(), getCurrentText()});
      initCRange(Result.AllFixits.back().range, Fixit.getRange(), SM, BufferID);
      llvm::raw_svector_ostream OS(Buffer);
      OS << Fixit.getText() << NullTerm;
    }
    Parser.getDiagnosticHandler()(static_cast<void*>(&Result));
  }
};

swiftparse_client_node_t SynParser::parse(const char *source) {
  SourceManager SM;
  unsigned bufID = SM.addNewSourceBuffer(
    llvm::MemoryBuffer::getMemBuffer(source, "syntax_parse_source"));
  LangOptions langOpts;
  langOpts.BuildSyntaxTree = true;
  langOpts.CollectParsedToken = false;
  // Disable name lookups during parsing.
  // Not ready yet:
  // langOpts.EnableASTScopeLookup = true;

  auto parseActions =
    std::make_shared<CLibParseActions>(*this, SM, bufID);
  // We have to use SourceFileKind::Main to avoid diagnostics like
  // illegal_top_level_expr
  ParserUnit PU(SM, SourceFileKind::Main, bufID, langOpts,
                "syntax_parse_module", std::move(parseActions),
                /*SyntaxCache=*/nullptr);
  // Evaluating pound conditions may lead to unknown syntax.
  PU.getParser().State->PerformConditionEvaluation = false;
  std::unique_ptr<SynParserDiagConsumer> pConsumer;
  if (DiagHandler) {
    pConsumer = llvm::make_unique<SynParserDiagConsumer>(*this, bufID);
    PU.getDiagnosticEngine().addConsumer(*pConsumer);
  }
  return PU.parse();
}
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

//===--------------------- C API for diagnostics -------------------------====//

void
swiftparse_parser_set_diagnostic_handler(swiftparse_parser_t c_parser,
                                         swiftparse_diagnostic_handler_t hdl) {
  SynParser *parser = static_cast<SynParser*>(c_parser);
  parser->setDiagnosticHandler(hdl);
}

const char* swiftparse_diagnostic_get_message(swiftparser_diagnostic_t diag) {
  return static_cast<const DiagnosticDetail*>(diag)->Message;
}

unsigned swiftparse_diagnostic_get_fixit_count(swiftparser_diagnostic_t diag) {
  return static_cast<const DiagnosticDetail*>(diag)->AllFixits.size();
}

swiftparse_diagnostic_fixit_t
swiftparse_diagnostic_get_fixit(swiftparser_diagnostic_t diag, unsigned idx) {
  auto allFixits = static_cast<const DiagnosticDetail*>(diag)->AllFixits;
  assert(idx < allFixits.size());
  return allFixits[idx];
}

unsigned swiftparse_diagnostic_get_range_count(swiftparser_diagnostic_t diag) {
  return static_cast<const DiagnosticDetail*>(diag)->CRanges.size();
}

swiftparse_range_t
swiftparse_diagnostic_get_range(swiftparser_diagnostic_t diag, unsigned idx) {
  return static_cast<const DiagnosticDetail*>(diag)->CRanges[idx];
}

swiftparser_diagnostic_severity_t
swiftparse_diagnostic_get_severity(swiftparser_diagnostic_t diag) {
  return static_cast<const DiagnosticDetail*>(diag)->Severity;
}

unsigned swiftparse_diagnostic_get_source_loc(swiftparser_diagnostic_t diag) {
  return static_cast<const DiagnosticDetail*>(diag)->Offset;
}
