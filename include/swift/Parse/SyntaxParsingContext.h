//===----------- SyntaxParsingContext.h -==============----------*- C++ -*-===//
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

#ifndef SWIFT_PARSE_SYNTAXPARSINGCONTEXT_H
#define SWIFT_PARSE_SYNTAXPARSINGCONTEXT_H

#include "llvm/ADT/PointerUnion.h"
#include "swift/Basic/Debug.h"
#include "swift/Basic/SourceLoc.h"
#include "swift/Parse/ParsedRawSyntaxNode.h"
#include "swift/Parse/ParsedRawSyntaxRecorder.h"

namespace swift {

using namespace swift::syntax;

class ParsedSyntax;
class ParsedTokenSyntax;
struct ParsedTrivia;
class SourceFile;
enum class tok;
class Token;
class DiagnosticEngine;

namespace syntax {
  enum class SyntaxKind;
}

enum class SyntaxContextKind {
  Decl,
  Stmt,
  Expr,
  Type,
  Pattern,
  Syntax,
};

enum class SyntaxNodeCreationKind {
  /// This is for \c SyntaxParsingContext to collect the syntax data and create
  /// a 'recorded' ParsedRawSyntaxNode object, which would be a result of
  /// passing the index data to the \c SyntaxParseActions implementation.
  Recorded,
  /// This is for \c SyntaxParsingContext to collect the syntax data and create
  /// a 'deferred' ParsedRawSyntaxNode object, which captures the data for a
  /// \c SyntaxParseActions invocation to occur later.
  ///
  /// This is intended to be used for when it's not clear what will be the final
  /// syntax node in the current parsing context.
  Deferred,
};

constexpr size_t SyntaxAlignInBits = 3;

/// RAII object which receive RawSyntax parts. On destruction, this constructs
/// a specified syntax node from received parts and propagate it to the parent
/// context.
///
/// e.g.
///   parseExprParen() {
///     SyntaxParsingContext LocalCtxt(SyntaxContext, SyntaxKind::ParenExpr);
///     consumeToken(tok::l_paren) // In consumeToken(), a RawTokenSyntax is
///                                // added to the context.
///     parseExpr(); // On returning from parseExpr(), a Expr Syntax node is
///                  // created and added to the context.
///     consumeToken(tok::r_paren)
///     // Now the context holds { '(' Expr ')' }.
///     // From these parts, it creates ParenExpr node and add it to the parent.
///   }
class alignas(1 << SyntaxAlignInBits) SyntaxParsingContext {
public:
  /// The shared data for all syntax parsing contexts with the same root.
  /// This should be accessible from the root context only.
  struct alignas(1 << SyntaxAlignInBits) RootContextData {
    // The source file under parsing.
    SourceFile &SF;

    // Where to issue diagnostics.
    DiagnosticEngine &Diags;

    SourceManager &SourceMgr;

    unsigned BufferID;

    // Storage for Collected parts.
    std::vector<ParsedRawSyntaxNode> Storage;

    ParsedRawSyntaxRecorder Recorder;

    llvm::BumpPtrAllocator ScratchAlloc;

    RootContextData(SourceFile &SF, DiagnosticEngine &Diags,
                    SourceManager &SourceMgr, unsigned BufferID,
                    std::shared_ptr<SyntaxParseActions> spActions)
        : SF(SF), Diags(Diags), SourceMgr(SourceMgr), BufferID(BufferID),
          Recorder(std::move(spActions)) {}
  };

private:
  /// Indicates what action should be performed on the destruction of
  ///  SyntaxParsingContext
  enum class AccumulationMode {
    // Coerece the result to one of ContextKind.
    // E.g. for ContextKind::Expr, passthroug if the result is CallExpr, whereas
    // <UnknownExpr><SomeDecl /></UnknownDecl> for non Exprs.
    CoerceKind,

    // Construct a result Syntax with specified SyntaxKind.
    CreateSyntax,

    /// Construct a deferred raw node, to be recorded later.
    DeferSyntax,

    // Pass through all parts to the parent context.
    Transparent,

    // Discard all parts in the context.
    Discard,

    // The node has been found as incremental update and all parts shall be
    // discarded.
    SkippedForIncrementalUpdate,

    // Construct SourceFile syntax to the specified SF.
    Root,

    // Invalid.
    NotSet,
  };

  // When this context is a root, this points to an instance of RootContextData;
  // When this context isn't a root, this points to the parent context.
  const llvm::PointerUnion<RootContextData *, SyntaxParsingContext *>
      RootDataOrParent;

  // Reference to the
  SyntaxParsingContext *&CtxtHolder;

  RootContextData *RootData;

  // Offet for 'Storage' this context owns from.
  const size_t Offset;

  // Operation on destruction.
  AccumulationMode Mode = AccumulationMode::NotSet;

  // Additional info depending on \c Mode.
  union {
    // For AccumulationMode::CreateSyntax; desired syntax node kind.
    SyntaxKind SynKind;
    // For AccumulationMode::CoerceKind; desired syntax node category.
    SyntaxContextKind CtxtKind;
  };

  /// true if it's in backtracking context.
  bool IsBacktracking = false;

  /// true if ParsedSyntaxBuilders and ParsedSyntaxRecorder should create
  /// deferred nodes
  bool ShouldDefer = false;

  // If false, context does nothing.
  bool Enabled;

  /// Create a syntax node using the tail \c N elements of collected parts and
  /// replace those parts with the single result.
  void createNodeInPlace(SyntaxKind Kind, size_t N,
                         SyntaxNodeCreationKind nodeCreateK);

  ArrayRef<ParsedRawSyntaxNode> getParts() const {
    return llvm::makeArrayRef(getStorage()).drop_front(Offset);
  }
  MutableArrayRef<ParsedRawSyntaxNode> getParts() {
    return llvm::makeMutableArrayRef(getStorage().data(), getStorage().size()).drop_front(Offset);
  }

  ParsedRawSyntaxNode makeUnknownSyntax(SyntaxKind Kind,
                                  MutableArrayRef<ParsedRawSyntaxNode> Parts);
  ParsedRawSyntaxNode createSyntaxAs(SyntaxKind Kind,
                                     MutableArrayRef<ParsedRawSyntaxNode> Parts,
                                     SyntaxNodeCreationKind nodeCreateK);
  Optional<ParsedRawSyntaxNode> bridgeAs(SyntaxContextKind Kind,
                              MutableArrayRef<ParsedRawSyntaxNode> Parts);

public:
  /// Construct root context.
  SyntaxParsingContext(SyntaxParsingContext *&CtxtHolder, SourceFile &SF,
                       unsigned BufferID,
                       std::shared_ptr<SyntaxParseActions> SPActions);

  /// Designated constructor for child context.
  SyntaxParsingContext(SyntaxParsingContext *&CtxtHolder)
      : RootDataOrParent(CtxtHolder), CtxtHolder(CtxtHolder),
        RootData(CtxtHolder->RootData), Offset(RootData->Storage.size()),
        IsBacktracking(CtxtHolder->IsBacktracking),
        ShouldDefer(CtxtHolder->ShouldDefer),
        Enabled(CtxtHolder->isEnabled()) {
    assert(CtxtHolder->isTopOfContextStack() &&
           "SyntaxParsingContext cannot have multiple children");
    assert(CtxtHolder->Mode != AccumulationMode::SkippedForIncrementalUpdate &&
           "Cannot create child context for a node loaded from the cache");
    CtxtHolder = this;
  }

  SyntaxParsingContext(SyntaxParsingContext *&CtxtHolder, SyntaxContextKind Kind)
      : SyntaxParsingContext(CtxtHolder) {
    setCoerceKind(Kind);
  }

  SyntaxParsingContext(SyntaxParsingContext *&CtxtHolder, SyntaxKind Kind)
      : SyntaxParsingContext(CtxtHolder) {
    setCreateSyntax(Kind);
  }

  ~SyntaxParsingContext();

  /// Try looking up if an unmodified node exists at \p LexerOffset of the same
  /// kind. If a node is found, replace the node that is currently being
  /// constructed by the parsing context with the found node and return the
  /// number of bytes the found node took up in the original source. The lexer
  /// should pretend it has read these bytes and continue from the advanced
  /// offset. If nothing is found \c 0 is returned.
  size_t lookupNode(size_t LexerOffset, SourceLoc Loc);

  void disable() { Enabled = false; }
  bool isEnabled() const { return Enabled; }
  bool isRoot() const { return RootDataOrParent.is<RootContextData*>(); }
  bool isTopOfContextStack() const { return this == CtxtHolder; }

  SyntaxParsingContext *getParent() const {
    return RootDataOrParent.get<SyntaxParsingContext*>();
  }

  RootContextData *getRootData() { return RootData; }

  const RootContextData *getRootData() const { return RootData; }

  std::vector<ParsedRawSyntaxNode> &getStorage() { return getRootData()->Storage; }

  const std::vector<ParsedRawSyntaxNode> &getStorage() const {
    return getRootData()->Storage;
  }

  const SyntaxParsingContext *getRoot() const;

  ParsedRawSyntaxRecorder &getRecorder() { return getRootData()->Recorder; }

  llvm::BumpPtrAllocator &getScratchAlloc() {
    return getRootData()->ScratchAlloc;
  }

  /// Add RawSyntax to the parts.
  void addRawSyntax(ParsedRawSyntaxNode Raw);

  /// Add Token with Trivia to the parts.
  void addToken(Token &Tok, const ParsedTrivia &LeadingTrivia,
                const ParsedTrivia &TrailingTrivia);

  /// Add Syntax to the parts.
  void addSyntax(ParsedSyntax Node);

  template<typename SyntaxNode>
  llvm::Optional<SyntaxNode> popIf() {
    auto &Storage = getStorage();
    if (Storage.size() <= Offset)
      return llvm::None;
    if (!SyntaxNode::kindof(Storage.back().getKind()))
      return llvm::None;
    auto rawNode = std::move(Storage.back());
    Storage.pop_back();
    return SyntaxNode(std::move(rawNode));
  }

  ParsedTokenSyntax popToken();

  /// Create a node using the tail of the collected parts. The number of parts
  /// is automatically determined from \c Kind. Node: limited number of \c Kind
  /// are supported. See the implementation.
  void createNodeInPlace(SyntaxKind Kind,
      SyntaxNodeCreationKind nodeCreateK = SyntaxNodeCreationKind::Recorded);

  /// Squashing nodes from the back of the pending syntax list to a given syntax
  /// collection kind. If there're no nodes that can fit into the collection kind,
  /// this function does nothing. Otherwise, it creates a collection node in place
  /// to contain all sequential suitable nodes from back.
  void collectNodesInPlace(SyntaxKind ColletionKind,
       SyntaxNodeCreationKind nodeCreateK = SyntaxNodeCreationKind::Recorded);

  /// On destruction, construct a specified kind of syntax node consuming the
  /// collected parts, then append it to the parent context.
  void setCreateSyntax(SyntaxKind Kind) {
    Mode = AccumulationMode::CreateSyntax;
    SynKind = Kind;
  }

  /// Same as \c setCreateSyntax but create a deferred node instead of a
  /// recorded one.
  void setDeferSyntax(SyntaxKind Kind) {
    Mode = AccumulationMode::DeferSyntax;
    SynKind = Kind;
  }

  /// On destruction, if the parts size is 1 and it's kind of \c Kind, just
  /// append it to the parent context. Otherwise, create Unknown{Kind} node from
  /// the collected parts.
  void setCoerceKind(SyntaxContextKind Kind) {
    Mode = AccumulationMode::CoerceKind;
    CtxtKind = Kind;
  }

  /// Move the collected parts to the tail of parent context.
  void setTransparent() { Mode = AccumulationMode::Transparent; }

  /// This context is a back tracking context, so we should discard collected
  /// parts on this context.
  void setBackTracking() {
    Mode = AccumulationMode::Discard;
    IsBacktracking = true;
  }

  bool isBacktracking() const { return IsBacktracking; }

  void setShouldDefer(bool Value = true) { ShouldDefer = Value; }

  bool shouldDefer() const {
    return ShouldDefer || IsBacktracking || Mode == AccumulationMode::Discard;
  }

  /// Explicitly finalizing syntax tree creation.
  /// This function will be called during the destroying of a root syntax
  /// parsing context. However, we can explicitly call this function to get
  /// the syntax tree before closing the root context.
  OpaqueSyntaxNode finalizeRoot();

  /// Make a missing node corresponding to the given token kind and
  /// push this node into the context. The synthesized node can help
  /// the creation of valid syntax nodes.
  void synthesize(tok Kind, SourceLoc Loc);

  /// Dump the nodes that are in the storage stack of the SyntaxParsingContext
  SWIFT_DEBUG_DUMPER(dumpStorage());
};

} // namespace swift
#endif // SWIFT_SYNTAX_PARSING_CONTEXT_H
