//===--- ParseRequests.cpp - Parsing Requests -----------------------------===//
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
// Lazy parsing requests
//
//===----------------------------------------------------------------------===//
#include "swift/AST/ParseRequests.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DeclContext.h"
#include "swift/AST/Module.h"
#include "swift/AST/SourceFile.h"
#include "swift/Parse/Parser.h"
#include "swift/Subsystems.h"
#include "swift/Syntax/SyntaxArena.h"
#include "swift/SyntaxParse/SyntaxTreeCreator.h"

using namespace swift;

namespace swift {
// Implement the type checker type zone (zone 10).
#define SWIFT_TYPEID_ZONE Parse
#define SWIFT_TYPEID_HEADER "swift/AST/ParseTypeIDZone.def"
#include "swift/Basic/ImplementTypeIDZone.h"
#undef SWIFT_TYPEID_ZONE
#undef SWIFT_TYPEID_HEADER
}

void swift::simple_display(llvm::raw_ostream &out,
                           const FingerprintAndMembers &value) {
  if (value.fingerprint)
    simple_display(out, value.fingerprint.getValue());
  else
    out << "<no fingerprint>";
  out << ", ";
  simple_display(out, value.members);
}

FingerprintAndMembers
ParseMembersRequest::evaluate(Evaluator &evaluator,
                              IterableDeclContext *idc) const {
  SourceFile &sf = *idc->getDecl()->getDeclContext()->getParentSourceFile();
  unsigned bufferID = *sf.getBufferID();

  // Lexer diaganostics have been emitted during skipping, so we disable lexer's
  // diagnostic engine here.
  Parser parser(bufferID, sf, /*No Lexer Diags*/nullptr, nullptr, nullptr);
  // Disable libSyntax creation in the delayed parsing.
  parser.SyntaxContext->disable();
  ASTContext &ctx = idc->getDecl()->getASTContext();
  auto declsAndHash = parser.parseDeclListDelayed(idc);
  FingerprintAndMembers fingerprintAndMembers = {declsAndHash.second,
                                                 declsAndHash.first};
  return FingerprintAndMembers{
      fingerprintAndMembers.fingerprint,
      ctx.AllocateCopy(llvm::makeArrayRef(fingerprintAndMembers.members))};
}

BraceStmt *ParseAbstractFunctionBodyRequest::evaluate(
    Evaluator &evaluator, AbstractFunctionDecl *afd) const {
  using BodyKind = AbstractFunctionDecl::BodyKind;

  switch (afd->getBodyKind()) {
  case BodyKind::Deserialized:
  case BodyKind::MemberwiseInitializer:
  case BodyKind::None:
  case BodyKind::Skipped:
    return nullptr;

  case BodyKind::TypeChecked:
  case BodyKind::Parsed:
    return afd->Body;

  case BodyKind::Synthesize: {
    BraceStmt *body;
    bool isTypeChecked;

    std::tie(body, isTypeChecked) = (afd->Synthesizer.Fn)(
        afd, afd->Synthesizer.Context);
    assert(body && "cannot synthesize a null body");
    afd->setBodyKind(isTypeChecked ? BodyKind::TypeChecked : BodyKind::Parsed);
    return body;
  }

  case BodyKind::Unparsed: {
    // FIXME: How do we configure code completion?
    SourceFile &sf = *afd->getDeclContext()->getParentSourceFile();
    SourceManager &sourceMgr = sf.getASTContext().SourceMgr;
    unsigned bufferID = sourceMgr.findBufferContainingLoc(afd->getLoc());
    Parser parser(bufferID, sf, static_cast<SILParserTUStateBase *>(nullptr),
                  nullptr, nullptr);
    parser.SyntaxContext->disable();
    auto body = parser.parseAbstractFunctionBodyDelayed(afd);
    afd->setBodyKind(BodyKind::Parsed);
    return body;
  }
  }
  llvm_unreachable("Unhandled BodyKind in switch");
}

//----------------------------------------------------------------------------//
// ParseSourceFileRequest computation.
//----------------------------------------------------------------------------//

/// A thunk that deletes an allocated PersistentParserState. This is needed for
/// us to be able to forward declare a unique_ptr to the state in the AST.
static void deletePersistentParserState(PersistentParserState *state) {
  delete state;
}

ArrayRef<Decl *> ParseSourceFileRequest::evaluate(Evaluator &evaluator,
                                                  SourceFile *SF) const {
  assert(SF);
  auto &ctx = SF->getASTContext();
  auto bufferID = SF->getBufferID();

  // If there's no buffer, there's nothing to parse.
  if (!bufferID)
    return {};

  std::shared_ptr<SyntaxTreeCreator> sTreeCreator;
  if (SF->shouldBuildSyntaxTree()) {
    sTreeCreator = std::make_shared<SyntaxTreeCreator>(
        ctx.SourceMgr, *bufferID, SF->SyntaxParsingCache, ctx.getSyntaxArena());
  }

  // If we've been asked to silence warnings, do so now. This is needed for
  // secondary files, which can be parsed multiple times.
  auto &diags = ctx.Diags;
  auto didSuppressWarnings = diags.getSuppressWarnings();
  auto shouldSuppress = SF->getParsingOptions().contains(
      SourceFile::ParsingFlags::SuppressWarnings);
  diags.setSuppressWarnings(didSuppressWarnings || shouldSuppress);
  SWIFT_DEFER { diags.setSuppressWarnings(didSuppressWarnings); };

  // If this buffer is for code completion, hook up the state needed by its
  // second pass.
  PersistentParserState *state = nullptr;
  if (ctx.SourceMgr.getCodeCompletionBufferID() == bufferID) {
    state = new PersistentParserState();
    SF->setDelayedParserState({state, &deletePersistentParserState});
  }

  FrontendStatsTracer tracer(ctx.Stats, "Parsing");
  Parser parser(*bufferID, *SF, /*SIL*/ nullptr, state, sTreeCreator);
  PrettyStackTraceParser StackTrace(parser);

  llvm::SaveAndRestore<NullablePtr<llvm::MD5>> S(parser.CurrentTokenHash,
                                                 SF->getInterfaceHashPtr());

  SmallVector<Decl *, 128> decls;
  parser.parseTopLevel(decls);

  if (sTreeCreator) {
    auto rawNode = parser.finalizeSyntaxTree();
    sTreeCreator->acceptSyntaxRoot(rawNode, *SF);
  }
  return ctx.AllocateCopy(decls);
}

Optional<ArrayRef<Decl *>> ParseSourceFileRequest::getCachedResult() const {
  auto *SF = std::get<0>(getStorage());
  return SF->getCachedTopLevelDecls();
}

void ParseSourceFileRequest::cacheResult(ArrayRef<Decl *> decls) const {
  auto *SF = std::get<0>(getStorage());
  assert(!SF->Decls);
  SF->Decls = decls;

  // Verify the parsed source file.
  verify(*SF);
}

//----------------------------------------------------------------------------//
// CodeCompletionSecondPassRequest computation.
//----------------------------------------------------------------------------//


void swift::simple_display(llvm::raw_ostream &out,
                           const CodeCompletionCallbacksFactory *factory) { }

// Define request evaluation functions for each of the type checker requests.
static AbstractRequestFunction *parseRequestFunctions[] = {
#define SWIFT_REQUEST(Zone, Name, Sig, Caching, LocOptions)                    \
  reinterpret_cast<AbstractRequestFunction *>(&Name::evaluateRequest),
#include "swift/AST/ParseTypeIDZone.def"
#undef SWIFT_REQUEST
};

void swift::registerParseRequestFunctions(Evaluator &evaluator) {
  evaluator.registerRequestFunctions(Zone::Parse,
                                     parseRequestFunctions);
}
