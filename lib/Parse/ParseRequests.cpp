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

using namespace swift;

namespace swift {
// Implement the type checker type zone (zone 10).
#define SWIFT_TYPEID_ZONE Parse
#define SWIFT_TYPEID_HEADER "swift/AST/ParseTypeIDZone.def"
#include "swift/Basic/ImplementTypeIDZone.h"
#undef SWIFT_TYPEID_ZONE
#undef SWIFT_TYPEID_HEADER
}

ArrayRef<Decl *>
ParseMembersRequest::evaluate(Evaluator &evaluator,
                              IterableDeclContext *idc) const {
  SourceFile &sf = *idc->getDecl()->getDeclContext()->getParentSourceFile();
  unsigned bufferID = *sf.getBufferID();

  // Lexer diaganostics have been emitted during skipping, so we disable lexer's
  // diagnostic engine here.
  Parser parser(bufferID, sf, /*No Lexer Diags*/nullptr, nullptr, nullptr);
  // Disable libSyntax creation in the delayed parsing.
  parser.SyntaxContext->setDiscard();
  ASTContext &ctx = idc->getDecl()->getASTContext();
  return ctx.AllocateCopy(
      llvm::makeArrayRef(parser.parseDeclListDelayed(idc)));
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
    parser.SyntaxContext->setDiscard();
    auto body = parser.parseAbstractFunctionBodyDelayed(afd);
    afd->setBodyKind(BodyKind::Parsed);
    return body;
  }
  }
  llvm_unreachable("Unhandled BodyKind in switch");
}


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
