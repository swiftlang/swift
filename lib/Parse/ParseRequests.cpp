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
#include "swift/AST/ASTWalker.h"
#include "swift/AST/AvailabilitySpec.h"
#include "swift/AST/Decl.h"
#include "swift/AST/DeclContext.h"
#include "swift/AST/DiagnosticsParse.h"
#include "swift/AST/Module.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/TypeRepr.h"
#include "swift/Basic/Assertions.h"
#include "swift/Basic/Defer.h"
#include "swift/Bridging/ASTGen.h"
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

void swift::simple_display(llvm::raw_ostream &out,
                           const FingerprintAndMembers &value) {
  if (value.fingerprint)
    simple_display(out, *value.fingerprint);
  else
    out << "<no fingerprint>";
  out << ", ";
  simple_display(out, value.members);
}

namespace {

/// Find 'ValueDecl' with opaque type results and let \p SF track the opaque
/// return type mapping.
// FIXME: This should be done in Sema or AST?
class OpaqueResultCollector : public ASTWalker {
  SourceFile &SF;

public:
  OpaqueResultCollector(SourceFile &SF) : SF(SF) {}

  void handle(ValueDecl *VD) const {
    TypeRepr *tyR = nullptr;
    if (auto *FD = dyn_cast<FuncDecl>(VD)) {
      if (isa<AccessorDecl>(VD))
        return;
      tyR = FD->getResultTypeRepr();
    } else if (auto *VarD = dyn_cast<VarDecl>(VD)) {
      if (!VarD->getParentPatternBinding())
        return;
      tyR = VarD->getTypeReprOrParentPatternTypeRepr();
    } else if (auto *SD = dyn_cast<SubscriptDecl>(VD)) {
      tyR = SD->getElementTypeRepr();
    }

    if (!tyR || !tyR->hasOpaque())
      return;

    SF.addUnvalidatedDeclWithOpaqueResultType(VD);
  }

  PreWalkAction walkToDeclPre(Decl *D) override {
    if (auto *VD = dyn_cast<ValueDecl>(D))
      handle(VD);

    if (auto *IDC = dyn_cast<IterableDeclContext>(D)) {
      // Only walk into cached parsed members to avoid invoking lazy parsing.
      ParseMembersRequest req(IDC);
      if (SF.getASTContext().evaluator.hasCachedResult(req)) {
        auto memberResult = evaluateOrFatal(SF.getASTContext().evaluator, req);
        for (auto *D : memberResult.members)
          D->walk(*this);
      }
      return Action::SkipChildren();
    }

    return Action::Continue();
  }

  MacroWalking getMacroWalkingBehavior() const override {
    return MacroWalking::None;
  }

  PreWalkAction walkToTypeReprPre(TypeRepr *T) override {
    return Action::SkipNode();
  }
};

void registerDeclWithOpaqueResultType(SourceFile *SF, ArrayRef<ASTNode> items) {
  if (!SF)
    return;
  OpaqueResultCollector walker(*SF);
  for (const ASTNode &item : items)
    const_cast<ASTNode &>(item).walk(walker);
}

void registerDeclWithOpaqueResultType(SourceFile *SF, ArrayRef<Decl *> items) {
  if (!SF)
    return;
  OpaqueResultCollector walker(*SF);
  for (Decl *item : items)
    item->walk(walker);
}

} // namespace

FingerprintAndMembers
ParseMembersRequest::evaluate(Evaluator &evaluator,
                              IterableDeclContext *idc) const {
  SourceFile *sf = idc->getAsGenericContext()->getParentSourceFile();
  ASTContext &ctx = idc->getDecl()->getASTContext();
  auto fileUnit
    = dyn_cast<FileUnit>(idc->getAsGenericContext()->getModuleScopeContext());
  if (!sf) {
    // If there is no parent source file, this is a deserialized or synthesized
    // declaration context, in which case `getMembers()` has all of the members.
    // Filter out the implicitly-generated ones.
    SmallVector<Decl *, 4> members;
    for (auto decl : idc->getMembers()) {
      if (!decl->isImplicit()) {
        members.push_back(decl);
      }
    }

    std::optional<Fingerprint> fp = std::nullopt;
    if (!idc->getDecl()->isImplicit() && fileUnit) {
      fp = fileUnit->loadFingerprint(idc);
    }
    return FingerprintAndMembers{fp, ctx.AllocateCopy(members)};
  }

  unsigned bufferID = sf->getBufferID();

  // Lexer diagnostics have been emitted during skipping, so we disable lexer's
  // diagnostic engine here.
  Parser parser(bufferID, *sf, /*No Lexer Diags*/nullptr, nullptr, nullptr);
  parser.InFreestandingMacroArgument = idc->inFreestandingMacroArgument();

  auto declsAndHash = parser.parseDeclListDelayed(idc);
  FingerprintAndMembers fingerprintAndMembers = {declsAndHash.second,
                                                 declsAndHash.first};

  registerDeclWithOpaqueResultType(sf->getOutermostParentSourceFile(),
                                   fingerprintAndMembers.members);
  return FingerprintAndMembers{
      fingerprintAndMembers.fingerprint,
      ctx.AllocateCopy(llvm::ArrayRef(fingerprintAndMembers.members))};
}

BodyAndFingerprint
ParseAbstractFunctionBodyRequest::evaluate(Evaluator &evaluator,
                                           AbstractFunctionDecl *afd) const {
  using BodyKind = AbstractFunctionDecl::BodyKind;

  switch (afd->getBodyKind()) {
  case BodyKind::Deserialized:
  case BodyKind::SILSynthesize:
  case BodyKind::None:
    return {};

  case BodyKind::TypeChecked:
  case BodyKind::Parsed:
    return afd->BodyAndFP;

  case BodyKind::Synthesize: {
    BraceStmt *body;
    bool isTypeChecked;

    std::tie(body, isTypeChecked) = (afd->Synthesizer.Fn)(
        afd, afd->Synthesizer.Context);
    assert(body && "cannot synthesize a null body");
    afd->setBodyKind(isTypeChecked ? BodyKind::TypeChecked : BodyKind::Parsed);
    return {body, Fingerprint::ZERO()};
  }

  case BodyKind::Unparsed: {
    // FIXME: How do we configure code completion?
    SourceManager &sourceMgr = afd->getASTContext().SourceMgr;
    unsigned bufferID =
        sourceMgr.findBufferContainingLoc(afd->getBodySourceRange().Start);
    SourceFile *sf = afd->getDeclContext()->getParentSourceFile();
    if (!sf) {
      auto sourceFiles = sourceMgr.getSourceFilesForBufferID(bufferID);
      auto expectedModule = afd->getParentModule();
      for (auto checkSF: sourceFiles) {
        if (checkSF->getParentModule() == expectedModule) {
          sf = checkSF;
          break;
        }
      }
      assert(sf && "Could not find source file containing parsed body");
    }

    Parser parser(bufferID, *sf, /*SIL*/ nullptr);
    auto result = parser.parseAbstractFunctionBodyDelayed(afd);
    afd->setBodyKind(BodyKind::Parsed);
    registerDeclWithOpaqueResultType(afd->getOutermostParentSourceFile(),
                                     result.getBody()->getElements());
    return result;
  }
  }
  llvm_unreachable("Unhandled BodyKind in switch");
}

//----------------------------------------------------------------------------//
// ExportedSourceFileRequest computation.
//----------------------------------------------------------------------------//

static BridgedGeneratedSourceFileKind
getBridgedGeneratedSourceFileKind(const GeneratedSourceInfo *genInfo) {
  if (!genInfo)
    return BridgedGeneratedSourceFileKindNone;

  switch (genInfo->kind) {

#define MACRO_ROLE(Name, Description)                                          \
  case GeneratedSourceInfo::Kind::Name##MacroExpansion:                        \
    return BridgedGeneratedSourceFileKind##Name##MacroExpansion;
#include "swift/Basic/MacroRoles.def"
#undef MACRO_ROLE

  case GeneratedSourceInfo::Kind::ReplacedFunctionBody:
    return BridgedGeneratedSourceFileKindReplacedFunctionBody;
  case GeneratedSourceInfo::Kind::PrettyPrinted:
    return BridgedGeneratedSourceFileKindPrettyPrinted;
  case GeneratedSourceInfo::Kind::DefaultArgument:
    return BridgedGeneratedSourceFileKindDefaultArgument;
  case GeneratedSourceInfo::AttributeFromClang:
    return BridgedGeneratedSourceFileKindAttributeFromClang;
  }
}

void *ExportedSourceFileRequest::evaluate(Evaluator &evaluator,
                                          const SourceFile *SF) const {
#if SWIFT_BUILD_SWIFT_SYNTAX
  // The SwiftSyntax parser doesn't (yet?) handle SIL.
  if (SF->Kind == SourceFileKind::SIL)
    return nullptr;

  auto &ctx = SF->getASTContext();

  const auto *genInfo = SF->getGeneratedSourceFileInfo();
  DeclContext *dc = const_cast<SourceFile *>(SF);
  if (genInfo && genInfo->declContext)
    dc = genInfo->declContext;

  // Parse the source file.
  auto exportedSourceFile = swift_ASTGen_parseSourceFile(
      SF->getBuffer(), SF->getParentModule()->getName().str(),
      SF->getFilename(), dc, getBridgedGeneratedSourceFileKind(genInfo));

  // Round-trip validation if needed.
  if (SF->getParsingOptions().contains(SourceFile::ParsingFlags::RoundTrip)) {
    if (swift_ASTGen_roundTripCheck(exportedSourceFile)) {
      SourceLoc loc = ctx.SourceMgr.getLocForBufferStart(SF->getBufferID());
      ctx.Diags.diagnose(loc, diag::parser_round_trip_error);
    }
  }

  ctx.addCleanup([exportedSourceFile] {
    swift_ASTGen_destroySourceFile(exportedSourceFile);
  });
  return exportedSourceFile;
#else
  return nullptr;
#endif // SWIFT_BUILD_SWIFT_SYNTAX
}

//----------------------------------------------------------------------------//
// ParseSourceFileRequest computation.
//----------------------------------------------------------------------------//

namespace {

#if SWIFT_BUILD_SWIFT_SYNTAX
/// Whether we can "parse" the source file via ASTGen.
bool shouldParseViaASTGen(SourceFile &SF) {
  auto &ctx = SF.getASTContext();
  auto &langOpts = ctx.LangOpts;

  if (!langOpts.hasFeature(Feature::ParserASTGen))
    return false;

  switch (SF.Kind) {
    case SourceFileKind::SIL:
      return false;
    case SourceFileKind::Library:
    case SourceFileKind::Main:
    case SourceFileKind::Interface:
    case SourceFileKind::MacroExpansion:
    case SourceFileKind::DefaultArgument:
      break;
  }

  // TODO: Migrate SourceKit features to Syntax based.
  if (SF.shouldCollectTokens())
    return false;

  // TODO: Implement DebuggerContextChange in ASTGen.
  if (langOpts.DebuggerSupport)
    return false;

  // TODO: IDE inspection (code completion) support in ASTGen.
  if (ctx.SourceMgr.getIDEInspectionTargetBufferID() == SF.getBufferID())
    return false;

  if (auto *generatedInfo = SF.getGeneratedSourceFileInfo()) {
    // TODO: Handle generated.
    if (generatedInfo->kind == GeneratedSourceInfo::Kind::AttributeFromClang) {
      return false;
    }
  }

  return true;
}

template <typename Bridged, typename Native>
void appendToVector(Bridged cNode, void *vecPtr) {
  auto vec = static_cast<SmallVectorImpl<Native> *>(vecPtr);
  vec->push_back(cNode.unbridged());
}

SourceFileParsingResult parseSourceFileViaASTGen(SourceFile &SF) {
  ASTContext &Ctx = SF.getASTContext();
  DiagnosticEngine &Diags = Ctx.Diags;
  SourceManager &SM = Ctx.SourceMgr;
  const LangOptions &langOpts = Ctx.LangOpts;
  const GeneratedSourceInfo *genInfo = SF.getGeneratedSourceFileInfo();

  DeclContext *declContext = &SF;
  if (genInfo && genInfo->declContext) {
    declContext = genInfo->declContext;
  }
  Decl *attachedDecl = nullptr;
  if (genInfo && genInfo->astNode) {
    attachedDecl =
        ASTNode::getFromOpaqueValue(genInfo->astNode).dyn_cast<Decl *>();
  }

  // Parse the file.
  auto *exportedSourceFile = SF.getExportedSourceFile();
  assert(exportedSourceFile && "Couldn't parse via SyntaxParser");

  // Collect virtual files.
  // FIXME: Avoid side effects in the request.
  // FIXME: Do this lazily in SourceManager::getVirtualFile().
  BridgedVirtualFile *virtualFiles = nullptr;
  size_t numVirtualFiles =
      swift_ASTGen_virtualFiles(exportedSourceFile, &virtualFiles);
  SourceLoc bufferStart = SM.getLocForBufferStart(SF.getBufferID());
  for (size_t i = 0; i != numVirtualFiles; ++i) {
    auto &VF = virtualFiles[i];
    Ctx.SourceMgr.createVirtualFile(
        bufferStart.getAdvancedLoc(VF.StartPosition), VF.Name.unbridged(),
        VF.LineOffset, VF.EndPosition - VF.StartPosition);
    StringRef name = Ctx.AllocateCopy(VF.Name.unbridged());
    SF.VirtualFilePaths.emplace_back(
        name, bufferStart.getAdvancedLoc(VF.NamePosition));
  }
  swift_ASTGen_freeBridgedVirtualFiles(virtualFiles, numVirtualFiles);

  // Emit parser diagnostics.
  (void)swift_ASTGen_emitParserDiagnostics(
      Ctx, &Diags, exportedSourceFile, /*emitOnlyErrors=*/false,
      /*downgradePlaceholderErrorsToWarnings=*/langOpts.Playground ||
          langOpts.WarnOnEditorPlaceholder);

  // Generate AST nodes.
  SmallVector<ASTNode, 128> items;
  swift_ASTGen_buildTopLevelASTNodes(
      &Diags, exportedSourceFile, declContext, attachedDecl, Ctx,
      static_cast<SmallVectorImpl<ASTNode> *>(&items),
      appendToVector<BridgedASTNode, ASTNode>);

  // Fingerprint.
  // FIXME: Split request (SourceFileFingerprintRequest).
  std::optional<Fingerprint> fp;
  if (SF.hasInterfaceHash())
    fp = swift_ASTGen_getSourceFileFingerprint(exportedSourceFile, Ctx)
             .unbridged();

  registerDeclWithOpaqueResultType(declContext->getOutermostParentSourceFile(),
                                   items);

  return SourceFileParsingResult{/*TopLevelItems=*/Ctx.AllocateCopy(items),
                                 /*CollectedTokens=*/std::nullopt,
                                 /*Fingerprint=*/fp};
}
#endif // SWIFT_BUILD_SWIFT_SYNTAX

/// A thunk that deletes an allocated PersistentParserState. This is needed for
/// us to be able to forward declare a unique_ptr to the state in the AST.
void deletePersistentParserState(PersistentParserState *state) { delete state; }

SourceFileParsingResult parseSourceFile(SourceFile &SF) {
  auto &ctx = SF.getASTContext();
  auto bufferID = SF.getBufferID();

  // If this buffer is for IDE functionality, hook up the state needed by its
  // second pass.
  PersistentParserState *state = nullptr;
  if (ctx.SourceMgr.getIDEInspectionTargetBufferID() == bufferID) {
    state = new PersistentParserState();
    SF.setDelayedParserState({state, &deletePersistentParserState});
  }

  Parser parser(bufferID, SF, /*SIL*/ nullptr, state);
  PrettyStackTraceParser StackTrace(parser);

  // If the buffer is generated source information, we might have more
  // context that we need to set up for parsing.
  SmallVector<ASTNode, 128> items;
  if (auto generatedInfo = ctx.SourceMgr.getGeneratedSourceInfo(bufferID)) {
    if (generatedInfo->declContext)
      parser.CurDeclContext = generatedInfo->declContext;

    switch (generatedInfo->kind) {
    case GeneratedSourceInfo::DeclarationMacroExpansion:
    case GeneratedSourceInfo::CodeItemMacroExpansion:
      if (parser.CurDeclContext->isTypeContext()) {
        parser.parseExpandedMemberList(items);
      } else {
        parser.parseTopLevelItems(items);
      }
      break;

    case GeneratedSourceInfo::ExpressionMacroExpansion:
    case GeneratedSourceInfo::DefaultArgument: {
      // Prime the lexer.
      if (parser.Tok.is(tok::NUM_TOKENS))
        parser.consumeTokenWithoutFeedingReceiver();

      ParserResult<Expr> resultExpr = parser.parseExpr(diag::expected_expr);
      if (auto expr = resultExpr.getPtrOrNull())
        items.push_back(expr);

      if (!parser.Tok.is(tok::eof)) {
        parser.diagnose(parser.Tok, diag::extra_tokens_after_expression);
        while (!parser.Tok.is(tok::eof))
          parser.consumeToken();
      }

      break;
    }

    case GeneratedSourceInfo::PreambleMacroExpansion:
    case GeneratedSourceInfo::ReplacedFunctionBody:
    case GeneratedSourceInfo::PrettyPrinted:{
      parser.parseTopLevelItems(items);
      break;
    }

    case GeneratedSourceInfo::BodyMacroExpansion: {
      // Prime the lexer.
      if (parser.Tok.is(tok::NUM_TOKENS))
        parser.consumeTokenWithoutFeedingReceiver();

      if (parser.Tok.is(tok::l_brace)) {
        if (auto body =
                parser.parseBraceItemList(diag::invalid_diagnostic)
                  .getPtrOrNull())
          items.push_back(body);
      }

      break;
    }

    case GeneratedSourceInfo::MemberMacroExpansion: {
      parser.parseExpandedMemberList(items);
      break;
    }

    case GeneratedSourceInfo::AccessorMacroExpansion: {
      ASTNode astNode = ASTNode::getFromOpaqueValue(generatedInfo->astNode);
      auto attachedDecl = astNode.get<Decl *>();
      auto accessorsForStorage = dyn_cast<AbstractStorageDecl>(attachedDecl);

      parser.parseTopLevelAccessors(accessorsForStorage, items);
      break;
    }

    case GeneratedSourceInfo::MemberAttributeMacroExpansion:
      parser.parseExpandedAttributeList(items, /*isFromClangAttribute=*/false);
      break;

    case GeneratedSourceInfo::AttributeFromClang:
      parser.parseExpandedAttributeList(items, /*isFromClangAttribute=*/true);
      break;

    case GeneratedSourceInfo::PeerMacroExpansion: {
      if (parser.CurDeclContext->isTypeContext()) {
        parser.parseExpandedMemberList(items);
      } else {
        parser.parseTopLevelItems(items);
      }
      break;
    }

    case GeneratedSourceInfo::ConformanceMacroExpansion:
    case GeneratedSourceInfo::ExtensionMacroExpansion: {
      parser.parseTopLevelItems(items);
      break;
    }
    }
  } else {
    parser.parseTopLevelItems(items);
  }

  std::optional<ArrayRef<Token>> tokensRef;
  if (auto tokens = parser.takeTokenReceiver()->finalize())
    tokensRef = ctx.AllocateCopy(*tokens);

  std::optional<Fingerprint> fp;
  if (parser.CurrentTokenHash)
    fp = Fingerprint(std::move(*parser.CurrentTokenHash));

  registerDeclWithOpaqueResultType(
      parser.CurDeclContext->getOutermostParentSourceFile(), items);

  return SourceFileParsingResult{ctx.AllocateCopy(items), tokensRef, fp};
}

} // namespace

SourceFileParsingResult ParseSourceFileRequest::evaluate(Evaluator &evaluator,
                                                         SourceFile *SF) const {
  assert(SF);
  auto &ctx = SF->getASTContext();

  // If we've been asked to silence warnings, do so now. This is needed for
  // secondary files, which can be parsed multiple times.
  auto &diags = ctx.Diags;
  auto didSuppressWarnings = diags.getSuppressWarnings();
  auto shouldSuppress = SF->getParsingOptions().contains(
      SourceFile::ParsingFlags::SuppressWarnings);
  diags.setSuppressWarnings(didSuppressWarnings || shouldSuppress);
  SWIFT_DEFER { diags.setSuppressWarnings(didSuppressWarnings); };

#if SWIFT_BUILD_SWIFT_SYNTAX
  if (shouldParseViaASTGen(*SF))
    return parseSourceFileViaASTGen(*SF);
#endif

  return parseSourceFile(*SF);
}

evaluator::DependencySource ParseSourceFileRequest::readDependencySource(
    const evaluator::DependencyRecorder &e) const {
  return std::get<0>(getStorage());
}

std::optional<SourceFileParsingResult>
ParseSourceFileRequest::getCachedResult() const {
  auto *SF = std::get<0>(getStorage());
  auto items = SF->getCachedTopLevelItems();
  if (!items)
    return std::nullopt;

  return SourceFileParsingResult{*items, SF->AllCollectedTokens,
                                 SF->InterfaceHash};
}

void ParseSourceFileRequest::cacheResult(SourceFileParsingResult result) const {
  auto *SF = std::get<0>(getStorage());
  assert(!SF->Items);
  SF->Items = result.TopLevelItems;
  SF->AllCollectedTokens = result.CollectedTokens;
  SF->InterfaceHash = result.Fingerprint;

  // Verify the parsed source file.
  verify(*SF);
}

ArrayRef<Decl *> ParseTopLevelDeclsRequest::evaluate(
    Evaluator &evaluator, SourceFile *SF) const {
  auto items = evaluateOrDefault(evaluator, ParseSourceFileRequest{SF}, {})
    .TopLevelItems;

  std::vector<Decl *> decls;
  for (auto item : items) {
    if (auto decl = item.dyn_cast<Decl *>())
      decls.push_back(decl);
  }

  return SF->getASTContext().AllocateCopy(decls);
}

//----------------------------------------------------------------------------//
// AvailabilityMacroArgumentsRequest computation.
//----------------------------------------------------------------------------//

namespace {
bool parseAvailabilityMacroDefinitionViaASTGen(
    unsigned bufferID, ASTContext &ctx, std::string &name,
    llvm::VersionTuple &version, SmallVectorImpl<AvailabilitySpec *> &specs) {
  StringRef buffer = ctx.SourceMgr.getEntireTextForBuffer(bufferID);
  DeclContext *dc = ctx.MainModule;

  BridgedAvailabilityMacroDefinition parsed;
  bool hadError = swift_ASTGen_parseAvailabilityMacroDefinition(ctx, dc, &ctx.Diags, buffer,
                                                  &parsed);
  if (hadError)
    return true;
  SWIFT_DEFER { swift_ASTGen_freeAvailabilityMacroDefinition(&parsed); };

  name = parsed.name.unbridged();
  version = parsed.version.unbridged();
  specs.clear();
  for (auto spec : parsed.specs.unbridged<BridgedAvailabilitySpec>()) {
    specs.push_back(spec.unbridged());
  }

  return false;
}

bool parseAvailabilityMacroDefinitionViaLegacyParser(
    unsigned bufferID, ASTContext &ctx, std::string &name,
    llvm::VersionTuple &version, SmallVectorImpl<AvailabilitySpec *> &specs) {
  auto &SM = ctx.SourceMgr;

  // Create temporary parser.
  swift::ParserUnit PU(SM, SourceFileKind::Main, bufferID, ctx.LangOpts,
                       "unknown");

  ForwardingDiagnosticConsumer PDC(ctx.Diags);
  PU.getDiagnosticEngine().addConsumer(PDC);

  // Parse the argument.
  SmallVector<AvailabilitySpec *, 4> tmpSpecs;
  ParserStatus status =
      PU.getParser().parseAvailabilityMacroDefinition(name, version, tmpSpecs);
  if (status.isError())
    return true;

  // Copy the Specs to the requesting ASTContext from the temporary context
  // in the ParserUnit.
  for (auto *spec : tmpSpecs) {
    specs.push_back(spec->clone(ctx));
  }

  return false;
}

bool parseAvailabilityMacroDefinition(
    unsigned bufferID, ASTContext &ctx, std::string &name,
    llvm::VersionTuple &version, SmallVectorImpl<AvailabilitySpec *> &specs) {
#if SWIFT_BUILD_SWIFT_SYNTAX
  if (ctx.LangOpts.hasFeature(Feature::ParserASTGen))
    return parseAvailabilityMacroDefinitionViaASTGen(bufferID, ctx, name,
                                                     version, specs);
#endif
  return parseAvailabilityMacroDefinitionViaLegacyParser(bufferID, ctx, name,
                                                         version, specs);
}

} // namespace

const AvailabilityMacroMap *
AvailabilityMacroArgumentsRequest::evaluate(Evaluator &evaluator,
                                            ASTContext *ctx) const {
  SourceManager &SM = ctx->SourceMgr;

  // Allocate all buffers in one go to avoid repeating the sorting in
  // findBufferContainingLocInternal.
  llvm::SmallVector<unsigned, 4> bufferIDs;
  for (auto macro : ctx->LangOpts.AvailabilityMacros) {
    unsigned bufferID =
        SM.addMemBufferCopy(macro, "-define-availability argument");
    bufferIDs.push_back(bufferID);
  }

  auto *map = new AvailabilityMacroMap();
  ctx->addCleanup([map]() { delete map; });

  // Parse each macro definition.
  for (unsigned bufferID : bufferIDs) {
    std::string name;
    llvm::VersionTuple version;
    SmallVector<AvailabilitySpec *, 4> specs;
    if (parseAvailabilityMacroDefinition(bufferID, *ctx, name, version, specs))
      continue;

    if (map->hasMacroNameVersion(name, version)) {
      ctx->Diags.diagnose(SM.getLocForBufferStart(bufferID),
                          diag::attr_availability_duplicate, name,
                          version.getAsString());
      continue;
    }

    map->addEntry(name, version, specs);
  }

  return map;
}

//----------------------------------------------------------------------------//
// IDEInspectionSecondPassRequest computation.
//----------------------------------------------------------------------------//

void swift::simple_display(llvm::raw_ostream &out,
                           const IDEInspectionCallbacksFactory *factory) { }

evaluator::DependencySource
IDEInspectionSecondPassRequest::readDependencySource(
    const evaluator::DependencyRecorder &e) const {
  return std::get<0>(getStorage());
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
