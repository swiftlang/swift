//===--- SyntacticMacroExpansion.cpp --------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/IDETool/SyntacticMacroExpansion.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/MacroDefinition.h"
#include "swift/AST/PluginLoader.h"
#include "swift/AST/TypeRepr.h"
#include "swift/Driver/FrontendUtil.h"
#include "swift/Frontend/Frontend.h"
#include "swift/IDE/Utils.h"
#include "swift/Sema/IDETypeChecking.h"

using namespace swift;
using namespace ide;

std::shared_ptr<SyntacticMacroExpansionInstance>
SyntacticMacroExpansion::getInstance(ArrayRef<const char *> args,
                                     std::string &error) {
  // Compute the signature of the invocation.
  StableHasher argHasher = StableHasher::defaultHasher();
  for (auto arg : args)
    argHasher.combine(StringRef(arg));
  Fingerprint argHash(std::move(argHasher));

  // Check if the current instance is usable.
  if (auto currentInstance = this->currentInstance) {
    if (currentInstance->getArgHash() == argHash) {
      return currentInstance;
    }
  }

  // Create and configure a new instance.
  auto instance = std::make_shared<SyntacticMacroExpansionInstance>(argHash);

  bool failed = instance->setup(SwiftExecutablePath, args, Plugins, error);
  if (failed)
    return nullptr;

  currentInstance = instance;
  return instance;
}

bool SyntacticMacroExpansionInstance::setup(
    StringRef SwiftExecutablePath, ArrayRef<const char *> args,
    std::shared_ptr<PluginRegistry> plugins, std::string &error) {
  SmallString<256> driverPath(SwiftExecutablePath);
  llvm::sys::path::remove_filename(driverPath);
  llvm::sys::path::append(driverPath, "swiftc");

  // Setup CompilerInstance to configure the plugin search path correctly.
  bool hadError = driver::getSingleFrontendInvocationFromDriverArguments(
      driverPath, args, Diags,
      [&](ArrayRef<const char *> frontendArgs) {
        return invocation.parseArgs(
            frontendArgs, Diags, /*ConfigurationFileBuffers=*/nullptr,
            /*workingDirectory=*/{}, SwiftExecutablePath);
      },
      /*ForceNoOutput=*/true);
  if (hadError) {
    error = "failed to setup compiler invocation";
    return true;
  }

  // Setup filesystem.
  auto FS = invocation.getSearchPathOptions().makeOverlayFileSystem(
      SourceMgr.getFileSystem());
  if (!FS) {
    llvm::consumeError(FS.takeError());
    error = "failed to setup overlay filesystem";
    return true;
  }
  SourceMgr.setFileSystem(FS.get());

  // Setup ASTContext.
  Ctx.reset(ASTContext::get(
      invocation.getLangOptions(), invocation.getTypeCheckerOptions(),
      invocation.getSILOptions(), invocation.getSearchPathOptions(),
      invocation.getClangImporterOptions(), invocation.getSymbolGraphOptions(),
      SourceMgr, Diags));
  registerParseRequestFunctions(Ctx->evaluator);
  registerTypeCheckerRequestFunctions(Ctx->evaluator);

  std::unique_ptr<PluginLoader> pluginLoader =
      std::make_unique<PluginLoader>(*Ctx, /*DepTracker=*/nullptr);
  pluginLoader->setRegistry(plugins.get());
  Ctx->setPluginLoader(std::move(pluginLoader));

  // Create a module where SourceFiles reside.
  Identifier ID = Ctx->getIdentifier(invocation.getModuleName());
  TheModule = ModuleDecl::create(ID, *Ctx);

  return false;
}

SourceFile *
SyntacticMacroExpansionInstance::getSourceFile(llvm::MemoryBuffer *inputBuf) {

  // If there is a SourceFile with the same name and the content, use it.
  // Note that this finds the generated source file that was created in the
  // previous expansion requests.
  if (auto bufID =
          SourceMgr.getIDForBufferIdentifier(inputBuf->getBufferIdentifier())) {
    if (inputBuf->getBuffer() == SourceMgr.getEntireTextForBuffer(*bufID)) {
      SourceLoc bufLoc = SourceMgr.getLocForBufferStart(*bufID);
      if (SourceFile *existing =
              TheModule->getSourceFileContainingLocation(bufLoc)) {
        return existing;
      }
    }
  }

  // Otherwise, create a new SourceFile.
  SourceFile *SF = new (getASTContext()) SourceFile(
      *TheModule, SourceFileKind::Main, SourceMgr.addMemBufferCopy(inputBuf));
  SF->setImports({});
  TheModule->addFile(*SF);

  return SF;
}

MacroDecl *SyntacticMacroExpansionInstance::getSynthesizedMacroDecl(
    Identifier name, const MacroExpansionSpecifier &expansion) {
  auto &ctx = getASTContext();

  // Reuse cached MacroDecl of the same name if it's already created.
  MacroDecl *macro;
  auto found = MacroDecls.find(name);
  if (found != MacroDecls.end()) {
    macro = found->second;
  } else {
    macro = new (ctx) MacroDecl(
        /*macroLoc=*/{}, DeclName(name), /*nameLoc=*/{},
        /*genericParams=*/nullptr, /*parameterList=*/nullptr,
        /*arrowLoc=*/{}, /*resultType=*/nullptr,
        /*definition=*/nullptr, /*parent=*/TheModule);
    macro->setImplicit();
    MacroDecls.insert({name, macro});
  }

  // Add missing role attributes to MacroDecl.
  MacroRoles roles = expansion.macroRoles;
  for (auto attr : macro->getAttrs().getAttributes<MacroRoleAttr>()) {
    roles -= attr->getMacroRole();
  }
  for (MacroRole role : getAllMacroRoles()) {
    if (!roles.contains(role))
      continue;

    MacroSyntax syntax = getFreestandingMacroRoles().contains(role)
                             ? MacroSyntax::Freestanding
                             : MacroSyntax::Attached;

    auto *attr = MacroRoleAttr::create(ctx, /*atLoc=*/{}, /*range=*/{}, syntax,
                                       /*lParenLoc=*/{}, role, /*names=*/{},
                                       /*rParenLoc=*/{}, /*implicit=*/true);
    macro->getAttrs().add(attr);
  }

  // Set the macro definition.
  macro->setDefinition(expansion.macroDefinition);

  return macro;
}

/// Create a unique name of the expansion. The result is *appended* to \p out.
static void addExpansionDiscriminator(SmallString<32> &out,
                                      const SourceFile *SF, SourceLoc loc,
                                      MacroDecl *macro,
                                      Optional<MacroRole> role = None) {
  SourceManager &SM = SF->getASTContext().SourceMgr;
  auto lineColumn = SM.getLineAndColumnInBuffer(loc);

  StableHasher hasher = StableHasher::defaultHasher();

  // Macro name.
  hasher.combine(macro->getName().getBaseIdentifier().str());
  hasher.combine(uint8_t{0});

  // Module name.
  hasher.combine(SF->getParentModule()->getName().str());
  hasher.combine(uint8_t{0});

  // File base name.
  // Do not use the full path because we want this hash stable.
  hasher.combine(llvm::sys::path::filename(SF->getFilename()));
  hasher.combine(uint8_t{0});

  // Line/column
  hasher.combine(lineColumn.first);
  hasher.combine(lineColumn.second);

  // Macro role.
  if (role) {
    hasher.combine(*role);
  }

  Fingerprint hash(std::move(hasher));
  out.append(hash.getRawValue());
}

/// Perform expansion of the specified freestanding macro using the 'MacroDecl'.
static std::vector<unsigned>
expandFreestandingMacro(MacroDecl *macro,
                        FreestandingMacroExpansion *expansion) {
  std::vector<unsigned> bufferIDs;

  SmallString<32> discriminator;
  discriminator.append("macro_");
  addExpansionDiscriminator(discriminator,
                            expansion->getDeclContext()->getParentSourceFile(),
                            expansion->getPoundLoc(), macro);

  expansion->setMacroRef(macro);

  SourceFile *expandedSource =
      swift::evaluateFreestandingMacro(expansion, discriminator);
  if (expandedSource)
    bufferIDs.push_back(*expandedSource->getBufferID());

  return bufferIDs;
}

/// Perform expansion of the specified decl and the attribute using the
/// 'MacroDecl'. If the macro has multiple roles, evaluate it for all macro
/// roles.
static std::vector<unsigned>
expandAttachedMacro(MacroDecl *macro, CustomAttr *attr, Decl *attachedDecl) {

  std::vector<unsigned> bufferIDs;
  auto evaluate = [&](Decl *target, bool passParent, MacroRole role) {

    SmallString<32> discriminator;
    discriminator.append("macro_");
    addExpansionDiscriminator(discriminator,
                              target->getDeclContext()->getParentSourceFile(),
                              target->getLoc(), macro, role);

    SourceFile *expandedSource = swift::evaluateAttachedMacro(
        macro, target, attr, passParent, role, discriminator);
    if (expandedSource)
      bufferIDs.push_back(*expandedSource->getBufferID());
  };

  MacroRoles roles = macro->getMacroRoles();
  if (roles.contains(MacroRole::Accessor)) {
    if (isa<AbstractStorageDecl>(attachedDecl))
      evaluate(attachedDecl, /*passParent=*/false, MacroRole::Accessor);
  }
  if (roles.contains(MacroRole::MemberAttribute)) {
    if (auto *idc = dyn_cast<IterableDeclContext>(attachedDecl)) {
      for (auto *member : idc->getParsedMembers()) {
        // 'VarDecl' in 'IterableDeclContext' are part of 'PatternBindingDecl'.
        if (isa<VarDecl>(member))
          continue;
        evaluate(member, /*passParent=*/true, MacroRole::MemberAttribute);
      }
    }
  }
  if (roles.contains(MacroRole::Member)) {
    evaluate(attachedDecl, /*passParent=*/false, MacroRole::Member);
  }
  if (roles.contains(MacroRole::Peer)) {
    evaluate(attachedDecl, /*passParent=*/false, MacroRole::Peer);
  }
  if (roles.contains(MacroRole::Conformance)) {
    evaluate(attachedDecl, /*passParent=*/false, MacroRole::Conformance);
  }
  return bufferIDs;
}

/// Get the name of the custom attribute. This is used to create a dummy
/// MacroDecl.
static Identifier getCustomAttrName(ASTContext &ctx, const CustomAttr *attr) {
  TypeRepr *tyR = attr->getTypeRepr();
  if (auto ref = dyn_cast<DeclRefTypeRepr>(tyR)) {
    return ref->getNameRef().getBaseIdentifier();
  }

  // If the attribute is not an identifier type, create an identifier with its
  // textual representation.
  SmallString<32> name;
  llvm::raw_svector_ostream OS(name);
  tyR->print(OS);
  return ctx.getIdentifier(name);
}

namespace {

/// Find macro expansion i.e. '#foo' or '@foo' at the specified source location.
/// If a freestanding expansion (i.e. #foo) is found, the result 'ExpansionNode'
/// only has the node. If an attribute is found, the attribute and the attached
/// decl object is returned.
struct ExpansionNode {
  CustomAttr *attribute;
  ASTNode node;
};
class MacroExpansionFinder : public ASTWalker {
  SourceManager &SM;
  SourceLoc LocToResolve;
  llvm::Optional<ExpansionNode> Result;

  bool rangeContainsLocToResolve(SourceRange Range) const {
    return SM.rangeContainsTokenLoc(Range, LocToResolve);
  }

public:
  MacroExpansionFinder(SourceManager &SM, SourceLoc LocToResolve)
      : SM(SM), LocToResolve(LocToResolve) {}

  llvm::Optional<ExpansionNode> getResult() const { return Result; }

  MacroWalking getMacroWalkingBehavior() const override {
    return MacroWalking::None;
  }

  PreWalkAction walkToDeclPre(Decl *D) override {
    // Visit all 'VarDecl' because 'getSourceRangeIncludingAttrs()' doesn't
    // include attribute its ranges.
    if (!isa<VarDecl>(D) &&
        !rangeContainsLocToResolve(D->getSourceRangeIncludingAttrs())) {
      return Action::SkipChildren();
    }

    // Check the attributes.
    for (DeclAttribute *attr : D->getAttrs()) {
      if (auto customAttr = dyn_cast<CustomAttr>(attr)) {
        SourceRange nameRange(customAttr->getRangeWithAt().Start,
                              customAttr->getTypeExpr()->getEndLoc());
        if (rangeContainsLocToResolve(nameRange)) {
          Result = ExpansionNode{customAttr, ASTNode(D)};
          return Action::Stop();
        }
      }
    }

    // Check 'MacroExpansionDecl'.
    if (auto med = dyn_cast<MacroExpansionDecl>(D)) {
      SourceRange nameRange(med->getExpansionInfo()->SigilLoc,
                            med->getMacroNameLoc().getEndLoc());
      if (rangeContainsLocToResolve(nameRange)) {
        Result = ExpansionNode{nullptr, ASTNode(med)};
        return Action::Stop();
      }
    }

    return Action::Continue();
  }

  PreWalkResult<Expr *> walkToExprPre(Expr *E) override {
    if (!rangeContainsLocToResolve(E->getSourceRange())) {
      return Action::SkipChildren(E);
    }

    // Check 'MacroExpansionExpr'.
    if (auto mee = dyn_cast<MacroExpansionExpr>(E)) {
      SourceRange nameRange(mee->getExpansionInfo()->SigilLoc,
                            mee->getMacroNameLoc().getEndLoc());
      if (rangeContainsLocToResolve(nameRange)) {
        Result = ExpansionNode{nullptr, ASTNode(mee)};
        return Action::Stop();
      }
    }

    return Action::Continue(E);
  }

  PreWalkResult<Stmt *> walkToStmtPre(Stmt *S) override {
    if (!rangeContainsLocToResolve(S->getSourceRange())) {
      return Action::SkipChildren(S);
    }
    return Action::Continue(S);
  }
  PreWalkResult<ArgumentList *>
  walkToArgumentListPre(ArgumentList *AL) override {
    if (!rangeContainsLocToResolve(AL->getSourceRange())) {
      return Action::SkipChildren(AL);
    }
    return Action::Continue(AL);
  }
  PreWalkAction walkToParameterListPre(ParameterList *PL) override {
    if (!rangeContainsLocToResolve(PL->getSourceRange())) {
      return Action::SkipChildren();
    }
    return Action::Continue();
  }
  PreWalkAction walkToTypeReprPre(TypeRepr *T) override {
    // TypeRepr cannot have macro expansions in it.
    return Action::SkipChildren();
  }
};
} // namespace

bool SyntacticMacroExpansionInstance::getExpansion(
    SourceFile *SF, const MacroExpansionSpecifier &expansion,
    SourceEditConsumer &consumer) {

  // Find the expansion at 'expantion.offset'.
  MacroExpansionFinder expansionFinder(
      SourceMgr,
      SourceMgr.getLocForOffset(*SF->getBufferID(), expansion.offset));
  SF->walk(expansionFinder);
  auto expansionNode = expansionFinder.getResult();
  if (!expansionNode)
    return true;

  // Expand the macro.
  std::vector<unsigned> bufferIDs;
  if (auto *attr = expansionNode->attribute) {
    // Attached macros.
    MacroDecl *macro = getSynthesizedMacroDecl(
        getCustomAttrName(getASTContext(), attr), expansion);
    auto *attachedTo = expansionNode->node.get<Decl *>();
    bufferIDs = expandAttachedMacro(macro, attr, attachedTo);

    // For an attached macro, remove the custom attribute; it's been fully
    // subsumed by its expansions.
    SourceRange range = attr->getRangeWithAt();
    auto charRange = Lexer::getCharSourceRangeFromSourceRange(SourceMgr, range);
    consumer.remove(SourceMgr, charRange);
  } else {
    // Freestanding macros.
    FreestandingMacroExpansion *freestanding;
    auto node = expansionNode->node;
    if (node.is<Expr *>()) {
      freestanding = cast<MacroExpansionExpr>(node.get<Expr *>());
    } else {
      freestanding = cast<MacroExpansionDecl>(node.get<Decl *>());
    }

    MacroDecl *macro = getSynthesizedMacroDecl(
        freestanding->getMacroName().getBaseIdentifier(), expansion);
    bufferIDs = expandFreestandingMacro(macro, freestanding);
  }

  // Send all edits to the consumer.
  for (unsigned bufferID : bufferIDs) {
    consumer.acceptMacroExpansionBuffer(SourceMgr, bufferID, SF,
                                        /*adjust=*/false);
  }

  return false;
}

bool SyntacticMacroExpansionInstance::getExpansions(
    llvm::MemoryBuffer *inputBuf, ArrayRef<MacroExpansionSpecifier> expansions,
    SourceEditConsumer &consumer) {
  std::scoped_lock<std::mutex> lock(mtx);

  // Create a source file.
  SourceFile *SF = getSourceFile(inputBuf);

  bool hasError = false;
  for (const auto &expansion : expansions) {
    hasError |= getExpansion(SF, expansion, consumer);
  }
  return hasError;
}
