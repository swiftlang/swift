//===--- TypeCheckMacros.cpp -  Macro Handling ----------------------------===//
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
// This file implements support for the evaluation of macros.
//
//===----------------------------------------------------------------------===//

#include "TypeCheckMacros.h"
#include "TypeChecker.h"
#include "swift/ABI/MetadataValues.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/ASTMangler.h"
#include "swift/AST/Expr.h"
#include "swift/AST/NameLookupRequests.h"
#include "swift/AST/MacroDefinition.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Basic/StringExtras.h"
#include "swift/Demangling/Demangler.h"
#include "swift/Demangling/ManglingMacros.h"
#include "swift/Parse/Lexer.h"
#include "swift/Subsystems.h"
#include "llvm/Config/config.h"

#if defined(_WIN32)
#define WIN32_LEAN_AND_MEAN
#define NOMINMAX
#include <windows.h>
#else
#include <dlfcn.h>
#endif

using namespace swift;

extern "C" void *swift_ASTGen_resolveMacroType(const void *macroType);

extern "C" void swift_ASTGen_destroyMacro(void *macro);

extern "C" ptrdiff_t swift_ASTGen_expandFreestandingMacro(
    void *diagEngine, void *macro,
    const char *discriminator,
    ptrdiff_t discriminatorLength,
    void *sourceFile,
    const void *sourceLocation,
    const char **evaluatedSource, ptrdiff_t *evaluatedSourceLength);

extern "C" ptrdiff_t swift_ASTGen_expandAttachedMacro(
    void *diagEngine, void *macro,
    const char *discriminator,
    ptrdiff_t discriminatorLength,
    uint32_t rawMacroRole,
    void *customAttrSourceFile,
    const void *customAttrSourceLocation,
    void *declarationSourceFile,
    const void *declarationSourceLocation,
    void *parentDeclSourceFile,
    const void *parentDeclSourceLocation,
    const char **evaluatedSource,
    ptrdiff_t *evaluatedSourceLength
);

/// Produce the mangled name for the nominal type descriptor of a type
/// referenced by its module and type name.
static std::string mangledNameForTypeMetadataAccessor(
    StringRef moduleName, StringRef typeName, Node::Kind typeKind) {
  using namespace Demangle;

  //  kind=Global
  //    kind=NominalTypeDescriptor
  //      kind=Type
  //        kind=Structure|Enum|Class
  //          kind=Module, text=moduleName
  //          kind=Identifier, text=typeName
  Demangle::Demangler D;
  auto *global = D.createNode(Node::Kind::Global);
  {
    auto *nominalDescriptor =
        D.createNode(Node::Kind::TypeMetadataAccessFunction);
    {
      auto *type = D.createNode(Node::Kind::Type);
      {
        auto *module = D.createNode(Node::Kind::Module, moduleName);
        auto *identifier = D.createNode(Node::Kind::Identifier, typeName);
        auto *structNode = D.createNode(typeKind);
        structNode->addChild(module, D);
        structNode->addChild(identifier, D);
        type->addChild(structNode, D);
      }
      nominalDescriptor->addChild(type, D);
    }
    global->addChild(nominalDescriptor, D);
  }

  auto mangleResult = mangleNode(global);
  assert(mangleResult.isSuccess());
  return mangleResult.result();
}

#if SWIFT_SWIFT_PARSER
/// Look for macro's type metadata given its external module and type name.
static void const *lookupMacroTypeMetadataByExternalName(
    ASTContext &ctx, StringRef moduleName, StringRef typeName,
    void *libraryHint = nullptr
) {
  // Look up the type metadata accessor as a struct, enum, or class.
  const Demangle::Node::Kind typeKinds[] = {
    Demangle::Node::Kind::Structure,
    Demangle::Node::Kind::Enum,
    Demangle::Node::Kind::Class
  };

  void *accessorAddr = nullptr;
  for (auto typeKind : typeKinds) {
    auto symbolName = mangledNameForTypeMetadataAccessor(
        moduleName, typeName, typeKind);
    accessorAddr = ctx.getAddressOfSymbol(symbolName.c_str(), libraryHint);
    if (accessorAddr)
      break;
  }

  if (!accessorAddr)
    return nullptr;

  // Call the accessor to form type metadata.
  using MetadataAccessFunc = const void *(MetadataRequest);
  auto accessor = reinterpret_cast<MetadataAccessFunc*>(accessorAddr);
  return accessor(MetadataRequest(MetadataState::Complete));
}
#endif

/// Handle the "A.B" spelling of an external macro definition, from early
/// pitches of the expression-macros proposal, which is also used as the syntax
/// for builtin macro definitions.
///
/// \returns The macro definition if the pattern is recognized, or \c None
/// otherwise.
static Optional<MacroDefinition> handleOldStyleOrBuiltinMacroDefinition(
    ASTContext &ctx, Expr *expr
) {
  auto memberExpr = dyn_cast<UnresolvedDotExpr>(expr);
  if (!memberExpr)
    return None;

  Expr *base = memberExpr->getBase();
  auto baseDeclRef = dyn_cast<UnresolvedDeclRefExpr>(base);
  if (!baseDeclRef)
    return None;

  if (memberExpr->getName().isSpecial() ||
      baseDeclRef->getName().isSpecial())
    return None;

  Identifier moduleName = baseDeclRef->getName().getBaseIdentifier();
  Identifier typeName = memberExpr->getName().getBaseIdentifier();

  // If this is a reference to the builtin module, check if it's a known
  // builtin macro.
  if (moduleName.str().equals("Builtin") && ctx.SILOpts.ParseStdlib) {
    if (typeName.str() == "ExternalMacro") {
      return MacroDefinition::forBuiltin(BuiltinMacroKind::ExternalMacro);
    }

    ctx.Diags.diagnose(
        memberExpr->getLoc(), diag::macro_definition_unknown_builtin,
        typeName
    );

    return MacroDefinition::forInvalid();
  }

  std::string newCode;
  {
    llvm::raw_string_ostream out(newCode);
    out << "#externalMacro(module: \"" << moduleName << "\", type: \""
        << typeName << "\")";
  }

  ctx.Diags.diagnose(
      memberExpr->getLoc(), diag::macro_definition_old_style
  ).fixItReplace(memberExpr->getSourceRange(), newCode);

  return MacroDefinition::forExternal(moduleName, typeName);
}

/// Translate an argument provided as a string literal into an identifier,
/// or return \c None and emit an error if it cannot be done.
Optional<Identifier> getIdentifierFromStringLiteralArgument(
    ASTContext &ctx, MacroExpansionExpr *expansion, unsigned index) {
  auto argList = expansion->getArgs();

  // If there's no argument here, an error was diagnosed elsewhere.
  if (!argList || index >= argList->size()) {
    return None;
  }

  auto arg = argList->getExpr(index);
  auto stringLiteral = dyn_cast<StringLiteralExpr>(arg);
  if (!stringLiteral) {
    ctx.Diags.diagnose(
        arg->getLoc(), diag::external_macro_arg_not_type_name, index
    );

    return None;
  }


  auto contents = stringLiteral->getValue();
  if (!Lexer::isIdentifier(contents)) {
    ctx.Diags.diagnose(
        arg->getLoc(), diag::external_macro_arg_not_type_name, index
    );

    return None;
  }

  return ctx.getIdentifier(contents);
}

MacroDefinition MacroDefinitionRequest::evaluate(
    Evaluator &evaluator, MacroDecl *macro
) const {
  ASTContext &ctx = macro->getASTContext();

  // If no definition was provided, the macro is... undefined, of course.
  auto definition = macro->definition;
  if (!definition)
    return MacroDefinition::forUndefined();

  // Recognize the "A.B" spelling of external macro definitions from early
  // pitches of the feature. This spelling is also used for builtin macro
  // definitions.
  if (auto oldStyleOrBuiltinResult =
          handleOldStyleOrBuiltinMacroDefinition(ctx, definition)) {
    return *oldStyleOrBuiltinResult;
  }

  // At this point, we must have a macro expansion expression.
  auto expansion = dyn_cast<MacroExpansionExpr>(definition);
  if (!expansion) {
    ctx.Diags.diagnose(
        definition->getLoc(), diag::macro_definition_not_expansion
    );

    return MacroDefinition::forInvalid();
  }

  // Type-check the macro expansion.
  Type resultType = macro->mapTypeIntoContext(macro->getResultInterfaceType());

  constraints::ContextualTypeInfo contextualType {
    TypeLoc::withoutLoc(resultType),
    // FIXME: Add a contextual type purpose for macro definition checking.
    ContextualTypePurpose::CTP_CoerceOperand
  };

  PrettyStackTraceDecl debugStack("type checking macro definition", macro);
  Type typeCheckedType = TypeChecker::typeCheckExpression(
      definition, macro, contextualType,
      TypeCheckExprFlags::DisableMacroExpansions);
  if (!typeCheckedType)
    return MacroDefinition::forInvalid();

  // Dig out the macro that was expanded.
  auto expandedMacro =
      dyn_cast_or_null<MacroDecl>(expansion->getMacroRef().getDecl());
  if (!expandedMacro)
    return MacroDefinition::forInvalid();

  // FIXME: Only external macros are supported at this point.
  auto builtinKind = expandedMacro->getBuiltinKind();
  if (builtinKind != BuiltinMacroKind::ExternalMacro) {
    ctx.Diags.diagnose(
        definition->getLoc(), diag::macro_definition_unsupported
    );

    return MacroDefinition::forInvalid();
  }

  // Dig out the module and type name.
  auto moduleName = getIdentifierFromStringLiteralArgument(ctx, expansion, 0);
  if (!moduleName) {
    return MacroDefinition::forInvalid();
  }

  auto typeName = getIdentifierFromStringLiteralArgument(ctx, expansion, 1);
  if (!typeName) {
    return MacroDefinition::forInvalid();
  }

  return MacroDefinition::forExternal(*moduleName, *typeName);
}

/// Load a plugin library based on a module name.
static void *loadPluginByName(StringRef searchPath, StringRef moduleName, llvm::vfs::FileSystem &fs) {
  SmallString<128> fullPath(searchPath);
  llvm::sys::path::append(fullPath, "lib" + moduleName + LTDL_SHLIB_EXT);
  if (fs.getRealPath(fullPath, fullPath))
    return nullptr;

#if defined(_WIN32)
  return LoadLibraryA(fullPath.c_str());
#else
  return dlopen(fullPath.c_str(), RTLD_LAZY);
#endif
}

void *CompilerPluginLoadRequest::evaluate(
    Evaluator &evaluator, ASTContext *ctx, Identifier moduleName
) const {
  auto fs = ctx->SourceMgr.getFileSystem();
  auto &searchPathOpts = ctx->SearchPathOpts;
  for (const auto &path : searchPathOpts.PluginSearchPaths) {
    if (auto found = loadPluginByName(path, moduleName.str(), *fs))
      return found;
  }

  return nullptr;
}

static Optional<ExternalMacroDefinition>
resolveInProcessMacro(
    ASTContext &ctx, Identifier moduleName, Identifier typeName,
    void *libraryHint = nullptr
) {
#if SWIFT_SWIFT_PARSER
  /// Look for the type metadata given the external module and type names.
  auto macroMetatype = lookupMacroTypeMetadataByExternalName(
      ctx, moduleName.str(), typeName.str(), libraryHint);
  if (macroMetatype) {
    // Check whether the macro metatype is in-process.
    if (auto inProcess = swift_ASTGen_resolveMacroType(macroMetatype)) {
      // Make sure we clean up after the macro.
      ctx.addCleanup([inProcess]() {
        swift_ASTGen_destroyMacro(inProcess);
      });

      return ExternalMacroDefinition{inProcess};
    }
  }
#endif

  return None;
}

ExternalMacroDefinition
ExternalMacroDefinitionRequest::evaluate(
    Evaluator &evaluator, ASTContext *ctx,
    Identifier moduleName, Identifier typeName
) const {
  // Try to load a plugin module from the plugin search paths. If it
  // succeeds, resolve in-process from that plugin
  CompilerPluginLoadRequest loadRequest{ctx, moduleName};
  if (auto loadedLibrary = evaluateOrDefault(
          evaluator, loadRequest, nullptr)) {
    if (auto inProcess = resolveInProcessMacro(
            *ctx, moduleName, typeName, loadedLibrary))
      return *inProcess;
  }

  // Try to resolve in-process.
  if (auto inProcess = resolveInProcessMacro(*ctx, moduleName, typeName))
    return *inProcess;

  return ExternalMacroDefinition{nullptr};
}

/// Adjust the given mangled name for a macro expansion to produce a valid
/// buffer name.
static std::string adjustMacroExpansionBufferName(StringRef name) {
  std::string result;
  if (name.startswith(MANGLING_PREFIX_STR)) {
    result += MACRO_EXPANSION_BUFFER_MANGLING_PREFIX;
    name = name.drop_front(StringRef(MANGLING_PREFIX_STR).size());
  }

  result += name;
  result += ".swift";
  return result;
}

ArrayRef<unsigned> ExpandMemberAttributeMacros::evaluate(Evaluator &evaluator,
                                                         Decl *decl) const {
  if (decl->isImplicit())
    return { };

  auto *parentDecl = decl->getDeclContext()->getAsDecl();
  if (!parentDecl)
    return { };

  if (isa<PatternBindingDecl>(decl))
    return { };

  SmallVector<unsigned, 2> bufferIDs;
  parentDecl->forEachAttachedMacro(MacroRole::MemberAttribute,
      [&](CustomAttr *attr, MacroDecl *macro) {
        if (auto bufferID = expandAttributes(attr, macro, decl))
          bufferIDs.push_back(*bufferID);
      });

  return parentDecl->getASTContext().AllocateCopy(bufferIDs);
}

ArrayRef<unsigned> ExpandSynthesizedMemberMacroRequest::evaluate(
    Evaluator &evaluator, Decl *decl
) const {
  SmallVector<unsigned, 2> bufferIDs;
  decl->forEachAttachedMacro(MacroRole::Member,
      [&](CustomAttr *attr, MacroDecl *macro) {
        if (auto bufferID = expandMembers(attr, macro, decl))
          bufferIDs.push_back(*bufferID);
      });

  return decl->getASTContext().AllocateCopy(bufferIDs);
}

ArrayRef<unsigned>
ExpandPeerMacroRequest::evaluate(Evaluator &evaluator, Decl *decl) const {
  SmallVector<unsigned, 2> bufferIDs;
  decl->forEachAttachedMacro(MacroRole::Peer,
      [&](CustomAttr *attr, MacroDecl *macro) {
        if (auto bufferID = expandPeers(attr, macro, decl))
          bufferIDs.push_back(*bufferID);
      });

  return decl->getASTContext().AllocateCopy(bufferIDs);
}

/// Determine whether the given source file is from an expansion of the given
/// macro.
static bool isFromExpansionOfMacro(SourceFile *sourceFile, MacroDecl *macro,
                                   MacroRole role) {
  while (sourceFile) {
    auto expansion = sourceFile->getMacroExpansion();
    if (!expansion)
      return false;

    if (auto expansionExpr = dyn_cast_or_null<MacroExpansionExpr>(
            expansion.dyn_cast<Expr *>())) {
      if (expansionExpr->getMacroRef().getDecl() == macro)
        return true;
    } else if (auto expansionDecl = dyn_cast_or_null<MacroExpansionDecl>(
            expansion.dyn_cast<Decl *>())) {
      // FIXME: Update once MacroExpansionDecl has a proper macro reference
      // in it.
      if (expansionDecl->getMacroName().getFullName() == macro->getName())
        return true;
    } else if (auto *macroAttr = sourceFile->getAttachedMacroAttribute()) {
      auto *decl = expansion.dyn_cast<Decl *>();
      auto *macroDecl = decl->getResolvedMacro(macroAttr);
      if (!macroDecl)
        return false;

      return macroDecl == macro &&
             sourceFile->getFulfilledMacroRole() == role;
    } else {
      llvm_unreachable("Unknown macro expansion node kind");
    }

    sourceFile = sourceFile->getEnclosingSourceFile();
  }

  return false;
}

Expr *swift::expandMacroExpr(
    DeclContext *dc, Expr *expr, ConcreteDeclRef macroRef, Type expandedType
) {
  ASTContext &ctx = dc->getASTContext();
  SourceManager &sourceMgr = ctx.SourceMgr;

  auto moduleDecl = dc->getParentModule();
  auto sourceFile = moduleDecl->getSourceFileContainingLocation(expr->getLoc());
  if (!sourceFile)
    return nullptr;

  // Evaluate the macro.
  NullTerminatedStringRef evaluatedSource;

  MacroDecl *macro = cast<MacroDecl>(macroRef.getDecl());

  if (isFromExpansionOfMacro(sourceFile, macro, MacroRole::Expression)) {
    ctx.Diags.diagnose(expr->getLoc(), diag::macro_recursive, macro->getName());
    return nullptr;
  }

  std::string discriminator;
  auto macroDef = macro->getDefinition();
  switch (macroDef.kind) {
  case MacroDefinition::Kind::Undefined:
  case MacroDefinition::Kind::Invalid:
    // Already diagnosed as an error elsewhere.
    return nullptr;

  case MacroDefinition::Kind::Builtin: {
    switch (macroDef.getBuiltinKind()) {
    case BuiltinMacroKind::ExternalMacro:
        ctx.Diags.diagnose(
            expr->getLoc(), diag::external_macro_outside_macro_definition);
      return nullptr;
    }
  }

  case MacroDefinition::Kind::External: {
    // Retrieve the external definition of the macro.
    auto external = macroDef.getExternalMacro();
    ExternalMacroDefinitionRequest request{
      &ctx, external.moduleName, external.macroTypeName
    };
    auto externalDef = evaluateOrDefault(
        ctx.evaluator, request, ExternalMacroDefinition()
    );
    if (!externalDef.opaqueHandle) {
      ctx.Diags.diagnose(
         expr->getLoc(), diag::external_macro_not_found,
         external.moduleName.str(),
         external.macroTypeName.str(),
         macro->getName()
      );
      macro->diagnose(diag::decl_declared_here, macro->getName());
      return nullptr;
    }

    // Make sure macros are enabled before we expand.
    if (!ctx.LangOpts.hasFeature(Feature::Macros)) {
      ctx.Diags.diagnose(expr->getLoc(), diag::macro_experimental);
      return nullptr;
    }

#if SWIFT_SWIFT_PARSER
    PrettyStackTraceExpr debugStack(ctx, "expanding macro", expr);

    // Builtin macros are handled via ASTGen.
    auto astGenSourceFile = sourceFile->exportedSourceFile;
    if (!astGenSourceFile)
      return nullptr;

    if (auto expansionExpr = dyn_cast<MacroExpansionExpr>(expr)) {
      Mangle::ASTMangler mangler;
      discriminator = mangler.mangleMacroExpansion(expansionExpr);
    }

    const char *evaluatedSourceAddress;
    ptrdiff_t evaluatedSourceLength;
    swift_ASTGen_expandFreestandingMacro(
        &ctx.Diags,
        externalDef.opaqueHandle,
        discriminator.data(), discriminator.size(),
        astGenSourceFile, expr->getStartLoc().getOpaquePointerValue(),
        &evaluatedSourceAddress, &evaluatedSourceLength);
    if (!evaluatedSourceAddress)
      return nullptr;
    evaluatedSource = NullTerminatedStringRef(evaluatedSourceAddress,
                                              (size_t)evaluatedSourceLength);
    break;
#else
    ctx.Diags.diagnose(expr->getLoc(), diag::macro_unsupported);
    return nullptr;
#endif
  }
  }

  // Figure out a reasonable name for the macro expansion buffer.
  std::string bufferName;
  if (discriminator.empty())
    bufferName = "macro-expansion";
  else {
    bufferName = adjustMacroExpansionBufferName(discriminator);
  }

  // Dump macro expansions to standard output, if requested.
  if (ctx.LangOpts.DumpMacroExpansions) {
    llvm::errs() << bufferName << " as " << expandedType.getString()
      << "\n------------------------------\n"
      << evaluatedSource
      << "\n------------------------------\n";
  }

  // Create a new source buffer with the contents of the expanded macro.
  auto macroBuffer =
      llvm::MemoryBuffer::getMemBufferCopy(evaluatedSource, bufferName);
  unsigned macroBufferID = sourceMgr.addNewSourceBuffer(std::move(macroBuffer));
  auto macroBufferRange = sourceMgr.getRangeForBuffer(macroBufferID);
  GeneratedSourceInfo sourceInfo{
    GeneratedSourceInfo::ExpressionMacroExpansion,
    Lexer::getCharSourceRangeFromSourceRange(
      sourceMgr, expr->getSourceRange()),
    macroBufferRange,
    ASTNode(expr).getOpaqueValue(),
    dc
  };
  sourceMgr.setGeneratedSourceInfo(macroBufferID, sourceInfo);
  free((void*)evaluatedSource.data());

  // Create a source file to hold the macro buffer. This is automatically
  // registered with the enclosing module.
  auto macroSourceFile = new (ctx) SourceFile(
      *dc->getParentModule(), SourceFileKind::MacroExpansion, macroBufferID,
      /*parsingOpts=*/{}, /*isPrimary=*/false);
  macroSourceFile->setImports(sourceFile->getImports());

  // Retrieve the parsed expression from the list of top-level items.
  auto topLevelItems = macroSourceFile->getTopLevelItems();
  Expr *expandedExpr = nullptr;
  if (topLevelItems.size() == 1) {
    expandedExpr = topLevelItems.front().dyn_cast<Expr *>();
  }

  if (!expandedExpr) {
    ctx.Diags.diagnose(
        macroBufferRange.getStart(), diag::expected_macro_expansion_expr);
    return nullptr;
  }

  // Type-check the expanded expression.
  // FIXME: Would like to pass through type checking options like "discarded"
  // that are captured by TypeCheckExprOptions.
  constraints::ContextualTypeInfo contextualType {
    TypeLoc::withoutLoc(expandedType),
    // FIXME: Add a contextual type purpose for macro expansion.
    ContextualTypePurpose::CTP_CoerceOperand
  };

  PrettyStackTraceExpr debugStack(
      ctx, "type checking expanded macro", expandedExpr);
  Type realExpandedType = TypeChecker::typeCheckExpression(
      expandedExpr, dc, contextualType);
  if (!realExpandedType)
    return nullptr;

  assert((expandedType->isEqual(realExpandedType) ||
          realExpandedType->hasError()) &&
         "Type checking changed the result type?");
  return expandedExpr;
}

/// Expands the given macro expansion declaration and type-check the result.
bool swift::expandFreestandingDeclarationMacro(
    MacroExpansionDecl *med, SmallVectorImpl<Decl *> &results) {
  auto *dc = med->getDeclContext();
  ASTContext &ctx = dc->getASTContext();
  SourceManager &sourceMgr = ctx.SourceMgr;

  auto moduleDecl = dc->getParentModule();
  auto sourceFile = moduleDecl->getSourceFileContainingLocation(med->getLoc());
  if (!sourceFile)
    return false;

  // Evaluate the macro.
  NullTerminatedStringRef evaluatedSource;

  MacroDecl *macro = cast<MacroDecl>(med->getMacroRef().getDecl());
  assert(macro->getMacroRoles()
             .contains(MacroRole::Declaration));

  if (isFromExpansionOfMacro(sourceFile, macro, MacroRole::Declaration)) {
    med->diagnose(diag::macro_recursive, macro->getName());
    return false;
  }

  auto macroDef = macro->getDefinition();
  switch (macroDef.kind) {
  case MacroDefinition::Kind::Undefined:
  case MacroDefinition::Kind::Invalid:
    // Already diagnosed as an error elsewhere.
    return false;

  case MacroDefinition::Kind::Builtin: {
    switch (macroDef.getBuiltinKind()) {
    case BuiltinMacroKind::ExternalMacro:
      // FIXME: Error here.
      return false;
    }
  }

  case MacroDefinition::Kind::External: {
    // Retrieve the external definition of the macro.
    auto external = macroDef.getExternalMacro();
    ExternalMacroDefinitionRequest request{
        &ctx, external.moduleName, external.macroTypeName
    };
    auto externalDef = evaluateOrDefault(
        ctx.evaluator, request, ExternalMacroDefinition()
    );
    if (!externalDef.opaqueHandle) {
      med->diagnose(diag::external_macro_not_found,
                    external.moduleName.str(),
                    external.macroTypeName.str(),
                    macro->getName()
      );
      macro->diagnose(diag::decl_declared_here, macro->getName());
      return false;
    }

    // Make sure macros are enabled before we expand.
    if (!ctx.LangOpts.hasFeature(Feature::Macros)) {
      med->diagnose(diag::macro_experimental);
      return false;
    }

#if SWIFT_SWIFT_PARSER
    PrettyStackTraceDecl debugStack("expanding declaration macro", med);

    // Builtin macros are handled via ASTGen.
    auto astGenSourceFile = sourceFile->exportedSourceFile;
    if (!astGenSourceFile)
      return false;

    Mangle::ASTMangler mangler;
    auto discriminator = mangler.mangleMacroExpansion(med);

    const char *evaluatedSourceAddress;
    ptrdiff_t evaluatedSourceLength;
    swift_ASTGen_expandFreestandingMacro(
        &ctx.Diags,
        externalDef.opaqueHandle,
        discriminator.data(), discriminator.size(),
        astGenSourceFile, med->getStartLoc().getOpaquePointerValue(),
        &evaluatedSourceAddress, &evaluatedSourceLength);
    if (!evaluatedSourceAddress)
      return false;
    evaluatedSource = NullTerminatedStringRef(evaluatedSourceAddress,
                                              (size_t)evaluatedSourceLength);
    break;
#else
    med->diagnose(diag::macro_unsupported);
    return false;
#endif
  }
  }

  // Figure out a reasonable name for the macro expansion buffer.
  std::string bufferName;
  {
    Mangle::ASTMangler mangler;
    bufferName = adjustMacroExpansionBufferName(
        mangler.mangleMacroExpansion(med));
  }

  // Dump macro expansions to standard output, if requested.
  if (ctx.LangOpts.DumpMacroExpansions) {
    llvm::errs() << bufferName
                 << "\n------------------------------\n"
                 << evaluatedSource
                 << "\n------------------------------\n";
  }

  // Create a new source buffer with the contents of the expanded macro.
  auto macroBuffer =
      llvm::MemoryBuffer::getMemBufferCopy(evaluatedSource, bufferName);
  unsigned macroBufferID = sourceMgr.addNewSourceBuffer(std::move(macroBuffer));
  auto macroBufferRange = sourceMgr.getRangeForBuffer(macroBufferID);
  GeneratedSourceInfo sourceInfo{
      GeneratedSourceInfo::FreestandingDeclMacroExpansion,
      Lexer::getCharSourceRangeFromSourceRange(
        sourceMgr, med->getSourceRange()),
      macroBufferRange,
      ASTNode(med).getOpaqueValue(),
      dc
  };
  sourceMgr.setGeneratedSourceInfo(macroBufferID, sourceInfo);
  free((void*)evaluatedSource.data());

  // Create a source file to hold the macro buffer. This is automatically
  // registered with the enclosing module.
  auto macroSourceFile = new (ctx) SourceFile(
      *dc->getParentModule(), SourceFileKind::MacroExpansion, macroBufferID,
      /*parsingOpts=*/{}, /*isPrimary=*/false);
  macroSourceFile->setImports(sourceFile->getImports());

  PrettyStackTraceDecl debugStack(
      "type checking expanded declaration macro", med);

  // Retrieve the parsed declarations from the list of top-level items.
  auto topLevelItems = macroSourceFile->getTopLevelItems();
  for (auto item : topLevelItems) {
    auto *decl = item.dyn_cast<Decl *>();
    if (!decl) {
      ctx.Diags.diagnose(
          macroBufferRange.getStart(), diag::expected_macro_expansion_decls);
      return false;
    }
    decl->setDeclContext(dc);
    results.push_back(decl);
  }
  return true;
}

// If this storage declaration is a variable with an explicit initializer,
// return the range from the `=` to the end of the explicit initializer.
static Optional<SourceRange> getExplicitInitializerRange(
    AbstractStorageDecl *storage) {
  auto var = dyn_cast<VarDecl>(storage);
  if (!var)
    return None;

  auto pattern = var->getParentPatternBinding();
  if (!pattern)
    return None;

  unsigned index = pattern->getPatternEntryIndexForVarDecl(var);
  SourceLoc equalLoc = pattern->getEqualLoc(index);
  SourceRange initRange = pattern->getOriginalInitRange(index);
  if (equalLoc.isInvalid() || initRange.End.isInvalid())
    return None;

  return SourceRange(equalLoc, initRange.End);
}

static SourceFile *
evaluateAttachedMacro(MacroDecl *macro, Decl *attachedTo, CustomAttr *attr,
                      bool passParentContext, MacroRole role) {
  DeclContext *dc;
  if (role == MacroRole::Peer) {
    dc = attachedTo->getDeclContext();
  } else {
    dc = attachedTo->getInnermostDeclContext();
  }

  ASTContext &ctx = dc->getASTContext();
  SourceManager &sourceMgr = ctx.SourceMgr;

  auto moduleDecl = dc->getParentModule();

  auto attrSourceFile =
    moduleDecl->getSourceFileContainingLocation(attr->AtLoc);
  if (!attrSourceFile)
    return nullptr;

  auto declSourceFile =
      moduleDecl->getSourceFileContainingLocation(attachedTo->getStartLoc());
  if (!declSourceFile)
    return nullptr;

  Decl *parentDecl = nullptr;
  SourceFile *parentDeclSourceFile = nullptr;
  if (passParentContext) {
    parentDecl = attachedTo->getDeclContext()->getAsDecl();
    if (!parentDecl)
      return nullptr;

    parentDeclSourceFile =
      moduleDecl->getSourceFileContainingLocation(parentDecl->getLoc());
    if (!parentDeclSourceFile)
      return nullptr;
  }

  if (isFromExpansionOfMacro(attrSourceFile, macro, role) ||
      isFromExpansionOfMacro(declSourceFile, macro, role) ||
      isFromExpansionOfMacro(parentDeclSourceFile, macro, role)) {
    attachedTo->diagnose(diag::macro_recursive, macro->getName());
    return nullptr;
  }

  // Evaluate the macro.
  NullTerminatedStringRef evaluatedSource;

  std::string discriminator;
  auto macroDef = macro->getDefinition();
  switch (macroDef.kind) {
  case MacroDefinition::Kind::Undefined:
  case MacroDefinition::Kind::Invalid:
    // Already diagnosed as an error elsewhere.
    return nullptr;

  case MacroDefinition::Kind::Builtin: {
    switch (macroDef.getBuiltinKind()) {
    case BuiltinMacroKind::ExternalMacro:
      // FIXME: Error here.
      return nullptr;
    }
  }

  case MacroDefinition::Kind::External: {
    // Retrieve the external definition of the macro.
    auto external = macroDef.getExternalMacro();
    ExternalMacroDefinitionRequest request{
        &ctx, external.moduleName, external.macroTypeName
    };
    auto externalDef = evaluateOrDefault(
        ctx.evaluator, request, ExternalMacroDefinition()
    );
    if (!externalDef.opaqueHandle) {
      attachedTo->diagnose(diag::external_macro_not_found,
                        external.moduleName.str(),
                        external.macroTypeName.str(),
                        macro->getName()
      );
      macro->diagnose(diag::decl_declared_here, macro->getName());
      return nullptr;
    }

    // Make sure macros are enabled before we expand.
    if (!ctx.LangOpts.hasFeature(Feature::Macros)) {
      attachedTo->diagnose(diag::macro_experimental);
      return nullptr;
    }

#if SWIFT_SWIFT_PARSER
    PrettyStackTraceDecl debugStack("expanding attached macro", attachedTo);

    auto astGenAttrSourceFile = attrSourceFile->exportedSourceFile;
    if (!astGenAttrSourceFile)
      return nullptr;

    auto astGenDeclSourceFile = declSourceFile->exportedSourceFile;
    if (!astGenDeclSourceFile)
      return nullptr;

    void *astGenParentDeclSourceFile = nullptr;
    const void *parentDeclLoc = nullptr;
    if (passParentContext) {
      astGenParentDeclSourceFile = parentDeclSourceFile->exportedSourceFile;
      if (!astGenParentDeclSourceFile)
        return nullptr;

      parentDeclLoc = parentDecl->getStartLoc().getOpaquePointerValue();
    }

    Decl *searchDecl = attachedTo;
    if (auto var = dyn_cast<VarDecl>(attachedTo))
      searchDecl = var->getParentPatternBinding();

    {
      Mangle::ASTMangler mangler;
      discriminator =
        mangler.mangleAttachedMacroExpansion(attachedTo, attr, role);
    }

    const char *evaluatedSourceAddress;
    ptrdiff_t evaluatedSourceLength;
    swift_ASTGen_expandAttachedMacro(
        &ctx.Diags,
        externalDef.opaqueHandle,
        discriminator.data(), discriminator.size(),
        static_cast<uint32_t>(role),
        astGenAttrSourceFile, attr->AtLoc.getOpaquePointerValue(),
        astGenDeclSourceFile, searchDecl->getStartLoc().getOpaquePointerValue(),
        astGenParentDeclSourceFile, parentDeclLoc,
        &evaluatedSourceAddress, &evaluatedSourceLength);
    if (!evaluatedSourceAddress)
      return nullptr;
    evaluatedSource = NullTerminatedStringRef(evaluatedSourceAddress,
                                              (size_t)evaluatedSourceLength);
    break;
#else
    attachedTo->diagnose(diag::macro_unsupported);
    return nullptr;
#endif
  }
  }

  // Figure out a reasonable name for the macro expansion buffer.
  std::string bufferName = adjustMacroExpansionBufferName(discriminator);

  // Dump macro expansions to standard output, if requested.
  if (ctx.LangOpts.DumpMacroExpansions) {
    llvm::errs() << bufferName
                 << "\n------------------------------\n"
                 << evaluatedSource
                 << "\n------------------------------\n";
  }

  CharSourceRange generatedOriginalSourceRange;
  GeneratedSourceInfo::Kind generatedSourceKind;
  switch (role) {
  case MacroRole::Accessor: {
    generatedSourceKind = GeneratedSourceInfo::AccessorMacroExpansion;

    // Compute the location where the accessors will be added.
    auto storage = cast<AbstractStorageDecl>(attachedTo);
    auto bracesRange = storage->getBracesRange();
    if (bracesRange.Start.isValid()) {
      // We have braces already, so insert them inside the leading '{'.
      generatedOriginalSourceRange = CharSourceRange(
         Lexer::getLocForEndOfToken(sourceMgr, bracesRange.Start), 0);
    } else if (auto initRange = getExplicitInitializerRange(storage)) {
      // The accessor had an initializer, so the initializer (including
      // the `=`) is replaced by the accessors.
      generatedOriginalSourceRange =
          Lexer::getCharSourceRangeFromSourceRange(sourceMgr, *initRange);
    } else {
      // The accessors go at the end.
      SourceLoc endLoc = storage->getEndLoc();
      if (auto var = dyn_cast<VarDecl>(storage)) {
        if (auto pattern = var->getParentPattern())
          endLoc = pattern->getEndLoc();
      }

      generatedOriginalSourceRange = CharSourceRange(
         Lexer::getLocForEndOfToken(sourceMgr, endLoc), 0);
    }

    break;
  }

  case MacroRole::MemberAttribute: {
    generatedSourceKind = GeneratedSourceInfo::MemberAttributeMacroExpansion;
    SourceLoc startLoc;
    if (auto valueDecl = dyn_cast<ValueDecl>(attachedTo))
      startLoc = valueDecl->getAttributeInsertionLoc(/*forModifier=*/false);
    else
      startLoc = attachedTo->getStartLoc();

    generatedOriginalSourceRange = CharSourceRange(startLoc, 0);
    break;
  }

  case MacroRole::Member: {
    generatedSourceKind = GeneratedSourceInfo::MemberMacroExpansion;

    // Semantically, we insert members right before the closing brace.
    SourceLoc rightBraceLoc;
    if (auto nominal = dyn_cast<NominalTypeDecl>(attachedTo)) {
      rightBraceLoc = nominal->getBraces().End;
    } else {
      auto ext = cast<ExtensionDecl>(parentDecl);
      rightBraceLoc = ext->getBraces().End;
    }

    generatedOriginalSourceRange = CharSourceRange(rightBraceLoc, 0);
    break;
  }

  case MacroRole::Peer: {
    generatedSourceKind = GeneratedSourceInfo::PeerMacroExpansion;
    SourceLoc afterDeclLoc =
        Lexer::getLocForEndOfToken(sourceMgr, attachedTo->getEndLoc());
    generatedOriginalSourceRange = CharSourceRange(afterDeclLoc, 0);
    break;
  }

  case MacroRole::Expression:
  case MacroRole::Declaration:
    llvm_unreachable("freestanding macro in attached macro evaluation");
  }

  // Create a new source buffer with the contents of the expanded macro.
  auto macroBuffer =
      llvm::MemoryBuffer::getMemBufferCopy(evaluatedSource, bufferName);
  unsigned macroBufferID = sourceMgr.addNewSourceBuffer(std::move(macroBuffer));
  auto macroBufferRange = sourceMgr.getRangeForBuffer(macroBufferID);
  GeneratedSourceInfo sourceInfo{
      generatedSourceKind,
      generatedOriginalSourceRange,
      macroBufferRange,
      ASTNode(attachedTo).getOpaqueValue(),
      dc,
      attr
  };
  sourceMgr.setGeneratedSourceInfo(macroBufferID, sourceInfo);
  free((void*)evaluatedSource.data());

  // Create a source file to hold the macro buffer. This is automatically
  // registered with the enclosing module.
  auto macroSourceFile = new (ctx) SourceFile(
      *dc->getParentModule(), SourceFileKind::MacroExpansion, macroBufferID,
      /*parsingOpts=*/{}, /*isPrimary=*/false);
  macroSourceFile->setImports(declSourceFile->getImports());

  return macroSourceFile;
}

Optional<unsigned> swift::expandAccessors(
    AbstractStorageDecl *storage, CustomAttr *attr, MacroDecl *macro
) {
  // Evaluate the macro.
  auto macroSourceFile = evaluateAttachedMacro(macro, storage, attr,
                                               /*passParentContext*/false,
                                               MacroRole::Accessor);
  if (!macroSourceFile)
    return None;

  PrettyStackTraceDecl debugStack(
      "type checking expanded declaration macro", storage);

  // Trigger parsing of the sequence of accessor declarations. This has the
  // side effect of registering those accessor declarations with the storage
  // declaration, so there is nothing further to do.
  for (auto decl : macroSourceFile->getTopLevelItems()) {
    auto accessor = dyn_cast_or_null<AccessorDecl>(decl.dyn_cast<Decl *>());
    if (!accessor)
      continue;

    if (accessor->isObservingAccessor())
      continue;

    // If any non-observing accessor was added, remove the initializer if
    // there is one.
    if (auto var = dyn_cast<VarDecl>(storage)) {
      if (auto binding = var->getParentPatternBinding()) {
        unsigned index = binding->getPatternEntryIndexForVarDecl(var);
        binding->setInit(index, nullptr);
        break;
      }
    }
  }

  return macroSourceFile->getBufferID();
}

ArrayRef<unsigned> ExpandAccessorMacros::evaluate(
    Evaluator &evaluator, AbstractStorageDecl *storage
) const {
  llvm::SmallVector<unsigned, 1> bufferIDs;
  storage->forEachAttachedMacro(MacroRole::Accessor,
      [&](CustomAttr *customAttr, MacroDecl *macro) {
        if (auto bufferID = expandAccessors(
                storage, customAttr, macro))
          bufferIDs.push_back(*bufferID);
      });

  return storage->getASTContext().AllocateCopy(bufferIDs);
}

Optional<unsigned>
swift::expandAttributes(CustomAttr *attr, MacroDecl *macro, Decl *member) {
  // Evaluate the macro.
  auto macroSourceFile = evaluateAttachedMacro(macro, member, attr,
                                               /*passParentContext*/true,
                                               MacroRole::MemberAttribute);
  if (!macroSourceFile)
    return None;

  PrettyStackTraceDecl debugStack(
      "type checking expanded declaration macro", member);

  auto topLevelDecls = macroSourceFile->getTopLevelDecls();
  for (auto decl : topLevelDecls) {
    // Add the new attributes to the semantic attribute list.
    SmallVector<DeclAttribute *, 2> attrs(decl->getAttrs().begin(),
                                          decl->getAttrs().end());
    for (auto *attr : attrs) {
      member->getAttrs().add(attr);
    }
  }

  return macroSourceFile->getBufferID();
}

Optional<unsigned>
swift::expandMembers(CustomAttr *attr, MacroDecl *macro, Decl *decl) {
  // Evaluate the macro.
  auto macroSourceFile = evaluateAttachedMacro(macro, decl, attr,
                                               /*passParentContext*/false,
                                               MacroRole::Member);
  if (!macroSourceFile)
    return None;

  PrettyStackTraceDecl debugStack(
      "type checking expanded declaration macro", decl);

  auto topLevelDecls = macroSourceFile->getTopLevelDecls();
  for (auto member : topLevelDecls) {
    // Note that synthesized members are not considered implicit. They have
    // proper source ranges that should be validated, and ASTScope does not
    // expand implicit scopes to the parent scope tree.

    if (auto *nominal = dyn_cast<NominalTypeDecl>(decl)) {
      nominal->addMember(member);
    } else if (auto *extension = dyn_cast<ExtensionDecl>(decl)) {
      extension->addMember(member);
    }
  }

  return macroSourceFile->getBufferID();
}

Optional<unsigned>
swift::expandPeers(CustomAttr *attr, MacroDecl *macro, Decl *decl) {
  auto macroSourceFile = evaluateAttachedMacro(macro, decl, attr,
                                               /*passParentContext*/false,
                                               MacroRole::Peer);
  if (!macroSourceFile)
    return None;

  PrettyStackTraceDecl debugStack("applying expanded peer macro", decl);

  auto *parent = decl->getDeclContext();
  auto topLevelDecls = macroSourceFile->getTopLevelDecls();
  for (auto peer : topLevelDecls) {
    if (auto *nominal = dyn_cast<NominalTypeDecl>(parent)) {
      nominal->addMember(peer);
    } else if (auto *extension = dyn_cast<ExtensionDecl>(parent)) {
      extension->addMember(peer);
    }
  }

  return macroSourceFile->getBufferID();
}

MacroDecl *
ResolveMacroRequest::evaluate(Evaluator &evaluator,
                              UnresolvedMacroReference macroRef,
                              DeclContext *dc) const {
  auto &ctx = dc->getASTContext();
  auto roles = macroRef.getMacroRoles();
  auto foundMacros = TypeChecker::lookupMacros(
      dc, macroRef.getMacroName(), SourceLoc(), roles);
  if (foundMacros.empty())
    return nullptr;

  // If we already have a MacroExpansionExpr, use that. Otherwise,
  // create one.
  MacroExpansionExpr *macroExpansion;
  if (auto *expr = macroRef.getExpr()) {
    macroExpansion = expr;
  } else {
    SourceRange genericArgsRange = macroRef.getGenericArgsRange();
    macroExpansion = new (ctx) MacroExpansionExpr(
      dc, macroRef.getSigilLoc(), macroRef.getMacroName(),
      macroRef.getMacroNameLoc(), genericArgsRange.Start,
      macroRef.getGenericArgs(), genericArgsRange.End,
      macroRef.getArgs(), roles);
  }

  Expr *result = macroExpansion;
  TypeChecker::typeCheckExpression(result, dc);

  auto macroDeclRef = macroExpansion->getMacroRef();
  if (auto *macroDecl = dyn_cast_or_null<MacroDecl>(macroDeclRef.getDecl()))
    return macroDecl;

  // If we couldn't resolve a macro decl, the attribute is invalid.
  if (auto *attr = macroRef.getAttr())
    attr->setInvalid();

  return nullptr;
}
