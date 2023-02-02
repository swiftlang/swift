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
    ASTContext &ctx, StringRef moduleName, StringRef typeName) {
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
    accessorAddr = ctx.getAddressOfSymbol(symbolName.c_str());
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

ExternalMacroDefinition
ExternalMacroDefinitionRequest::evaluate(
    Evaluator &evaluator, ASTContext *ctx,
    Identifier moduleName, Identifier typeName
) const {
#if SWIFT_SWIFT_PARSER
  /// Look for the type metadata given the external module and type names.
  auto macroMetatype = lookupMacroTypeMetadataByExternalName(
      *ctx, moduleName.str(), typeName.str());
  if (macroMetatype) {
    // Check whether the macro metatype is in-process.
    if (auto inProcess = swift_ASTGen_resolveMacroType(macroMetatype)) {
      // Make sure we clean up after the macro.
      ctx->addCleanup([inProcess]() {
        swift_ASTGen_destroyMacro(inProcess);
      });

      return ExternalMacroDefinition{inProcess};
    }
  }
#endif

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

bool ExpandMemberAttributeMacros::evaluate(Evaluator &evaluator,
                                           Decl *decl) const {
  auto *parentDecl = decl->getDeclContext()->getAsDecl();
  if (!parentDecl)
    return false;

  bool addedAttributes = false;
  parentDecl->forEachAttachedMacro(MacroRole::MemberAttribute,
      [&](CustomAttr *attr, MacroDecl *macro) {
        addedAttributes |= expandAttributes(attr, macro, decl);
      });

  return addedAttributes;
}

bool ExpandSynthesizedMemberMacroRequest::evaluate(Evaluator &evaluator,
                                                   Decl *decl) const {
  bool synthesizedMembers = false;
  decl->forEachAttachedMacro(MacroRole::Member,
      [&](CustomAttr *attr, MacroDecl *macro) {
        synthesizedMembers |= expandMembers(attr, macro, decl);
      });

  return synthesizedMembers;
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
      if (expansionDecl->getMacro().getFullName() == macro->getName())
        return true;
    } else if (auto *macroAttr = sourceFile->getAttachedMacroAttribute()) {
      auto *decl = expansion.dyn_cast<Decl *>();
      auto &ctx = decl->getASTContext();
      auto *macroDecl = evaluateOrDefault(ctx.evaluator,
          ResolveMacroRequest{macroAttr, role, decl->getDeclContext()},
          nullptr);
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
    expr->getSourceRange(),
    SourceRange(macroBufferRange.getStart(), macroBufferRange.getEnd()),
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
      med->getSourceRange(),
      SourceRange(macroBufferRange.getStart(), macroBufferRange.getEnd()),
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
    TypeChecker::typeCheckDecl(decl);
    results.push_back(decl);
  }
  return true;
}

void swift::expandAccessors(
    AbstractStorageDecl *storage, CustomAttr *attr, MacroDecl *macro
) {
  auto *dc = storage->getInnermostDeclContext();
  ASTContext &ctx = dc->getASTContext();
  SourceManager &sourceMgr = ctx.SourceMgr;

  auto moduleDecl = dc->getParentModule();

  auto attrSourceFile =
    moduleDecl->getSourceFileContainingLocation(attr->AtLoc);
  if (!attrSourceFile)
    return;

  auto declSourceFile =
      moduleDecl->getSourceFileContainingLocation(storage->getStartLoc());
  if (!declSourceFile)
    return;

  // Evaluate the macro.
  NullTerminatedStringRef evaluatedSource;

  if (isFromExpansionOfMacro(attrSourceFile, macro, MacroRole::Accessor) ||
      isFromExpansionOfMacro(declSourceFile, macro, MacroRole::Accessor)) {
    storage->diagnose(diag::macro_recursive, macro->getName());
    return;
  }

  auto macroDef = macro->getDefinition();
  switch (macroDef.kind) {
  case MacroDefinition::Kind::Undefined:
  case MacroDefinition::Kind::Invalid:
    // Already diagnosed as an error elsewhere.
    return;

  case MacroDefinition::Kind::Builtin: {
    switch (macroDef.getBuiltinKind()) {
    case BuiltinMacroKind::ExternalMacro:
      // FIXME: Error here.
      return;
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
      storage->diagnose(diag::external_macro_not_found,
                        external.moduleName.str(),
                        external.macroTypeName.str(),
                        macro->getName()
      );
      macro->diagnose(diag::decl_declared_here, macro->getName());
      return;
    }

    // Make sure macros are enabled before we expand.
    if (!ctx.LangOpts.hasFeature(Feature::Macros)) {
      storage->diagnose(diag::macro_experimental);
      return;
    }

#if SWIFT_SWIFT_PARSER
    PrettyStackTraceDecl debugStack("expanding accessor macro", storage);

    auto astGenAttrSourceFile = attrSourceFile->exportedSourceFile;
    if (!astGenAttrSourceFile)
      return;

    auto astGenDeclSourceFile = declSourceFile->exportedSourceFile;
    if (!astGenDeclSourceFile)
      return;

    Decl *searchDecl = storage;
    if (auto var = dyn_cast<VarDecl>(storage))
      searchDecl = var->getParentPatternBinding();

    const char *evaluatedSourceAddress;
    ptrdiff_t evaluatedSourceLength;
    swift_ASTGen_expandAttachedMacro(
        &ctx.Diags,
        externalDef.opaqueHandle,
        static_cast<uint32_t>(MacroRole::Accessor),
        astGenAttrSourceFile, attr->AtLoc.getOpaquePointerValue(),
        astGenDeclSourceFile, searchDecl->getStartLoc().getOpaquePointerValue(),
        /*parentDeclSourceFile*/nullptr, /*parentDeclLoc*/nullptr,
        &evaluatedSourceAddress, &evaluatedSourceLength);
    if (!evaluatedSourceAddress)
      return;
    evaluatedSource = NullTerminatedStringRef(evaluatedSourceAddress,
                                              (size_t)evaluatedSourceLength);
    break;
#else
    storage->diagnose(diag::macro_unsupported);
    return;
#endif
  }
  }

  // Figure out a reasonable name for the macro expansion buffer.
  std::string bufferName;
  {
    llvm::raw_string_ostream out(bufferName);

    out << "macro:" << storage->getName()
        << "@" << macro->getName().getBaseName();
    if (auto bufferID = declSourceFile->getBufferID()) {
      unsigned startLine, startColumn;
      std::tie(startLine, startColumn) =
          sourceMgr.getLineAndColumnInBuffer(storage->getStartLoc(), *bufferID);

      SourceLoc endLoc =
          Lexer::getLocForEndOfToken(sourceMgr, storage->getEndLoc());
      unsigned endLine, endColumn;
      std::tie(endLine, endColumn) =
          sourceMgr.getLineAndColumnInBuffer(endLoc, *bufferID);

      out << ":" << sourceMgr.getIdentifierForBuffer(*bufferID) << ":"
          << startLine << ":" << startColumn
          << "-" << endLine << ":" << endColumn;
    }
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
      GeneratedSourceInfo::AccessorMacroExpansion,
      storage->getEndLoc(),
      SourceRange(macroBufferRange.getStart(), macroBufferRange.getEnd()),
      ASTNode(storage).getOpaqueValue(),
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
}

// FIXME: Almost entirely duplicated code from `expandAccessors`.
// Factor this out into an `expandAttachedMacro` function, with
// arguments for the PrettyStackTrace string, 'attachedTo' decl, etc.
bool swift::expandAttributes(CustomAttr *attr, MacroDecl *macro, Decl *member) {
  auto *dc = member->getInnermostDeclContext();
  ASTContext &ctx = dc->getASTContext();
  SourceManager &sourceMgr = ctx.SourceMgr;

  auto moduleDecl = dc->getParentModule();

  auto attrSourceFile =
    moduleDecl->getSourceFileContainingLocation(attr->AtLoc);
  if (!attrSourceFile)
    return false;

  auto declSourceFile =
      moduleDecl->getSourceFileContainingLocation(member->getStartLoc());
  if (!declSourceFile)
    return false;

  Decl *parentDecl = member->getDeclContext()->getAsDecl();
  if (!parentDecl)
    return false;

  auto parentDeclSourceFile =
    moduleDecl->getSourceFileContainingLocation(parentDecl->getLoc());
  if (!parentDeclSourceFile)
    return false;

  // Evaluate the macro.
  NullTerminatedStringRef evaluatedSource;

  if (isFromExpansionOfMacro(attrSourceFile, macro, MacroRole::MemberAttribute) ||
      isFromExpansionOfMacro(declSourceFile, macro, MacroRole::MemberAttribute)) {
    member->diagnose(diag::macro_recursive, macro->getName());
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
      member->diagnose(diag::external_macro_not_found,
                        external.moduleName.str(),
                        external.macroTypeName.str(),
                        macro->getName()
      );
      macro->diagnose(diag::decl_declared_here, macro->getName());
      return false;
    }

    // Make sure macros are enabled before we expand.
    if (!ctx.LangOpts.hasFeature(Feature::Macros)) {
      member->diagnose(diag::macro_experimental);
      return false;
    }

#if SWIFT_SWIFT_PARSER
    PrettyStackTraceDecl debugStack("expanding attribute macro", member);

    auto astGenAttrSourceFile = attrSourceFile->exportedSourceFile;
    if (!astGenAttrSourceFile)
      return false;

    auto astGenDeclSourceFile = declSourceFile->exportedSourceFile;
    if (!astGenDeclSourceFile)
      return false;

    auto astGenParentDeclSourceFile = parentDeclSourceFile->exportedSourceFile;
    if (!astGenParentDeclSourceFile)
      return false;

    Decl *searchDecl = member;
    if (auto *var = dyn_cast<VarDecl>(member))
      searchDecl = var->getParentPatternBinding();

    const char *evaluatedSourceAddress;
    ptrdiff_t evaluatedSourceLength;
    swift_ASTGen_expandAttachedMacro(
        &ctx.Diags,
        externalDef.opaqueHandle,
        static_cast<uint32_t>(MacroRole::MemberAttribute),
        astGenAttrSourceFile, attr->AtLoc.getOpaquePointerValue(),
        astGenDeclSourceFile, searchDecl->getStartLoc().getOpaquePointerValue(),
        astGenParentDeclSourceFile, parentDecl->getStartLoc().getOpaquePointerValue(),
        &evaluatedSourceAddress, &evaluatedSourceLength);
    if (!evaluatedSourceAddress)
      return false;
    evaluatedSource = NullTerminatedStringRef(evaluatedSourceAddress,
                                              (size_t)evaluatedSourceLength);
    break;
#else
    member->diagnose(diag::macro_unsupported);
    return false;
#endif
  }
  }

  // Figure out a reasonable name for the macro expansion buffer.
  std::string bufferName;
  {
    llvm::raw_string_ostream out(bufferName);

    out << "macro:" // << member->getDescriptiveK()
        << "@" << macro->getName().getBaseName();
    if (auto bufferID = declSourceFile->getBufferID()) {
      unsigned startLine, startColumn;
      std::tie(startLine, startColumn) =
          sourceMgr.getLineAndColumnInBuffer(member->getStartLoc(), *bufferID);

      SourceLoc endLoc =
          Lexer::getLocForEndOfToken(sourceMgr, member->getEndLoc());
      unsigned endLine, endColumn;
      std::tie(endLine, endColumn) =
          sourceMgr.getLineAndColumnInBuffer(endLoc, *bufferID);

      out << ":" << sourceMgr.getIdentifierForBuffer(*bufferID) << ":"
          << startLine << ":" << startColumn
          << "-" << endLine << ":" << endColumn;
    }
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
      GeneratedSourceInfo::MemberAttributeMacroExpansion,
      member->getEndLoc(),
      SourceRange(macroBufferRange.getStart(), macroBufferRange.getEnd()),
      ASTNode(member).getOpaqueValue(),
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

  PrettyStackTraceDecl debugStack(
      "type checking expanded declaration macro", member);

  bool addedAttributes = false;
  auto topLevelDecls = macroSourceFile->getTopLevelDecls();
  for (auto decl : topLevelDecls) {
    // FIXME: We want to type check decl attributes applied to
    // the real declaration, ideally by appending the new attributes
    // to the result and changing TypeChecker::checkDeclAttributes
    // to use the semantic attribute list.
    decl->setDeclContext(dc);
    TypeChecker::typeCheckDecl(decl);

    // Add the new attributes to the semantic attribute list.
    SmallVector<DeclAttribute *, 2> attrs(decl->getAttrs().begin(),
                                          decl->getAttrs().end());
    for (auto *attr : attrs) {
      addedAttributes = true;
      member->getAttrs().add(attr);
    }
  }

  return addedAttributes;
}

bool swift::expandMembers(CustomAttr *attr, MacroDecl *macro, Decl *decl) {
  auto *dc = decl->getInnermostDeclContext();
  ASTContext &ctx = dc->getASTContext();
  SourceManager &sourceMgr = ctx.SourceMgr;
  auto moduleDecl = dc->getParentModule();

  auto attrSourceFile =
    moduleDecl->getSourceFileContainingLocation(attr->AtLoc);
  if (!attrSourceFile)
    return false;

  auto declSourceFile =
      moduleDecl->getSourceFileContainingLocation(decl->getStartLoc());
  if (!declSourceFile)
    return false;

  // Evaluate the macro.
  NullTerminatedStringRef evaluatedSource;

  if (isFromExpansionOfMacro(attrSourceFile, macro, MacroRole::Member) ||
      isFromExpansionOfMacro(declSourceFile, macro, MacroRole::Member)) {
    decl->diagnose(diag::macro_recursive, macro->getName());
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
      decl->diagnose(diag::external_macro_not_found,
                     external.moduleName.str(),
                     external.macroTypeName.str(),
                     macro->getName()
      );
      macro->diagnose(diag::decl_declared_here, macro->getName());
      return false;
    }

    // Make sure macros are enabled before we expand.
    if (!ctx.LangOpts.hasFeature(Feature::Macros)) {
      decl->diagnose(diag::macro_experimental);
      return false;
    }

#if SWIFT_SWIFT_PARSER
    PrettyStackTraceDecl debugStack("expanding attribute macro", decl);

    auto astGenAttrSourceFile = attrSourceFile->exportedSourceFile;
    if (!astGenAttrSourceFile)
      return false;

    auto astGenDeclSourceFile = declSourceFile->exportedSourceFile;
    if (!astGenDeclSourceFile)
      return false;

    const char *evaluatedSourceAddress;
    ptrdiff_t evaluatedSourceLength;
    swift_ASTGen_expandAttachedMacro(
        &ctx.Diags,
        externalDef.opaqueHandle,
        static_cast<uint32_t>(MacroRole::Member),
        astGenAttrSourceFile, attr->AtLoc.getOpaquePointerValue(),
        astGenDeclSourceFile, decl->getStartLoc().getOpaquePointerValue(),
        /*parentDeclSourceFile*/nullptr, /*parentDeclLoc*/nullptr,
        &evaluatedSourceAddress, &evaluatedSourceLength);
    if (!evaluatedSourceAddress)
      return false;
    evaluatedSource = NullTerminatedStringRef(evaluatedSourceAddress,
                                              (size_t)evaluatedSourceLength);
    break;
#else
    decl->diagnose(diag::macro_unsupported);
    return false;
#endif
  }
  }

  // Figure out a reasonable name for the macro expansion buffer.
  std::string bufferName;
  {
    llvm::raw_string_ostream out(bufferName);

    out << "macro:"
        << "@" << macro->getName().getBaseName();
    if (auto bufferID = declSourceFile->getBufferID()) {
      unsigned startLine, startColumn;
      std::tie(startLine, startColumn) =
          sourceMgr.getLineAndColumnInBuffer(decl->getStartLoc(), *bufferID);

      SourceLoc endLoc =
          Lexer::getLocForEndOfToken(sourceMgr, decl->getEndLoc());
      unsigned endLine, endColumn;
      std::tie(endLine, endColumn) =
          sourceMgr.getLineAndColumnInBuffer(endLoc, *bufferID);

      out << ":" << sourceMgr.getIdentifierForBuffer(*bufferID) << ":"
          << startLine << ":" << startColumn
          << "-" << endLine << ":" << endColumn;
    }
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
      GeneratedSourceInfo::MemberMacroExpansion,
      decl->getEndLoc(),
      SourceRange(macroBufferRange.getStart(), macroBufferRange.getEnd()),
      ASTNode(decl).getOpaqueValue(),
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

  PrettyStackTraceDecl debugStack(
      "type checking expanded declaration macro", decl);

  bool synthesizedMembers = false;
  auto topLevelDecls = macroSourceFile->getTopLevelDecls();
  for (auto member : topLevelDecls) {
    // Note that synthesized members are not considered implicit. They have
    // proper source ranges that should be validated, and ASTScope does not
    // expand implicit scopes to the parent scope tree.
    member->setDeclContext(decl->getInnermostDeclContext());

    if (auto *nominal = dyn_cast<NominalTypeDecl>(decl)) {
      nominal->addMember(member);
    } else if (auto *extension = dyn_cast<ExtensionDecl>(decl)) {
      extension->addMember(member);
    }

    synthesizedMembers = true;
  }

  return synthesizedMembers;
}

MacroDecl *
ResolveMacroRequest::evaluate(Evaluator &evaluator,
                              UnresolvedMacroReference macroRef,
                              MacroRoles roles,
                              DeclContext *dc) const {
  auto &ctx = dc->getASTContext();
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
