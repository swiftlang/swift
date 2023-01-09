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
#include "swift/Parse/Lexer.h"
#include "swift/Subsystems.h"

using namespace swift;

extern "C" void *swift_ASTGen_resolveMacroType(const void *macroType);

extern "C" void swift_ASTGen_destroyMacro(void *macro);

extern "C" ptrdiff_t swift_ASTGen_evaluateMacro(
    void *diagEngine, void *macro, void *sourceFile,
    const void *sourceLocation,
    const char **evaluatedSource, ptrdiff_t *evaluatedSourceLength);

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
      definition, macro, contextualType);
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

/// Determine whether the given source file is from an expansion of the given
/// macro.
static bool isFromExpansionOfMacro(SourceFile *sourceFile, MacroDecl *macro) {
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

  if (isFromExpansionOfMacro(sourceFile, macro)) {
    ctx.Diags.diagnose(expr->getLoc(), diag::macro_recursive, macro->getName());
    return nullptr;
  }

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

    const char *evaluatedSourceAddress;
    ptrdiff_t evaluatedSourceLength;
    swift_ASTGen_evaluateMacro(
        &ctx.Diags,
        externalDef.opaqueHandle,
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
  {
    llvm::raw_string_ostream out(bufferName);

    out << "macro:" << macro->getName().getBaseName();
    if (auto bufferID = sourceFile->getBufferID()) {
      unsigned startLine, startColumn;
      std::tie(startLine, startColumn) =
          sourceMgr.getLineAndColumnInBuffer(expr->getStartLoc(), *bufferID);

      SourceLoc endLoc =
          Lexer::getLocForEndOfToken(sourceMgr, expr->getEndLoc());
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
    GeneratedSourceInfo::MacroExpansion,
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
