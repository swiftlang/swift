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
#include "swift/Parse/Parser.h"
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
    StringRef moduleName, StringRef typeName) {
  using namespace Demangle;

  //  kind=Global
  //    kind=NominalTypeDescriptor
  //      kind=Type
  //        kind=Structure
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
        auto *structNode = D.createNode(Node::Kind::Structure);
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
  // Look up the type metadata accessor.
  auto symbolName = mangledNameForTypeMetadataAccessor(moduleName, typeName);
  auto accessorAddr = ctx.getAddressOfSymbol(symbolName.c_str());
  if (!accessorAddr)
    return nullptr;

  // Call the accessor to form type metadata.
  using MetadataAccessFunc = const void *(MetadataRequest);
  auto accessor = reinterpret_cast<MetadataAccessFunc*>(accessorAddr);
  return accessor(MetadataRequest(MetadataState::Complete));
}

MacroDefinition MacroDefinitionRequest::evaluate(
    Evaluator &evaluator, MacroDecl *macro
) const {
  ASTContext &ctx = macro->getASTContext();

#if SWIFT_SWIFT_PARSER
  /// Look for the type metadata given the external module and type names.
  auto macroMetatype = lookupMacroTypeMetadataByExternalName(
      ctx, macro->externalModuleName.str(),
      macro->externalMacroTypeName.str());
  if (macroMetatype) {
    // Check whether the macro metatype is in-process.
    if (auto inProcess = swift_ASTGen_resolveMacroType(macroMetatype)) {
      // Make sure we clean up after the macro.
      ctx.addCleanup([inProcess]() {
        swift_ASTGen_destroyMacro(inProcess);
      });

      return MacroDefinition::forInProcess(
          MacroDefinition::Expression, inProcess);
    }
  }
#endif

  return MacroDefinition::forMissing(
      ctx, macro->externalModuleName, macro->externalMacroTypeName);
}

#if SWIFT_SWIFT_PARSER
Expr *swift::expandMacroExpr(
    DeclContext *dc, Expr *expr, ConcreteDeclRef macroRef, Type expandedType
) {
  ASTContext &ctx = dc->getASTContext();
  SourceManager &sourceMgr = ctx.SourceMgr;

  // FIXME: Introduce a more robust way to ensure that we can get the "exported"
  // source file for a given context. If it's within a macro expansion, it
  // may not have a C++ SourceFile, but will have a Syntax tree.
  //
  // FIXME^2: And find a better name for "exportedSourceFile".
  auto sourceFile = dc->getParentSourceFile();
  if (!sourceFile)
    return nullptr;

  // Evaluate the macro.
  NullTerminatedStringRef evaluatedSource;

  MacroDecl *macro = cast<MacroDecl>(macroRef.getDecl());

  auto macroDef = evaluateOrDefault(
      ctx.evaluator, MacroDefinitionRequest{macro},
      MacroDefinition::forUndefined());
  switch (macroDef.implKind) {
  case MacroDefinition::ImplementationKind::Undefined:
    // Already diagnosed as an error elsewhere.
    return nullptr;

  case MacroDefinition::ImplementationKind::Missing: {
    auto missingInfo = macroDef.getMissingDefinition();
    ctx.Diags.diagnose(
        expr->getLoc(), diag::external_macro_not_found,
        missingInfo->externalModuleName.str(),
        missingInfo->externalMacroTypeName.str(), macro->getName());
    macro->diagnose(diag::decl_declared_here, macro->getName());
    return nullptr;
  }

  case MacroDefinition::ImplementationKind::InProcess: {
    PrettyStackTraceExpr debugStack(ctx, "expanding macro", expr);

    // Builtin macros are handled via ASTGen.
    auto astGenSourceFile = sourceFile->exportedSourceFile;
    if (!astGenSourceFile)
      return nullptr;

    const char *evaluatedSourceAddress;
    ptrdiff_t evaluatedSourceLength;
    swift_ASTGen_evaluateMacro(
        &ctx.Diags,
        macroDef.getInProcessOpaqueHandle(),
        astGenSourceFile, expr->getStartLoc().getOpaquePointerValue(),
        &evaluatedSourceAddress, &evaluatedSourceLength);
    if (!evaluatedSourceAddress)
      return nullptr;
    evaluatedSource = NullTerminatedStringRef(evaluatedSourceAddress,
                                              (size_t)evaluatedSourceLength);
    break;
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
    *sourceFile->getBufferID(),
    expr->getSourceRange(),
    SourceRange(macroBufferRange.getStart(), macroBufferRange.getEnd()),
    ASTNode(expr).getOpaqueValue()
  };
  sourceMgr.setGeneratedSourceInfo(macroBufferID, sourceInfo);
  free((void*)evaluatedSource.data());

  // Create a source file to hold the macro buffer. This is automatically
  // registered with the enclosing module.
  auto macroSourceFile = new (ctx) SourceFile(
      *dc->getParentModule(), SourceFileKind::MacroExpansion, macroBufferID,
      /*parsingOpts=*/{}, /*isPrimary=*/false);

  // Parse the expression.
  Parser parser(macroBufferID, *macroSourceFile, &ctx.Diags, nullptr, nullptr);
  parser.consumeTokenWithoutFeedingReceiver();

  // Set up a "local context" for parsing, so that we have a source of
  // closure and local-variable discriminators.
  LocalContext tempContext{};
  parser.CurDeclContext = dc;
  parser.CurLocalContext = &tempContext;
  {
    DiscriminatorFinder finder;
    expr->walk(finder);

    unsigned closureDiscriminator;
    if (finder.getFirstDiscriminator() ==
          AbstractClosureExpr::InvalidDiscriminator)
      closureDiscriminator = 0;
    else
      closureDiscriminator = finder.getFirstDiscriminator() + 1;

    tempContext.overrideNextClosureDiscriminator(closureDiscriminator);
  }

  auto parsedResult = parser.parseExpr(diag::expected_macro_expansion_expr);
  if (parsedResult.isParseError() || parsedResult.isNull()) {
    // Tack on a note to say where we expanded the macro from?
    return nullptr;
  }

  // Type-check the expanded expression.
  // FIXME: Would like to pass through type checking options like "discarded"
  // that are captured by TypeCheckExprOptions.
  Expr *expandedExpr = parsedResult.get();
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

#endif // SWIFT_SWIFT_PARSER
