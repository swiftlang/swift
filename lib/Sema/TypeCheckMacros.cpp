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
#include "swift/AST/ASTContext.h"
#include "swift/AST/CompilerPlugin.h"
#include "swift/AST/Expr.h"
#include "swift/AST/NameLookupRequests.h"
#include "swift/AST/MacroDefinition.h"
#include "swift/AST/PrettyStackTrace.h"
#include "swift/AST/SourceFile.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Basic/StringExtras.h"
#include "swift/Parse/Lexer.h"
#include "swift/Parse/Parser.h"
#include "swift/Subsystems.h"

using namespace swift;

extern "C" void *swift_ASTGen_lookupMacro(const char *macroName);

extern "C" void swift_ASTGen_destroyMacro(void *macro);

extern "C" ptrdiff_t swift_ASTGen_evaluateMacro(
    void *sourceFile, const void *sourceLocation,
    const char **evaluatedSource, ptrdiff_t *evaluatedSourceLength);

MacroDefinition MacroDefinitionRequest::evaluate(
    Evaluator &evaluator, MacroDecl *macro
) const {
#if SWIFT_SWIFT_PARSER
  ASTContext &ctx = macro->getASTContext();

  // FIXME: We need to perform lookups by resolving the external module name.

  StringRef macroName = macro->getName().getBaseName().userFacingName();
  // Look for a builtin macro with this name.
  if (auto *builtinHandle = swift_ASTGen_lookupMacro(
          macroName.str().c_str())) {
    // Make sure we clean up after the macro.
    ctx.addCleanup([builtinHandle]() {
      swift_ASTGen_destroyMacro(builtinHandle);
    });

    return MacroDefinition::forBuiltin(
        MacroDefinition::Expression, builtinHandle);
  }

  // Look for a plugin macro with this name.
  // NOTE: We really need to index based on the type name, which should be
  // unique.
  for (auto plugin : ctx.getLoadedPlugins(macroName)) {
    // FIXME: Handle other kinds of macros.
    return MacroDefinition::forCompilerPlugin(
        MacroDefinition::Expression, plugin);
  }
#endif

  // FIXME: Diagnose the lack of a macro implementation. This macro cannot be
  // expanded.
  return MacroDefinition::forInvalid();
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
      MacroDefinition::forInvalid());
  if (!macroDef) {
    return nullptr;
  }

  {
    PrettyStackTraceExpr debugStack(ctx, "expanding macro", expr);

    switch (macroDef.implKind) {
    case MacroDefinition::ImplementationKind::Builtin: {
      // Builtin macros are handled via ASTGen.
      auto astGenSourceFile = sourceFile->exportedSourceFile;
      if (!astGenSourceFile)
        return nullptr;

      // FIXME: Tell ASTGen which macro to use.
      const char *evaluatedSourceAddress;
      ptrdiff_t evaluatedSourceLength;
      swift_ASTGen_evaluateMacro(
          astGenSourceFile, expr->getStartLoc().getOpaquePointerValue(),
          &evaluatedSourceAddress, &evaluatedSourceLength);
      if (!evaluatedSourceAddress)
        return nullptr;
      evaluatedSource = NullTerminatedStringRef(evaluatedSourceAddress,
                                                (size_t)evaluatedSourceLength);
      break;
    }

    case MacroDefinition::ImplementationKind::Plugin: {
      auto *plugin = macroDef.getAsCompilerPlugin();
      auto bufferID = sourceFile->getBufferID();
      auto sourceFileText = sourceMgr.getEntireTextForBuffer(*bufferID);
      SmallVector<CompilerPlugin::Diagnostic, 8> pluginDiags;
      SWIFT_DEFER {
        for (auto &diag : pluginDiags)
          free((void*)diag.message.data());
      };
      auto evaluated = plugin->invokeRewrite(
         /*targetModuleName*/ dc->getParentModule()->getName().str(),
         /*filePath*/ sourceFile->getFilename(),
         /*sourceFileText*/ sourceFileText,
         /*range*/ Lexer::getCharSourceRangeFromSourceRange(
             sourceMgr, expr->getSourceRange()), ctx, pluginDiags);
      for (auto &diag : pluginDiags) {
        auto loc = sourceMgr.getLocForOffset(*bufferID, diag.position);
        switch (diag.severity) {
        case CompilerPlugin::DiagnosticSeverity::Note:
          ctx.Diags.diagnose(loc, diag::macro_note,
              ctx.getIdentifier(plugin->getName()),
              diag.message);
          break;
        case CompilerPlugin::DiagnosticSeverity::Warning:
          ctx.Diags.diagnose(loc, diag::macro_warning,
              ctx.getIdentifier(plugin->getName()),
              diag.message);
          break;
        case CompilerPlugin::DiagnosticSeverity::Error:
          ctx.Diags.diagnose(loc, diag::macro_error,
              ctx.getIdentifier(plugin->getName()),
              diag.message);
          break;
        }
      }
      if (evaluated)
        evaluatedSource = *evaluated;
      else
        return nullptr;
      break;
    }
    }
  }

  // Figure out a reasonable name for the macro expansion buffer.
  std::string bufferName;
  {
    llvm::raw_string_ostream out(bufferName);

    out << "Macro expansion of #" << macro->getName();
    if (auto bufferID = sourceFile->getBufferID()) {
      unsigned startLine, startColumn;
      std::tie(startLine, startColumn) =
          sourceMgr.getLineAndColumnInBuffer(expr->getStartLoc(), *bufferID);

      SourceLoc endLoc =
          Lexer::getLocForEndOfToken(sourceMgr, expr->getEndLoc());
      unsigned endLine, endColumn;
      std::tie(endLine, endColumn) =
          sourceMgr.getLineAndColumnInBuffer(endLoc, *bufferID);

      out << " in " << sourceMgr.getIdentifierForBuffer(*bufferID) << ":"
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
      llvm::MemoryBuffer::getMemBuffer(evaluatedSource, bufferName);
  unsigned macroBufferID = sourceMgr.addNewSourceBuffer(std::move(macroBuffer));

  // Create a source file to hold the macro buffer. This is automatically
  // registered with the enclosing module.
  auto macroSourceFile = new (ctx) SourceFile(
      *dc->getParentModule(), SourceFileKind::MacroExpansion, macroBufferID,
      /*parsingOpts=*/{}, /*isPrimary=*/false, expr);

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
