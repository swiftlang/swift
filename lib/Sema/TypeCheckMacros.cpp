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
#include "swift/AST/SourceFile.h"
#include "swift/AST/TypeCheckRequests.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Basic/StringExtras.h"
#include "swift/Parse/Lexer.h"
#include "swift/Parse/Parser.h"

using namespace swift;

extern "C" void *swift_ASTGen_lookupMacro(const char *macroName);

extern "C" void swift_ASTGen_destroyMacro(void *macro);

extern "C" void
swift_ASTGen_getMacroEvaluationContext(const void *sourceFile,
                                       void *declContext, void *astContext,
                                       void *macro, void **evaluationContext);

extern "C" ptrdiff_t swift_ASTGen_evaluateMacro(
    void *sourceFile, const void *sourceLocation,
    const char **evaluatedSource, ptrdiff_t *evaluatedSourceLength);

extern "C" void
swift_ASTGen_getMacroTypeSignature(void *macro,
                                   const char **evaluationContextPtr,
                                   ptrdiff_t *evaluationContextLengthPtr);

static NullTerminatedStringRef
getPluginMacroTypeSignature(CompilerPlugin *plugin, ASTContext &ctx) {
  auto genSig = plugin->invokeGenericSignature();
  auto typeSig = plugin->invokeTypeSignature();
  std::string source;
  llvm::raw_string_ostream out(source);
  out << "struct __MacroEvaluationContext" << (genSig ? *genSig : "") << " {\n"
      << "  typealias SignatureType = " << typeSig << "\n"
      << "}";
  auto len = source.length();
  auto *buffer = (char *)malloc(len + 1);
  memcpy(buffer, source.data(), len + 1);
  return {buffer, len};
}

StructDecl *MacroContextRequest::evaluate(Evaluator &evaluator,
                                          std::string macroName,
                                          ModuleDecl *mod) const {
#if SWIFT_SWIFT_PARSER
  auto &ctx = mod->getASTContext();
  auto *builtinMacro = swift_ASTGen_lookupMacro(macroName.c_str());
  NullTerminatedStringRef evaluatedSource;
  if (builtinMacro) {
    const char *evaluatedSourcePtr;
    ptrdiff_t evaluatedSourceLength;
    swift_ASTGen_getMacroTypeSignature(builtinMacro, &evaluatedSourcePtr,
                                       &evaluatedSourceLength);
    evaluatedSource = NullTerminatedStringRef(
        evaluatedSourcePtr, (size_t)evaluatedSourceLength);
  } else if (auto *plugin = ctx.getLoadedPlugin(macroName)) {
    evaluatedSource = getPluginMacroTypeSignature(plugin, ctx);
  } else {
    return nullptr;
  }

  // Create a new source buffer with the contents of the macro's
  // signature.
  SourceManager &sourceMgr = ctx.SourceMgr;
  std::string bufferName;
  {
    llvm::raw_string_ostream out(bufferName);
    out << "Macro signature of #" << macroName;
  }
  auto macroBuffer = llvm::MemoryBuffer::getMemBuffer(
      evaluatedSource, bufferName);
  unsigned macroBufferID =
      sourceMgr.addNewSourceBuffer(std::move(macroBuffer));
  auto macroSourceFile = new (ctx) SourceFile(
      *mod, SourceFileKind::Library, macroBufferID);
  mod->addAuxiliaryFile(*macroSourceFile);

  // Make sure implicit imports are resolved in this file.
  performImportResolution(*macroSourceFile);

  auto *start = sourceMgr.getLocForBufferStart(macroBufferID)
                    .getOpaquePointerValue();
  if (builtinMacro) {
    void *context = nullptr;
    swift_ASTGen_getMacroEvaluationContext(
        (const void *)start, (void *)(DeclContext *)macroSourceFile,
        (void *)&ctx, builtinMacro, &context);
    ctx.addCleanup([builtinMacro]() { swift_ASTGen_destroyMacro(builtinMacro); });
    return dyn_cast<StructDecl>((Decl *)context);
  } else {
    Parser parser(macroBufferID, *macroSourceFile, &ctx.Diags, nullptr,
                  nullptr);
    parser.consumeTokenWithoutFeedingReceiver();
    DeclAttributes attrs;
    auto parsedResult = parser.parseDeclStruct(Parser::PD_Default, attrs);
    if (parsedResult.isParseError() || parsedResult.isNull())
      return nullptr; // TODO: Diagnose this properly.
    return parsedResult.get();
  }
#else
  return nullptr;
#endif // SWIFT_SWIFT_PARSER
}

#if SWIFT_SWIFT_PARSER
Expr *swift::expandMacroExpr(
    DeclContext *dc, Expr *expr, StringRef macroName, Type expandedType
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

  // Built-in macros go through `MacroSystem` in Swift Syntax linked to this
  // compiler.
  if (swift_ASTGen_lookupMacro(macroName.str().c_str())) {
    auto astGenSourceFile = sourceFile->exportedSourceFile;
    if (!astGenSourceFile)
      return nullptr;
  
    const char *evaluatedSourceAddress;
    ptrdiff_t evaluatedSourceLength;
    swift_ASTGen_evaluateMacro(
        astGenSourceFile, expr->getStartLoc().getOpaquePointerValue(),
        &evaluatedSourceAddress, &evaluatedSourceLength);
    if (!evaluatedSourceAddress)
      return nullptr;
    evaluatedSource = NullTerminatedStringRef(evaluatedSourceAddress,
                                              (size_t)evaluatedSourceLength);
  }
  // Other macros go through a compiler plugin.
  else {
    auto mee = cast<MacroExpansionExpr>(expr);
    auto *plugin = ctx.getLoadedPlugin(macroName);
    assert(plugin && "Should have been checked during earlier type checking");
    auto bufferID = sourceFile->getBufferID();
    auto sourceFileText = sourceMgr.getEntireTextForBuffer(*bufferID);
    auto evaluated = plugin->invokeRewrite(
        /*targetModuleName*/ dc->getParentModule()->getName().str(),
        /*filePath*/ sourceFile->getFilename(),
        /*sourceFileText*/ sourceFileText,
        /*range*/ Lexer::getCharSourceRangeFromSourceRange(
            sourceMgr, mee->getSourceRange()),
        ctx);
    if (evaluated)
      evaluatedSource = *evaluated;
    else
      return nullptr;
  }

  // Figure out a reasonable name for the macro expansion buffer.
  std::string bufferName;
  {
    llvm::raw_string_ostream out(bufferName);

    out << "Macro expansion of #" << macroName;
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
