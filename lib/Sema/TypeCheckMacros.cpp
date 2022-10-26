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
#include "swift/AST/Expr.h"
#include "swift/AST/SourceFile.h"
#include "swift/Basic/SourceManager.h"
#include "swift/Parse/Lexer.h"
#include "swift/Parse/Parser.h"

using namespace swift;

extern "C" void *swift_ASTGen_lookupMacro(const char *macroName);

extern "C" void swift_ASTGen_destroyMacro(void *macro);

extern "C" void swift_ASTGen_getMacroTypeSignature(
    void *sourceFile, void *declContext, void *astContext, void *macro,
    void **genericSignature,
    void **signature);

extern "C" ptrdiff_t swift_ASTGen_evaluateMacro(
    void *sourceFile, const void *sourceLocation,
    const char **evaluatedSource, ptrdiff_t *evaluatedSourceLength);

#if SWIFT_SWIFT_PARSER

llvm::Optional<ASTGenMacroRAII> ASTGenMacroRAII::lookup(StringRef macroName,
                                              void *sourceFile,
                                              DeclContext *DC,
                                              ASTContext &ctx) {
  auto *macro = swift_ASTGen_lookupMacro(macroName.str().c_str());
  if (!macro)
    return None;

  void *genericParamList = nullptr;
  void *signatureAST = nullptr;
  swift_ASTGen_getMacroTypeSignature(sourceFile, (void *)DC, (void *)&ctx,
                                     macro, &genericParamList,
                                     &signatureAST);

  return ASTGenMacroRAII{macro, (TypeRepr *)signatureAST,
    (GenericParamList *)genericParamList};
}

ASTGenMacroRAII::~ASTGenMacroRAII() { /*swift_ASTGen_destroyMacro(opaqueMacro);*/ }

Expr *swift::expandMacroExpr(
    DeclContext *dc, Expr *expr, StringRef macroName, Type expandedType
) {
  ASTContext &ctx = dc->getASTContext();

  // FIXME: Introduce a more robust way to ensure that we can get the "exported"
  // source file for a given context. If it's within a macro expansion, it
  // may not have a C++ SourceFile, but will have a Syntax tree.
  //
  // FIXME^2: And find a better name for "exportedSourceFile".
  auto sourceFile = dc->getParentSourceFile();
  if (!sourceFile)
    return nullptr;

  auto astGenSourceFile = sourceFile->exportedSourceFile;
  if (!astGenSourceFile)
    return nullptr;

  // Evaluate the macro.
  const char *evaluatedSource;
  ptrdiff_t evaluatedSourceLength;
  swift_ASTGen_evaluateMacro(
      astGenSourceFile, expr->getStartLoc().getOpaquePointerValue(),
      &evaluatedSource, &evaluatedSourceLength);
  if (!evaluatedSource)
    return nullptr;

  SourceManager &sourceMgr = ctx.SourceMgr;

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
      llvm::MemoryBuffer::getMemBuffer(
          StringRef(evaluatedSource, evaluatedSourceLength), bufferName);
  unsigned macroBufferID = sourceMgr.addNewSourceBuffer(std::move(macroBuffer));

  // Create a source file to hold the macro buffer.
  // FIXME: Seems like we should record this somewhere?
  auto macroSourceFile = new (ctx) SourceFile(
      *dc->getParentModule(), SourceFileKind::Library, macroBufferID);

  // Parse the expression.
  Parser parser(macroBufferID, *macroSourceFile, &ctx.Diags, nullptr, nullptr);
  parser.consumeTokenWithoutFeedingReceiver();
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
